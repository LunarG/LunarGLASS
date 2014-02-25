//===- CanonicalizeInsts.cpp - Canonicalize instructions for LunarGLASS ---===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (C) 2010-2014 LunarG, Inc.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 
//     Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.
// 
//     Redistributions in binary form must reproduce the above
//     copyright notice, this list of conditions and the following
//     disclaimer in the documentation and/or other materials provided
//     with the distribution.
// 
//     Neither the name of LunarG Inc. nor the names of its
//     contributors may be used to endorse or promote products derived
//     from this software without specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
// LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
// ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
//===----------------------------------------------------------------------===//
//
// Author: Michael Ilseman, LunarG
// Author: John Kessenich, LunarG
//
//===----------------------------------------------------------------------===//
//
// Canonicalize instructions for LunarGLASS, this includes the following:
//
//   * fcmp ord %foo <some-constant> --> fcmp oeq %foo %foo
//   * operand hoisting: constant expressions and partial or
//                       undefined aggregates moved to separate inst
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Use.h"
#include "llvm/IR/User.h"
#include "llvm/Pass.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "Passes/PassSupport.h"
#include "Passes/Immutable/BackEndPointer.h"
#include "Passes/Util/InstructionUtil.h"
#include "Passes/Util/ConstantUtil.h"

// LunarGLASS helpers
#include "Exceptions.h"
#include "TopBuilder.h"
#include "Util.h"

using namespace llvm;
using namespace gla_llvm;

namespace  {
    class CanonicalizeInsts : public FunctionPass {
    public:
        static char ID;

        CanonicalizeInsts() : FunctionPass(ID)
        {
            initializeCanonicalizeInstsPass(*PassRegistry::getPassRegistry());
        }

        virtual bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;

    private:
        void decomposeOrd(BasicBlock*);

        void intrinsicSelection(BasicBlock*);

        void hoistOperands(BasicBlock*);

        void hoistConstantGEPs(Instruction*);

        void hoistUndefOps(Instruction*);

        CallInst* createSwizzleIntrinsic(Value* val, ArrayRef<Constant*> mask);

        CanonicalizeInsts(const CanonicalizeInsts&); // do not implement
        void operator=(const CanonicalizeInsts&); // do not implement

        Module* module;

        BackEnd* backEnd;
        bool changed;
    };
} // end namespace

inline bool NeedsToKeepUndefs(const Instruction* inst)
{
    return isa<ShuffleVectorInst>(inst) || IsGlaSwizzle(inst) || IsMultiInsert(inst);
}

CallInst* CanonicalizeInsts::createSwizzleIntrinsic(Value* val, ArrayRef<Constant*> mask)
{
    Instruction* inst = dyn_cast<Instruction>(val);
    if (! inst)
        gla::UnsupportedFunctionality("Creating a swizzle out of a constant (should fold instead)");

    Intrinsic::ID id = GetBasicType(inst)->isFloatTy() ? Intrinsic::gla_fSwizzle
                                                       : Intrinsic::gla_swizzle;

    Type* retTy = VectorType::get(GetBasicType(inst), mask.size());

    Constant* maskArg = ConstantVector::get(mask);

    Type* tys[] = { retTy, inst->getType(), maskArg->getType() };

    // Make a builder ready to insert right after the value, or after all the
    // PHINodes if the value is the result of a PHI.
    Instruction* insertPoint = isa<PHINode>(inst) ? inst->getParent()->getFirstNonPHI() : inst->getNextNode();

    IRBuilder<> builder(module->getContext());
    builder.SetInsertPoint(insertPoint);

    Function* sig = Intrinsic::getDeclaration(module, id, tys);
    return builder.CreateCall2(sig, inst, maskArg);
}

void CanonicalizeInsts::intrinsicSelection(BasicBlock* bb)
{
    SmallVector<Instruction*, 16> deadList;

    for (BasicBlock::iterator instI = bb->begin(), instE = bb->end(); instI != instE; ++instI) {
        if (! RepresentsSwizzle(instI) || ! backEnd->decomposeIntrinsic(EDiPreferSwizzle))
            continue;

        SmallVector<Constant*, 4> elts;
        llvm::Value* source;

        // Get the elements from the mask into maskElts
        if (ShuffleVectorInst* svInst = dyn_cast<ShuffleVectorInst>(instI)) {
            // Find the defined op
            source = svInst->getOperand(IsDefined(instI->getOperand(0)) ? 0 : 1);
            assert(IsDefined(source));

            Constant* mask = dyn_cast<Constant>(instI->getOperand(2));
            assert(mask);
            GetElements(mask, elts);
        } else {
            assert(IsMultiInsert(instI));
            source = GetMultiInsertUniqueSource(instI);
            assert(source);

            GetMultiInsertSelects(instI, elts);
        }

        CallInst* newIntr = createSwizzleIntrinsic(source, elts);

        instI->replaceAllUsesWith(newIntr);
        instI->dropAllReferences();
        deadList.push_back(instI);
    }

    for (SmallVector<Instruction*, 16>::iterator i = deadList.begin(), e = deadList.end(); i != e; ++i) {
        (*i)->eraseFromParent();
    }
}

void CanonicalizeInsts::decomposeOrd(BasicBlock* bb)
{
    if (! backEnd->decomposeNaNCompares())
        return;

    for (BasicBlock::iterator instI = bb->begin(), instE = bb->end(); instI != instE; /* empty */) {
        Instruction* inst = instI;
        ++instI;

        FCmpInst* cmp = dyn_cast<FCmpInst>(inst);
        if (cmp && cmp->getPredicate() == CmpInst::FCMP_ORD) {
            // See if/which one of the aguments is constant
            bool isLeftConstant  = isa<Constant>(cmp->getOperand(0));
            bool isRightConstant = isa<Constant>(cmp->getOperand(1));

            if (isLeftConstant ^ isRightConstant) {
                Value* arg = cmp->getOperand(isLeftConstant ? 1 : 0);
                FCmpInst* newCmp = new FCmpInst(CmpInst::FCMP_OEQ, arg, arg, "ord");
                ReplaceInstWithInst(cmp, newCmp);
                changed = true;
            }
        }
    }
}

void CanonicalizeInsts::hoistConstantGEPs(Instruction* inst)
{
    if (! backEnd->hoistGEPConstantOperands())
        return;

    Instruction* insertLoc = inst;

    for (User::op_iterator constIter = inst->op_begin(), end = inst->op_end();  constIter != end; ++constIter) {

        if (ConstantExpr* constExpr = dyn_cast<ConstantExpr>(*constIter)) {

            if (constExpr->isGEPWithNoNotionalOverIndexing()) {

                if (PHINode* phi = dyn_cast<PHINode>(inst))
                    insertLoc = phi->getIncomingBlock(*constIter)->getTerminator();

                // Convert the ConstantExpr to a GetElementPtrInst
                std::vector<Value*> gepIndices;
                ConstantExpr::op_iterator expIter = constExpr->op_begin(), end = constExpr->op_end();
                // Skip the first GEP index

                for (++expIter; expIter != end; ++expIter)
                    gepIndices.push_back(*expIter);

                // Insert new instruction and replace operand
                *constIter = GetElementPtrInst::Create(constExpr->getOperand(0), gepIndices, "gla_constGEP", insertLoc);
                changed = true;

            } else {
                // TODO: add support for more constant expressions
                // (e.g. involving undef)

                // assert(0 && "Non-GEP constant expression");
            }
        }
    }
}

void CanonicalizeInsts::hoistUndefOps(Instruction* inst)
{
    if (! backEnd->hoistUndefOperands())
        return;

    // Don't do it for instructions/intrinsics which require a constant as a
    // mask and will be interpreted specially by a backend.
    if (NeedsToKeepUndefs(inst)) {
        return;
    }

    Instruction* insertLoc = inst;

    for (User::op_iterator aggIter = inst->op_begin(), end = inst->op_end();  aggIter != end; ++aggIter) {
        Constant* agg = dyn_cast<Constant>(*aggIter);

        if (agg && ! gla::IsScalar(*aggIter) && ! gla::AreAllDefined(*aggIter)) {

            if (PHINode* phi = dyn_cast<PHINode>(inst))
                insertLoc = phi->getIncomingBlock(*aggIter)->getTerminator();

            // Create a global var representing the aggregate
            GlobalVariable* var = new GlobalVariable(agg->getType(), false, GlobalVariable::InternalLinkage, agg, "gla_globalAgg");
            module->getGlobalList().push_back(var);

            // Insert new instruction and replace operand
            *aggIter = new LoadInst(var, "aggregate", insertLoc);
            changed = true;
        }
    }
}

void CanonicalizeInsts::hoistOperands(BasicBlock* bb)
{
    for (BasicBlock::iterator instI = bb->begin(), instE = bb->end(); instI != instE; ++instI) {
        Instruction* inst = instI;

        // Find operands that are GEP constant expressions and hoist them into a
        // new instruction
        hoistConstantGEPs(inst);

        // Find operands that are undefined, or partially defined, and hoist
        // them to globals
        hoistUndefOps(inst);

    }
}

bool CanonicalizeInsts::runOnFunction(Function& F)
{
    BackEndPointer* bep = getAnalysisIfAvailable<BackEndPointer>();
    if (! bep) {
        return false;
    }
    backEnd = *bep;

    module = F.getParent();

    // Start off having not changed anything, our methods will set this to be
    // true if they perform any changes
    changed = false;

    for (Function::iterator bbI = F.begin(), bbE = F.end(); bbI != bbE; ++bbI) {

        // Select certain intrinsics over others (e.g. Swizzle instead of
        // MultiInsert/ShuffleVector if possible)
        intrinsicSelection(bbI);

        // fcmp ord %foo <some-constant> --> fcmp oeq %foo %foo
        // TODO: explore: fcmp ord %foo %bar --> fcmp oeq %foo %foo ; fcmp oeq %bar %bar
        decomposeOrd(bbI);

        // hoist constant GEPs and undefined operands.
        hoistOperands(bbI);
    }

    return changed;
}

void CanonicalizeInsts::getAnalysisUsage(AnalysisUsage& AU) const
{
    return;
}

void CanonicalizeInsts::print(raw_ostream&, const Module*) const
{
    return;
}


char CanonicalizeInsts::ID = 0;
INITIALIZE_PASS_BEGIN(CanonicalizeInsts,
                      "canonicalize-insts",
                      "Canonicalize instructions for LunarGLASS",
                      false,  // Whether it looks only at CFG
                      false); // Whether it is an analysis pass
INITIALIZE_PASS_END(CanonicalizeInsts,
                    "canonicalize-insts",
                    "Canonicalize instructions for LunarGLASS",
                    false,  // Whether it looks only at CFG
                    false); // Whether it is an analysis pass


FunctionPass* gla_llvm::createCanonicalizeInstsPass()
{
    return new CanonicalizeInsts();
}
