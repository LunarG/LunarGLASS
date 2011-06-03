//===- CanonicalizeInsts.cpp - Canonicalize instructions for LunarGLASS ---===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (C) 2010-2011 LunarG, Inc.
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; version 2 of the
// License.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301, USA.
//
//===----------------------------------------------------------------------===//
//
// Author: Michael Ilseman, LunarG
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

#include "llvm/GlobalVariable.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Use.h"
#include "llvm/User.h"

#include "Passes/PassSupport.h"
#include "Passes/Immutable/BackEndPointer.h"

// LunarGLASS helpers
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

        void hoistOperands(BasicBlock*);

        void hoistConstantGEPs(Instruction*);

        void hoistUndefOps(Instruction*);

        CanonicalizeInsts(const CanonicalizeInsts&); // do not implement
        void operator=(const CanonicalizeInsts&); // do not implement

        BackEnd* backEnd;
        bool changed;
    };
} // end namespace

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
                std::vector<llvm::Value*> gepIndices;
                ConstantExpr::op_iterator expIter = constExpr->op_begin(), end = constExpr->op_end();
                // Skip the first GEP index

                for (++expIter; expIter != end; ++expIter)
                    gepIndices.push_back(*expIter);

                // Insert new instruction and replace operand
                *constIter = GetElementPtrInst::Create(constExpr->getOperand(0), gepIndices.begin(), gepIndices.end(), "gla_constGEP", insertLoc);
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

    Instruction* insertLoc = inst;

    for (User::op_iterator aggIter = inst->op_begin(), end = inst->op_end();  aggIter != end; ++aggIter) {

        if (gla::IsAggregate(*aggIter) && !gla::AreAllDefined(*aggIter)) {

            if (PHINode* phi = dyn_cast<PHINode>(inst))
                insertLoc = phi->getIncomingBlock(*aggIter)->getTerminator();

            // Create a global var representing the aggregate
            Constant* agg = dyn_cast<Constant>(*aggIter);
            GlobalVariable* var = new GlobalVariable(agg->getType(), false, GlobalVariable::InternalLinkage, agg, "gla_globalAgg");
            inst->getParent()->getParent()->getParent()->getGlobalList().push_back(var);

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

    // Start off having not changed anything, our methods will set this to be
    // true if they perform any changes
    changed = false;

    for (Function::iterator bbI = F.begin(), bbE = F.end(); bbI != bbE; ++bbI) {
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

