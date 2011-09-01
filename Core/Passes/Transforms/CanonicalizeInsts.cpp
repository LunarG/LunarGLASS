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
#include "Exceptions.h"

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
        void decomposeIntrinsics(BasicBlock*);

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

void CanonicalizeInsts::decomposeIntrinsics(BasicBlock* bb)
{
    for (BasicBlock::iterator instI = bb->begin(), instE = bb->end(); instI != instE; /* empty */) {
        Instruction* inst = instI;
        ++instI;

        IntrinsicInst* intrinsic = dyn_cast<IntrinsicInst>(inst);
        if (intrinsic) {
            switch (intrinsic->getIntrinsicID()) {
                case llvm::Intrinsic::gla_fInverseSqrt:
                    if (backEnd->decomposeIntrinsic(EDiInverseSqrt)) {
                        UnsupportedFunctionality("decomposition of gla_fInverseSqrt");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fFraction:
                    if (backEnd->decomposeIntrinsic(EDiFraction)) {
                        UnsupportedFunctionality("decomposition of gla_fFraction");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fModF:
                    if (backEnd->decomposeIntrinsic(EDiModF)) {
                        UnsupportedFunctionality("decomposition of gla_fModF");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fMin:
                    if (backEnd->decomposeIntrinsic(EDiMin)) {
                        UnsupportedFunctionality("decomposition of gla_fMin");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fMax:
                    if (backEnd->decomposeIntrinsic(EDiMax)) {
                        UnsupportedFunctionality("decomposition of gla_fMax");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fClamp:
                    if (backEnd->decomposeIntrinsic(EDiClamp)) {
                        UnsupportedFunctionality("decomposition of gla_fClamp");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fMix:
                    if (backEnd->decomposeIntrinsic(EDiMix)) {
                        UnsupportedFunctionality("decomposition of gla_fMix");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fStep:
                    if (backEnd->decomposeIntrinsic(EDiStep)) {
                        UnsupportedFunctionality("decomposition of gla_fStep");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fSmoothStep:
                    if (backEnd->decomposeIntrinsic(EDiSmoothStep)) {
                        UnsupportedFunctionality("decomposition of gla_fSmoothStep");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fIsNan:
                    if (backEnd->decomposeIntrinsic(EDiIsNan)) {
                        UnsupportedFunctionality("decomposition of gla_fIsNan");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fFma:
                    if (backEnd->decomposeIntrinsic(EDiFma)) {
                        UnsupportedFunctionality("decomposition of gla_Fma");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fPackUnorm2x16:
                    if (backEnd->decomposeIntrinsic(EDiPackUnorm2x16)) {
                        UnsupportedFunctionality("decomposition of gla_fPackUnorm2x16");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fPackUnorm4x8:
                    if (backEnd->decomposeIntrinsic(EDiPackUnorm4x8)) {
                        UnsupportedFunctionality("decomposition of gla_fPackUnorm4x8");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fPackSnorm4x8:
                    if (backEnd->decomposeIntrinsic(EDiPackSnorm4x8)) {
                        UnsupportedFunctionality("decomposition of gla_fPackSnorm4x8");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fUnpackUnorm2x16:
                    if (backEnd->decomposeIntrinsic(EDiUnpackUnorm2x16)) {
                        UnsupportedFunctionality("decomposition of gla_fUnpackUnorm2x16");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fUnpackUnorm4x8:
                    if (backEnd->decomposeIntrinsic(EDiUnpackUnorm4x8)) {
                        UnsupportedFunctionality("decomposition of gla_fUnpackUnorm4x8");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fUnpackSnorm4x8:
                    if (backEnd->decomposeIntrinsic(EDiUnpackSnorm4x8)) {
                        UnsupportedFunctionality("decomposition of gla_fUnpackSnorm4x8");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fPackDouble2x32:
                    if (backEnd->decomposeIntrinsic(EDiPackDouble2x32)) {
                        UnsupportedFunctionality("decomposition of gla_fPackDouble2x32");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fUnpackDouble2x32:
                    if (backEnd->decomposeIntrinsic(EDiUnpackDouble2x32)) {
                        UnsupportedFunctionality("decomposition of gla_fUnpackDouble2x32");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fLength:
                    if (backEnd->decomposeIntrinsic(EDiLength)) {
                        UnsupportedFunctionality("decomposition of gla_fLength");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fDistance:
                    if (backEnd->decomposeIntrinsic(EDiDistance)) {
                        UnsupportedFunctionality("decomposition of gla_fDistance");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fDot:
                    if (backEnd->decomposeIntrinsic(EDiDot)) {
                        UnsupportedFunctionality("decomposition of gla_fDot");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fCross:
                    if (backEnd->decomposeIntrinsic(EDiCross)) {
                        UnsupportedFunctionality("decomposition of gla_fCross");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fNormalize:
                    if (backEnd->decomposeIntrinsic(EDiNormalize)) {
                        UnsupportedFunctionality("decomposition of gla_fNormalize");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fNormalize3D:
                    if (backEnd->decomposeIntrinsic(EDiNormalize3D)) {
                        UnsupportedFunctionality("decomposition of gla_fNormalize3D");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fFixedTransform:
                    if (backEnd->decomposeIntrinsic(EDiFixedTransform)) {
                        UnsupportedFunctionality("decomposition of gla_fFixedTransform");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fFaceForward:
                    if (backEnd->decomposeIntrinsic(EDiFaceForward)) {
                        UnsupportedFunctionality("decomposition of gla_fFaceForward");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fReflect:
                    if (backEnd->decomposeIntrinsic(EDiReflect)) {
                        UnsupportedFunctionality("decomposition of gla_fReflect");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fRefract:
                    if (backEnd->decomposeIntrinsic(EDiRefract)) {
                        UnsupportedFunctionality("decomposition of gla_fRefract");
                        //changed = true;
                    }
                    break;
                case llvm::Intrinsic::gla_fFilterWidth:
                    if (backEnd->decomposeIntrinsic(EDiFilterWidth)) {
                        UnsupportedFunctionality("decomposition of gla_fFilterWidth");
                        //changed = true;
                    }
                    break;
                default:
                    // The cases above needs to be comprehensive in terms of
                    // checking for what intrinsics to decompose.  If not there
                    // the assumption is it never needs to be decomposed.
                    ;
            }
        }
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

    // Don't do it for shufflevector, which requires a constant vector as a mask
    // and will be interpreted specially by a backend and
    if (inst->getOpcode() == llvm::Instruction::ShuffleVector) {
        return;
    }

    Instruction* insertLoc = inst;

    for (User::op_iterator aggIter = inst->op_begin(), end = inst->op_end();  aggIter != end; ++aggIter) {
        Constant* agg = dyn_cast<Constant>(*aggIter);

        if (agg && gla::IsAggregate(*aggIter) && !gla::AreAllDefined(*aggIter)) {

            if (PHINode* phi = dyn_cast<PHINode>(inst))
                insertLoc = phi->getIncomingBlock(*aggIter)->getTerminator();

            // Create a global var representing the aggregate
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
        // decompose intrinsics first, so their expansions are seen by the
        // other transforms
        decomposeIntrinsics(bbI);

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

