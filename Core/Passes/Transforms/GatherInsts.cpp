//===- GatherInsts.cpp - Gather multiple instructions into intrinsics -----===//
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
// Gather llvm instructions and LunarGLASS intrinsics into a single LunarGLASS
// intrinsic when able.
//
//   * trunc(mul(ext(x), ext(y))); trunc(mul(ext(x), ext(y)) >> 32)
//                          ==> mulExtended(x,y)
//
//   * div(x, y); rem(x, y) ==> ??? new div/rem combination ???
//
//   * fMax(fMin(0, x), 1)  ==> fSaturate(x)
//
//   * fMax(fMin(x, y), z)  ==> fClamp(x, y, z)
//
//
//
//===----------------------------------------------------------------------===//

#include "llvm/Pass.h"

#include "Passes/PassSupport.h"
#include "Passes/Immutable/BackEndPointer.h"
#include "Passes/Util/ConstantUtil.h"

// LunarGLASS helpers
#include "Exceptions.h"
#include "LunarGLASSTopIR.h"
#include "Util.h"

using namespace llvm;
using namespace gla_llvm;

namespace  {
    class GatherInsts : public FunctionPass {
    public:
        static char ID;

        GatherInsts() : FunctionPass(ID)
        {
            initializeGatherInstsPass(*PassRegistry::getPassRegistry());
        }

        virtual bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;

    private:
        // Delete the instructions in the deadList
        void dce();

        // Optimize min(max(a,b),c) into saturate or clamp
        bool visitMinMaxPair(IntrinsicInst* fMin, IntrinsicInst* fMax);


        bool visit(Instruction* inst);
        bool visitIntrinsic(IntrinsicInst* intr);

        // When combining/optimizing intrinsics, use the deadList to keep around
        // instructions to remove from the function. This allows iterators to be
        // preserved when iterating over instructions.
        std::vector<Instruction*> deadList;

        Module* module;
        LLVMContext* context;

        BackEnd* backEnd;
        IRBuilder<>* builder;

        GatherInsts(const GatherInsts&); // do not implement
        void operator=(const GatherInsts&); // do not implement
    };
} // end namespace

void GatherInsts::dce()
{
    for (std::vector<Instruction*>::iterator i = deadList.begin(), e = deadList.end(); i != e; ++i) {
        (*i)->dropAllReferences();
        (*i)->eraseFromParent();
    }
}

bool GatherInsts::runOnFunction(Function& F)
{
    releaseMemory();

    BackEndPointer* bep = getAnalysisIfAvailable<BackEndPointer>();
    if (! bep)
        return false;

    backEnd = *bep;

    module  = F.getParent();
    context = &F.getContext();
    builder = new IRBuilder<>(*context);

    bool changed = false;

    // Visit each instruction, trying to optimize
    for (Function::iterator bbI = F.begin(), bbE = F.end(); bbI != bbE; ++bbI) {
        for (BasicBlock::iterator instI = bbI->begin(), instE = bbI->end(); instI != instE; /* empty */) {
            BasicBlock::iterator prev = instI;
            ++instI;
            changed |= visit(prev);
        }
    }

    dce();

    delete builder;
    builder = 0;

    return changed;
}


bool GatherInsts::visitMinMaxPair(IntrinsicInst* fMin, IntrinsicInst* fMax)
{
    assert(fMin->getIntrinsicID() == Intrinsic::gla_fMin && fMax->getIntrinsicID() == Intrinsic::gla_fMax);
    assert(fMax->hasOneUse() && fMax->use_back() == fMin); //TODO: max(a, min(b,c))

    builder->SetInsertPoint(fMin);

    // Try to get constant operands for min and max, to see if we can make a
    // saturate
    Constant* minC0 = dyn_cast<Constant>(fMin->getArgOperand(0));
    Constant* minC1 = dyn_cast<Constant>(fMin->getArgOperand(1));
    if (minC0 && minC1) {
        gla::UnsupportedFunctionality("fMin should get constant-folded", EATContinue);
    }

    Constant* maxC0 = dyn_cast<Constant>(fMax->getArgOperand(0));
    Constant* maxC1 = dyn_cast<Constant>(fMax->getArgOperand(1));
    if (maxC0 && maxC1) {
        gla::UnsupportedFunctionality("fMax should get constant-folded", EATContinue);
    }

    // See if we can form a saturate instead of a clamp
    if ((minC0 || minC1) && (maxC0 || maxC1)) {
        // The value that may be saturated
        Value* saturateCandidate = 0;

        if (maxC0 && (maxC0->isNullValue() || maxC0->isNegativeZeroValue())) {
            saturateCandidate = fMax->getArgOperand(1);
        } else if (maxC1 && (maxC1->isNullValue() || maxC1->isNegativeZeroValue())) {
            saturateCandidate = fMax->getArgOperand(0);
        } else {
            saturateCandidate = 0;
        }

        if (saturateCandidate) {
            // Check that one of the fMin's argument is 1. If not, then don't do
            // a saturate

            bool fMinHasOne = (minC0 && IsOne(minC0)) || (minC1 && IsOne(minC1));
            if (! fMinHasOne) {
                saturateCandidate = 0;
            }
        }

        if (saturateCandidate) {
            // Make the fSaturate call
            const Type* tys[2] = { fMin->getType(), fMin->getType() };
            Function* f = Intrinsic::getDeclaration(module, Intrinsic::gla_fSaturate, tys, 2);
            Instruction* satInst = builder->CreateCall(f, saturateCandidate);

            fMin->replaceAllUsesWith(satInst);
            deadList.push_back(fMin);
            deadList.push_back(fMax);

            return true;
        }
    }

    // Make an fClamp
    const Type* tys[4] = { fMin->getType(), fMin->getType(), fMin->getType(), fMin->getType() };
    Function* f = Intrinsic::getDeclaration(module, Intrinsic::gla_fClamp, tys, 4);
    int nonfMaxOpIdx = fMin->getArgOperand(0) == fMax ? 1 : 0;
    assert(fMin->getArgOperand(nonfMaxOpIdx) != fMax && fMin->getArgOperand(!nonfMaxOpIdx) == fMax);

    Instruction* clampInst = builder->CreateCall3(f, fMax->getArgOperand(0), fMax->getArgOperand(1),
                                                  fMin->getArgOperand(nonfMaxOpIdx));

    fMin->replaceAllUsesWith(clampInst);
    deadList.push_back(fMin);
    deadList.push_back(fMax);

    return true;
}


bool GatherInsts::visitIntrinsic(IntrinsicInst* intr)
{
    switch (intr->getIntrinsicID()) {
    case Intrinsic::gla_fMax:
        // min(max(a,b),c)

        // TODO: handle the multiple-use case
        if (intr->hasOneUse())
            if (IntrinsicInst* useIntr = dyn_cast<IntrinsicInst>(intr->use_back()))
                if (useIntr->getIntrinsicID() == Intrinsic::gla_fMin)
                    return visitMinMaxPair(useIntr, intr);

        return false;

    case Intrinsic::gla_fMin:
        // max(a, min(b,c))
        // TODO: check for fMin into a saturate/clamp

        return false;

    default:
        return false;
    } // end of switch (intr->getIntrinsicID())
}


bool GatherInsts::visit(Instruction* inst)
{
    bool changed = false;

    if (IntrinsicInst* intr = dyn_cast<IntrinsicInst>(inst)) {
        return visitIntrinsic(intr);
    }

    switch (inst->getOpcode()) {

    case Instruction::SExt:
    case Instruction::ZExt:
        // TODO
        ;


    } // end of switch (inst->getOpcode())

    return changed;
}

void GatherInsts::getAnalysisUsage(AnalysisUsage& AU) const
{
    return;
}

void GatherInsts::print(raw_ostream&, const Module*) const
{
    return;
}

char GatherInsts::ID = 0;
INITIALIZE_PASS_BEGIN(GatherInsts,
                      "gather-instructions",
                      "Gather instructions for LunarGLASS",
                      false,  // Whether it looks only at CFG
                      false); // Whether it is an analysis pass
INITIALIZE_PASS_END(GatherInsts,
                    "gather-instructions",
                    "Gather instructions for LunarGLASS",
                    false,  // Whether it looks only at CFG
                    false); // Whether it is an analysis pass

FunctionPass* gla_llvm::createGatherInstsPass()
{
    return new GatherInsts();
}

