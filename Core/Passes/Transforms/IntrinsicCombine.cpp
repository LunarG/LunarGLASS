//===- IntrinsicCombine.cpp - Canonicalize instructions for LunarGLASS ---===//
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
// Combine and optimize over intrinsics to form fewer, simpler
// instructions. Analogous to InstCombine
//
//   * Any instruction dominated or post-dominated by discard is DCEed
//
//   * Change (hoist) discards into discardConditionals which will reside in the
//     post-dominance frontier. TODO: place these discards right after the
//     condition is computed. TODO: only do based on backend query. TODO:
//     migrate the condition as high as it can go.
//
//   * Constant-fold intrinsics. TODO: Fold more intrinsics.
//
//   * TODO: Combine multiple successive fWrites to the same output but with
//     different masks into a single fWrite.
//
//   * TODO: hoist all reading of inputs or instructions involving constants
//     into the header, or at the very least try to migrate discard as far
//     upwards as possible
//
//   * TODO: various inter-intrinsic optimizations
//
//   * TODO: combine min/maxes into clamps when possible
//
//===----------------------------------------------------------------------===//

#include "llvm/GlobalVariable.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/Use.h"
#include "llvm/User.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "Passes/PassSupport.h"
#include "Passes/Immutable/BackEndPointer.h"
#include "Passes/Util/DominatorsUtil.h"
#include "Passes/Util/FunctionUtil.h"
#include "Passes/Util/InstructionUtil.h"

// LunarGLASS helpers
#include "Exceptions.h"
#include "LunarGLASSTopIR.h"
#include "Util.h"


using namespace llvm;
using namespace gla_llvm;

namespace  {
    class IntrinsicCombine : public FunctionPass {
    public:
        static char ID;

        IntrinsicCombine() : FunctionPass(ID), numDiscardDCE(0)
        {
            initializeIntrinsicCombinePass(*PassRegistry::getPassRegistry());
        }

        virtual bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;

    private:
        // Gather all the (non-comparision) discards in the function. Returns
        // whether any were found.
        bool getDiscards(Function&);

        // Anything dominated or postdominated by a discard is dead.
        bool discardAwareDCE(Function&);

        // Discards can be turned into discardConditionals based on the condition needed
        // to reach them. The comparison comes from the post-dominance frontier,
        // and the discardConditional can go there right before the branch. The branch
        // then becomes an unconditional branch to the non-discarding path.
        bool hoistDiscards(Function&);

        // Visit an instruction, trying to optimize it
        bool visit(Instruction*);

        // Empty out the deadList
        void emptyDeadList();

        typedef SmallVector<Instruction*, 4> DiscardList;

        // Hold on to a list of our discards
        DiscardList discards;

        // When combining/optimizing intrinsics, use the deadList to keep around
        // instructions to remove from the function. This allows iterators to be
        // preserved when iterating over instructions.
        std::vector<Instruction*> deadList;

        DominatorTree* domTree;
        PostDominatorTree* postDomTree;
        PostDominanceFrontier* postDomFront;

        Module* module;
        LLVMContext* context;

        BackEnd* backEnd;

        // Statistic info
        int numDiscardDCE;
        int numConstantFolded;
        int numCombined;

        IntrinsicCombine(const IntrinsicCombine&); // do not implement
        void operator=(const IntrinsicCombine&); // do not implement
    };
} // end namespace

void IntrinsicCombine::emptyDeadList()
{
    for (std::vector<Instruction*>::iterator i = deadList.begin(), e = deadList.end(); i != e; ++i)
        (*i)->eraseFromParent();
}

bool IntrinsicCombine::getDiscards(Function& F)
{
    BasicBlock* exit = GetMainExit(F);
    assert(exit && "non-exiting shader, or non-canonicalized CFG");

    // Discards exist in exit predecessors
    for (pred_iterator bbI = pred_begin(exit), e = pred_end(exit); bbI != e; ++bbI)
        for (BasicBlock::iterator instI = (*bbI)->begin(), instE = (*bbI)->end(); instI != instE; ++instI)
            if (IsDiscard(instI))
                discards.push_back(instI);

    return ! discards.empty();
}


bool IntrinsicCombine::discardAwareDCE(Function& F)
{
    // TODO: Revise for side-effects that aren't killed by a discard

    // Build up deadList to be all the dominated and post-dominated instructions
    for (DiscardList::iterator i = discards.begin(), e = discards.end(); i != e; ++i) {
        GetAllDominatedInstructions(*i, *domTree->DT, deadList);
        // GetAllDominatedInstructions(*i, *postDomTree->DT, deadList); // See TODO
    }

    bool changed = deadList.empty();
    numDiscardDCE = deadList.size();

    // DCE: drop references.
    for (std::vector<Instruction*>::iterator i = deadList.begin(), e = deadList.end(); i != e; ++i) {
        (*i)->replaceAllUsesWith(UndefValue::get((*i)->getType()));
        (*i)->dropAllReferences();
    }

    // DCE: erase
    emptyDeadList();

    return changed;
}

bool IntrinsicCombine::hoistDiscards(Function& F)
{
    if (! backEnd->hoistDiscards()) {
        return false;
    }

    for (DiscardList::iterator i = discards.begin(), e = discards.end(); i != e; ++i) {
        PostDominanceFrontier::DomSetType pds = postDomFront->find((*i)->getParent())->second;
        assert(pds.size() == 1 && "Unknown flow control layout or unstructured flow control");

        BasicBlock* targetBlock = *pds.begin();
        BranchInst* br = dyn_cast<BranchInst>(targetBlock->getTerminator());
        assert(br && br->isConditional());

        // Find which branch is the discard
        bool isLeft = domTree->dominates(br->getSuccessor(0), (*i)->getParent());
        bool isRight = domTree->dominates(br->getSuccessor(1), (*i)->getParent());
        assert(! (isLeft && isRight) && "improper post-dominance frontier discovered");

        Value* cond = br->getCondition();

        IRBuilder<> builder(*context);
        builder.SetInsertPoint(br);

        // If we're the right branch, then we should negate the condition before
        // feeding it into discardConditional
        if (isRight) {
            cond = builder.CreateNot(cond);
        }

        const Type* boolTy = gla::GetBoolType(*context);
        builder.CreateCall(Intrinsic::getDeclaration(module, Intrinsic::gla_discardConditional, &boolTy, 1 ),
                           cond);

        // Make the branch now branch on a constant
        br->setCondition(isRight ? ConstantInt::getTrue(*context) : ConstantInt::getFalse(*context));

        // DCE the old discard
        (*i)->dropAllReferences();
        (*i)->eraseFromParent();
    }

    bool changed = ! discards.empty();
    discards.clear();

    return changed;
}

bool IntrinsicCombine::runOnFunction(Function& F)
{
    releaseMemory();

    BackEndPointer* bep = getAnalysisIfAvailable<BackEndPointer>();
    if (! bep)
        return false;

    backEnd = *bep;

    domTree = &getAnalysis<DominatorTree>();
    postDomTree = &getAnalysis<PostDominatorTree>();
    postDomFront = &getAnalysis<PostDominanceFrontier>();

    module  = F.getParent();
    context = &F.getContext();

    bool changed = false;

    bool hasDiscards = getDiscards(F);

    // Perform discard optimizations (if present)
    if (hasDiscards) {
        changed |= discardAwareDCE(F);
        changed |= hoistDiscards(F);
    }

    // Visit each instruction, trying to optimize
    for (Function::iterator bbI = F.begin(), bbE = F.end(); bbI != bbE; ++bbI) {
        for (BasicBlock::iterator instI = bbI->begin(), instE = bbI->end(); instI != instE; ++instI) {
            changed |= visit(instI);
        }
    }

    emptyDeadList();

    return changed;
}

bool IntrinsicCombine::visit(Instruction* inst)
{
    bool changed = false;

    // Try to constant fold it
    if (Constant* c = ConstantFoldIntrinsic(inst)) {
        inst->replaceAllUsesWith(c);
        inst->dropAllReferences();
        deadList.push_back(inst);

        ++numConstantFolded;
        return true;
    }

    // Try to combine it
    // TODO: intrinsic combining

    return changed;
}

void IntrinsicCombine::getAnalysisUsage(AnalysisUsage& AU) const
{
    AU.addRequired<DominatorTree>();
    AU.addRequired<PostDominatorTree>();
    AU.addRequired<PostDominanceFrontier>();
    return;
}

void IntrinsicCombine::print(raw_ostream&, const Module*) const
{
    return;
}


char IntrinsicCombine::ID = 0;
INITIALIZE_PASS_BEGIN(IntrinsicCombine,
                      "intrinsic-combine",
                      "Combine intrinsics for LunarGLASS",
                      false,  // Whether it looks only at CFG
                      false); // Whether it is an analysis pass
INITIALIZE_PASS_DEPENDENCY(DominatorTree)
INITIALIZE_PASS_DEPENDENCY(PostDominatorTree)
INITIALIZE_PASS_DEPENDENCY(PostDominanceFrontier)
INITIALIZE_PASS_END(IntrinsicCombine,
                    "intrinsic-combine",
                    "Combine intrinsics for LunarGLASS",
                    false,  // Whether it looks only at CFG
                    false); // Whether it is an analysis pass

FunctionPass* gla_llvm::createIntrinsicCombinePass()
{
    return new IntrinsicCombine();
}

