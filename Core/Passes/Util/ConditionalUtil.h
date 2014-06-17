//===- ConditionalUtil.h - Utility functions for conditionals -------------===//
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
//
// Provides utility functions and the Conditional class for dealing with,
// analyzing, and transforming conditionals.
//
//===----------------------------------------------------------------------===//

#ifndef CONDITIONAL_UTIL_H
#define CONDITIONAL_UTIL_H

#pragma warning(push, 1)
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/DominanceFrontier.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Local.h"
#pragma warning(pop)

#include "Passes/Util/ADT.h"
#include "Passes/Util/BasicBlockUtil.h"
#include "Passes/Util/FunctionUtil.h"

namespace gla_llvm {
    using namespace llvm;

    // Class providing analysis info about a conditional expression.
    class Conditional {
        typedef SmallPtrSet<BasicBlock*,8> Filter;
    public:
        Conditional(BasicBlock* entryBlock, BasicBlock* thenBlock, BasicBlock* elseBlock, DominanceFrontier& dfs, DominatorTree& dt, PostDominatorTree& pdt, LoopInfo& li, Pass* p)
            : entry(entryBlock)
            , left(thenBlock)
            , right(elseBlock)
            , domFront(&dfs)
            , domTree(&dt)
            , postDomTree(&pdt)
            , loopInfo(&li)
            , parentPass(p)
            , function(*entryBlock->getParent())
            , isMain(IsMain(function))
            , stageExit(0)
            , stageEpilogue(0)
        {
            if (isMain) {
                stageExit    = GetMainExit(function);
                stageEpilogue = GetMainEpilogue(function);
            }

            recalculate();
        }

        // Clear out all members of the conditional, useful if the conditional
        // has effectively been removed throught the course of some
        // transformations.
        void clear()
        {
            entry = left = right = merge = NULL;
            leftChildren.clear();
            rightChildren.clear();
            merges.clear();
        }

        // Recalculate all the analysis information (e.g. if the CFG has been
        // modified so as to render it incorrect).
        // TODO: loops: figure out interaction with stale domfront/domtree info
        void recalculate();

        // Whether there is no "then" block, only an "else" one. This may be
        // useful information, e.g. if a transformation wishes to invert a
        // condition and flip branches around.
        bool isIfElse() const { return left == merge; }

        bool isIfThen() const { return right == merge; }

        bool isIfThenElse() const { return ! (isIfElse() || isIfThen()); }

        // Whether we are a simple conditional. A simple conditional's then
        // and else subgraphs consist only of one block
        bool isSimpleConditional() const
        {
            bool right = rightChildren.size() == 1;
            bool left  = leftChildren.size() == 1;

            return (right && left) || (isIfElse() && right) || (isIfThen() && left);
        }

        // Whether the conditional is self-contained. This means that each the
        // then and else blocks, if they are distinct from the merge block,
        // point to subgraphs whose only exit is the merge block.
        bool isSelfContained() const { return selfContained; }

        // If the entry block does not dominate the merge block, then we have
        // some kind of shared merge block. This can arise from the elimination
        // of loops that originally contained breaks. The splitSharedMerge
        // method below will remove these.
        bool hasSharedMerge() const
        {
            return merge && ! domTree->dominates(entry, merge);
        }

        // Whether the conditional is empty. An empty conditional is a
        // self-contained conditional in which the then and else subgraphs, if
        // they exist, are all empty blocks. Currently unimplemented in the case
        // of then or else subgraphs, for which is currently will return false.
        bool isEmptyConditional() const
        {
            return isSelfContained() && AreEmptyBB(leftChildren) && AreEmptyBB(rightChildren);
        }

        bool contains(BasicBlock* bb) const
        {
            return entry == bb || merge == bb
                || Has(leftChildren, bb) || Has(rightChildren, bb);
        }

        // Whether the given conditional might have a cross-edge in it.
        bool hasPotentialCrossEdge() { return merges.size() > 1; }

        // Accessors.
        const BasicBlock* getEntryBlock() const { return entry; }
              BasicBlock* getEntryBlock()       { return entry; }

        const BasicBlock* getMergeBlock() const { return merge; }
              BasicBlock* getMergeBlock()       { return merge; }

        const BasicBlock* getThenBlock()  const { return left; }
              BasicBlock* getThenBlock()        { return left; }

        const BasicBlock* getElseBlock()  const { return right; }
              BasicBlock* getElseBlock()        { return right; }

        const BranchInst* getBranchInst() const { return dyn_cast<BranchInst>(entry->getTerminator()); }
              BranchInst* getBranchInst()       { return dyn_cast<BranchInst>(entry->getTerminator()); }

        const Value* getCondition() const { return getBranchInst()->getCondition(); };
              Value* getCondition()       { return getBranchInst()->getCondition(); };


        // Transformation/optimzations to conditionals


        // Eliminate cross-edges, if possible
        bool eliminateCrossEdges();

        // Transform phi nodes in the merge block whose result is solely
        // contingent on the entry's condition into selects. Currently only
        // creates them when the values are in the left and right blocks and
        // when it is an empty Conditional. Returns whether it created any
        // selects.
        // TODO: Extend to non-empty Conditionals. Extend to right and left
        // subgraphs
        bool createMergeSelects();

        // Split out a shared merge block. Updates all analysis info. Note that
        // this may require that contained and containing conditionals be
        // recalculated.
        bool splitSharedMerge()
        {
            // TODO: compile-time performance: Overhaul the conditionals system so that recalculation
            // doesn't have to happen as often
            recalculate();

            if (! hasSharedMerge())
                return false;

            // Gather all the preds associated with this conditional
            SmallVector<BasicBlock*, 8> preds;
            for (pred_iterator i = pred_begin(merge), e = pred_end(merge); i != e; ++i) {
                if (domTree->dominates(entry, *i))
                    preds.push_back(*i);
            }

            merge = SplitBlockPredecessors(merge, preds, "_split", parentPass);
            assert(! hasSharedMerge() && "Entry still does not dominate merge?");

            return true;
        }

        // Have the conditional simplify instructions and remove dead code in
        // it. Returns whether anything happened.
        bool simplifyInsts()
        {
            if (InvalidatedBB(entry)) {
                return false;
            }

            bool changed = false;
            // TODO: remove the below checks after conditionals properly clean
            // up after themselves. The lists currently may contain
            // unlinked/invalid blocks in them, so skip over them.
            for (SmallVectorImpl<BasicBlock*>::iterator i = leftChildren.begin(), e = leftChildren.end(); i != e; ++i) {
                if (! InvalidatedBB(*i)) {
                    changed |= SimplifyInstructionsInBlock(*i);
                }
            }

            for (SmallVectorImpl<BasicBlock*>::iterator i = rightChildren.begin(), e = rightChildren.end(); i != e; ++i) {
                if (! InvalidatedBB(*i)) {
                    changed |= SimplifyInstructionsInBlock(*i);
                }
            }

            return changed;
        }

        // Have the conditional remove itself from the function if it's
        // empty. Returns whether it removed itself. Note that other
        // conditionals containing this one may need to recalculate themselves.
        // Currently doesn't do anything if the merge block has phis in it, so
        // be sure to run createMergeSelects() first.
        // TODO: loops: Be able to handle phis in the merge block that don't depend on
        // any part of the conditional in any way.
        bool removeIfEmpty()
        {
            // We only do it if we're empty conditional, selfcontained, have no
            // phi nodes, and are linked to a function
            // TODO: can clean up when conditional trees are implemented
            if (InvalidatedBB(entry) || ! isEmptyConditional() || ! isSelfContained() || HasPHINodes(merge)) {
                return false;
            }

            ReplaceInstWithInst(entry->getTerminator(), BranchInst::Create(merge));

            RecursivelyRemoveNoPredecessorBlocks(left, domTree);
            RecursivelyRemoveNoPredecessorBlocks(right, domTree);

            SimplifyInstructionsInBlock(entry);
            SimplifyInstructionsInBlock(merge);

            MergeBlockIntoPredecessor(merge, parentPass);

            clear();

            return true;
        }

        // If this Conditional is really "unconditional", e.g. is something like
        // `br i1 true ...', then remove the unreachable branch from the
        // function. Returns whether it did anything.
        bool removeIfUnconditional()
        {
            // We only run if we're linked to a function
            // TODO: can clean up when conditional trees are implemented
            if (InvalidatedBB(entry)) {
                return false;
            }

            ConstantInt* p = dyn_cast<ConstantInt>(GetCondition(entry));
            if (! p) {
                return false;
            }

            ReplaceInstWithInst(entry->getTerminator(), BranchInst::Create(p->isOne() ? left : right));

            RecursivelyRemoveNoPredecessorBlocks(p->isOne() ? right : left, domTree);

            SimplifyInstructionsInBlock(entry);
            SimplifyInstructionsInBlock(merge);

            clear();

            return true;
        }

    private:
        BasicBlock* entry;
        BasicBlock* left;
        BasicBlock* right;

        DominanceFrontier* domFront;
        DominatorTree* domTree;
        PostDominatorTree* postDomTree;
        LoopInfo* loopInfo;

        // Hold a pointer back to our creator. This allows us to update analysis
        // info, i.e. dominator tree, loop info, and dominance frontiers.
        Pass* parentPass;

        bool filterExcludes;
        bool selfContained;

        BasicBlock* latch;
        BasicBlock* exit;

        Function& function;
        bool isMain;

        BasicBlock* stageExit;
        BasicBlock* stageEpilogue;

        SmallPtrSet<BasicBlock*,8> merges;

        BasicBlock* merge;

        bool incompleteMerges;

        // Children of the left and right blocks (including the blocks themselves)
        SmallVector<BasicBlock*, 32> leftChildren;
        SmallVector<BasicBlock*, 32> rightChildren;

    };

} // end namespace gla_llvm


#endif // CONDITIONAL_UTIL_H

