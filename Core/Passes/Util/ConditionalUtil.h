//===- ConditionalUtil.h - Utility functions for conditionals -------------===//
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
// Provides utility functions and the Conditional class for dealing with,
// analyzing, and transforming conditionals.
//
//===----------------------------------------------------------------------===//

#ifndef CONDITIONAL_UTIL_H
#define CONDITIONAL_UTIL_H

#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/DominanceFrontier.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Local.h"

#include "Passes/Util/ADT.h"
#include "Passes/Util/BasicBlockUtil.h"
#include "Passes/Util/FunctionUtil.h"

namespace gla_llvm {
    using namespace llvm;

    // Class providing analysis info about a conditional expression.
    class Conditional {
        typedef SmallPtrSet<BasicBlock*,8> Filter;
    public:
        Conditional(BasicBlock* entryBlock, BasicBlock* thenBlock, BasicBlock* elseBlock, DominanceFrontier& dfs, DominatorTree& dt, Loop* loop)
            : entry(entryBlock)
            , left(thenBlock)
            , right(elseBlock)
            , domFront(&dfs)
            , domTree(&dt)
            , mergesFilter(loop ? Filter(loop->getBlocks().begin(), loop->getBlocks().end()) : Filter())
            , latch(loop ? loop->getLoopLatch() : 0)
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
            selfContained = emptyConditional = false;
            leftChildren.clear();
            rightChildren.clear();
            merges.clear();
        }

        // Recalculate all the analysis information (e.g. if the CFG has been
        // modified so as to render it incorrect).
        // TODO: figure out interaction with stale domfront/domtree info
        void recalculate();

        // Whether the respective branch subgraphs contains returns/discards
        // bool hasThenReturn()      const { return leftReturns; }
        // bool hasElseReturn()      const { return rightReturns; }
        // bool hasThenDiscard()     const { return leftDiscards; }
        // bool hasElseDiscard()     const { return rightDiscards; }
        // bool hasDiscard()         const { return leftDiscards || rightDiscards; }
        // bool hasReturn()          const { return leftReturns || rightReturns; }
        // bool hasReturnOrDiscard() const { return hasReturn() || hasDiscard(); }

        // Whether there is no "then" block, only an "else" one. This may be
        // useful information, e.g. if a transformation wishes to invert a
        // condition and flip branches around.
        bool isIfElse() const { return left == merge; }

        bool isIfThen() const { return right == merge; }

        bool isIfThenElse() const { return ! (isIfElse() || isIfThen()); }

        // Whether the conditional is self-contained. This means that each the
        // then and else blocks, if they are distinct from the merge block,
        // point to subgraphs whose only exit is the merge block.
        bool isSelfContained() const { return selfContained; }

        // Whether the conditional is empty. An empty conditional is a
        // self-contained conditional in which the then and else subgraphs, if
        // they exist, are all empty blocks. Currently unimplemented in the case
        // of then or else subgraphs, for which is currently will return false.
        bool isEmptyConditional() const { return emptyConditional; }

        bool contains(BasicBlock* bb) const
        {
            return entry == bb || merge == bb
                || SmallVectorContains(leftChildren, bb) || SmallVectorContains(rightChildren, bb);
        }

        // Accessors.
        // TODO: consider whether these should be const
        const BasicBlock* getEntryBlock() const { return entry; }
        const BasicBlock* getMergeBlock() const { return merge; }
        const BasicBlock* getThenBlock()  const { return left; }
        const BasicBlock* getElseBlock()  const { return right; }

        const BranchInst* getBranchInst() const { return dyn_cast<BranchInst>(entry->getTerminator()); }

        const Value* getCondition() const { return getBranchInst()->getCondition(); };


        // Transformation/optimzations to conditionals


        // Transform phi nodes in the merge block whose result is solely
        // contingent on the entry's condition into selects. Currently only
        // creates them when the values are in the left and right blocks and
        // when it is an empty Conditional. Returns whether it created any
        // selects.
        // TODO: Extend to non-empty Conditionals. Extend to right and left
        // subgraphs
        bool createMergeSelects();

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
        // TODO: Be able to handle phis in the merge block that don't depend on
        // any part of the conditional in any way.
        bool removeIfEmpty()
        {
            // We only do it if we're empty conditional, selfcontained, have no
            // phi nodes, and are linked to a function
            // TODO: can clean up when conditional trees are implemented
            if (InvalidatedBB(entry) || ! emptyConditional || ! selfContained || HasPHINodes(merge)) {
                return false;
            }

            ReplaceInstWithInst(entry->getTerminator(), BranchInst::Create(merge));

            RecursivelyRemoveNoPredecessorBlocks(left, domTree);
            RecursivelyRemoveNoPredecessorBlocks(right, domTree);

            SimplifyInstructionsInBlock(entry);
            SimplifyInstructionsInBlock(merge);

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

        bool filterExcludes;

        Filter mergesFilter;

        BasicBlock* latch;

        Function& function;
        bool isMain;

        BasicBlock* stageExit;
        BasicBlock* stageEpilogue;

        SmallPtrSet<BasicBlock*,3> merges;

        BasicBlock* merge;

        bool selfContained;
        bool emptyConditional;

        // bool leftReturns;
        // bool leftDiscards;
        // bool rightReturns;
        // bool rightDiscards;

        bool incompleteMerges;

        // Children of the left and right blocks (including the blocks themselves)
        SmallVector<BasicBlock*, 32> leftChildren;
        SmallVector<BasicBlock*, 32> rightChildren;

    };

} // end namespace gla_llvm


#endif // CONDITIONAL_UTIL_H

