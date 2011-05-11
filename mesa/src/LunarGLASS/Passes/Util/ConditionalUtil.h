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
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Local.h"

#include "Passes/Util/ADT.h"
#include "Passes/Util/BasicBlockUtil.h"

namespace gla_llvm {
    using namespace llvm;

    // Class providing analysis info about a conditional expression.
    class Conditional {
    public:
        Conditional(BasicBlock* entryBlock, BasicBlock* thenBlock, BasicBlock* elseBlock, DominanceFrontier& dfs, DominatorTree& dt)
            : entry(entryBlock)
            , left(thenBlock)
            , right(elseBlock)
            , domFront(dfs)
            , domTree(dt)
        {
            recalculate();
        }

        // Clear out all members of the conditional, useful if the conditional
        // has effectively been removed throught the course of some
        // transformations.
        void clear()
        {
            entry = left = right = merge = NULL;
            selfContained = emptyConditional = false;
            children.clear();
        }

        // Recalculate all the analysis information (e.g. if the CFG has been
        // modified so as to render it incorrect).
        // TODO: figure out interaction with stale domfront/domtree info
        void recalculate();

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

        bool contains(BasicBlock* bb) const { return SmallVectorContains(children, bb); }

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
            bool changed = false;
            for (SmallVectorImpl<BasicBlock*>::iterator i = children.begin(), e = children.end(); i != e; ++i) {
                changed |= SimplifyInstructionsInBlock(entry);
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
            if (! emptyConditional || ! selfContained || HasPHINodes(merge) || ! entry->getParent())
                return false;

            ReplaceInstWithInst(entry->getTerminator(), BranchInst::Create(merge));

            RecursivelyRemoveNoPredecessorBlocks(left);
            RecursivelyRemoveNoPredecessorBlocks(right);

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
            ConstantInt* p = dyn_cast<ConstantInt>(GetCondition(entry));
            if (!p)
                return false;

            ReplaceInstWithInst(entry->getTerminator(), BranchInst::Create(p->isOne() ? left : right));

            RecursivelyRemoveNoPredecessorBlocks(p->isOne() ? right : left);

            SimplifyInstructionsInBlock(entry);
            SimplifyInstructionsInBlock(merge);

            clear();

            return true;
        }

    private:

        BasicBlock* entry;
        BasicBlock* left;
        BasicBlock* right;

        DominanceFrontier& domFront;
        DominatorTree& domTree;

        BasicBlock* merge;

        bool selfContained;
        bool emptyConditional;

        SmallVector<BasicBlock*, 32> children;


    };

} // end namespace gla_llvm


#endif // CONDITIONAL_UTIL_H

