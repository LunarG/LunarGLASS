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
// Provides utility functions and the Conditional class for dealing with and
// analyzing conditionals
//
//===----------------------------------------------------------------------===//

#ifndef CONDITIONAL_UTIL_H
#define CONDITIONAL_UTIL_H

#include "llvm/Analysis/Dominators.h"

#include "Passes/Util/ADT.h"
#include "Passes/Util/BasicBlockUtil.h"

namespace gla_llvm {
    using namespace llvm;

    // Class providing analysis info about a conditional expression.
    class Conditional {
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

        // Recalculate all the analysis information (e.g. if the CFG has been
        // modified so as to render it incorrect).
        // TODO: figure out interaction with stale domfront/domtree info
        // TODO: put in .cpp file
        void recalculate()
        {
            children.clear();
            children.push_back(left);
            children.push_back(right);

            // Calculate merge
            merge = GetSingleMergePoint(children, domFront);

            // Is it self contained?
            if (! merge)
                selfContained = false;
            else {
                DominanceFrontier::DomSetType leftDomFront  = domFront.find(left)->second;
                DominanceFrontier::DomSetType rightDomFront = domFront.find(right)->second;
                bool leftPure  = (left  == merge) || (leftDomFront.count(merge)  && leftDomFront.size() == 1);
                bool rightPure = (right == merge) || (rightDomFront.count(merge) && rightDomFront.size() == 1);
                selfContained = leftPure && rightPure;
            }

            // Is it an empty conditional?
            if (! selfContained || ! merge)
                emptyConditional = false;
            else {
                SmallVector<const BasicBlock*, 16> kids;
                if (! isIfElse())
                    GetDominatedChildren(domTree, left, kids);

                if (! isIfThen())
                    GetDominatedChildren(domTree, right, kids);

                emptyConditional = AreEmptyBB(kids);
            }

            // Recalculate children
            children.push_back(entry);
            if (! isIfThen())
                GetDominatedChildren(domTree, right, children);
            if (! isIfElse())
                GetDominatedChildren(domTree, left, children);
            if (merge)
                children.push_back(merge);
        }

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

        BasicBlock* getEntryBlock() const { return entry; }
        BasicBlock* getMergeBlock() const { return merge; }
        BasicBlock* getThenBlock()  const { return left; }
        BasicBlock* getElseBlock()  const { return right; }

        BranchInst* getBranchInst() const { return dyn_cast<BranchInst>(entry->getTerminator()); }

        Value* getCondition() const { return getBranchInst()->getCondition(); };
    };

} // end namespace gla_llvm


#endif // CONDITIONAL_UTIL_H

