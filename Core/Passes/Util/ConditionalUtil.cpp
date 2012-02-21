//===- ConditionalUtil.cpp - Utility functions for conditionals -----------===//
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

#include "Passes/Util/ConditionalUtil.h"

using namespace llvm;
using namespace gla_llvm;

void Conditional::recalculate()
{
    // For now, since we don't have a conditional tree, if the entry is not
    // linked to a function, then clear us out
    if (InvalidatedBB(entry)) {
        clear();
        return;
    }

    leftChildren.clear();
    leftChildren.push_back(left);
    rightChildren.clear();
    rightChildren.push_back(right);

    // Calculate merges
    GetMergePoints(right, left, *domFront, merges);

    // Filter and exclude blocks from merges.
    if (mergesFilter.size())
        SetIntersect(merges, mergesFilter);

    DominanceFrontier::DomSetType leftDomFront  = domFront->find(left)->second;
    DominanceFrontier::DomSetType rightDomFront = domFront->find(right)->second;

    // Set up which subgraphs have early returns/discards
    // leftDiscards  = false;
    // leftReturns   = false;
    // rightDiscards = false;
    // rightReturns  = false;

    // if (stageExit && merges.count(stageExit)) {
    //     leftDiscards = true;
    //     rightDiscards = true;
    // }

    // if (stageEpilogue && merges.count(stageEpilogue)) {
    //     leftReturns = true;
    //     rightReturns = true;
    // }

    // if (stageEpilogue && (! leftReturns || ! rightReturns)) {
    //     leftReturns  = leftDomFront.count(stageEpilogue);
    //     rightReturns = rightDomFront.count(stageEpilogue);
    // }

    // if (stageExit && (! leftDiscards || ! rightDiscards)) {
    //     leftDiscards  = leftDomFront.count(stageExit);
    //     rightDiscards = rightDomFront.count(stageExit);
    // }

    merge = 0;

    // If merges consists solely of the exit block, then one of the branches is
    // an early discard. The non-discard branch either dominates the return
    // block or the return block is in its dominance frontier. Structure this an
    // an if-then, where the then is the discard branch and the merge (i.e. the
    // "rest of the program") is the non-discard branch.
    if (merges.size() == 1 && merges.count(stageExit)) {
        bool leftIsMerge = leftDomFront.count(stageEpilogue) || domTree->dominates(left, stageEpilogue);
        merge = leftIsMerge ? left : right;
    }

    // Always exclude the exit from the merges.If there are other merge points,
    // then those should be used.
    merges.erase(stageExit);

    // TODO: Do something similar to the latch for the stage epilogue. This is a
    // bit more involved and would require some modifications to
    // BottomTranslator.
    // // Exclude the epilogue when we have more than one merge. If the epilogue is
    // // the sole merge point, then this can be structured as an if-then-else
    // // without the need for early returns (thus the epilogue shouldn't be
    // // excluded). If there are other merge points, then this represents an
    // // early-return scenario, and those other merge points should be used.
    // if (merges.size() > 1 && merges.count(stageEpilogue)) {
    //     merges.erase(stageEpilogue);
    // }
    merges.erase(stageEpilogue);

    // If we're a conditional in a loop and merges contains the latch, there are
    // two scenarios:
    //   1) This a loop with a simple latch; thus merges contains only the
    //      latch.
    //   2) This is a continuing conditional (thus, no simple latch); thus
    //      merges contains the real merge point along with the latch
    // Exclude the latch in scenario 2.
    if (latch && merges.size() > 1 && merges.count(latch)) {
        merges.erase(latch);
    }

    // If merges now contains one element, then the conditional has a merge
    // point
    if (merges.size() == 1)
        merge = *merges.begin();

    // If we're if-then, does the then block's dominance

    // If left or right is the merge, then clear out the children vectors (as we
    // don't want them to hold the merge point)
    if (left == merge) {
        leftChildren.clear();
    }
    if (right == merge) {
        rightChildren.clear();
    }


    // Is it self contained?
    if (! merge)
        selfContained = false;
    else {
        bool leftPure  = (left  == merge) || (leftDomFront.count(merge)  && leftDomFront.size() == 1);
        bool rightPure = (right == merge) || (rightDomFront.count(merge) && rightDomFront.size() == 1);
        selfContained = leftPure && rightPure;
    }

    // Populate the children
    if (! isIfElse())
        GetDominatedChildren(*domTree, left, leftChildren);

    if (! isIfThen())
        GetDominatedChildren(*domTree, right, rightChildren);

    // Is it an empty conditional?
    emptyConditional = selfContained && AreEmptyBB(leftChildren) && AreEmptyBB(rightChildren);
}

bool Conditional::createMergeSelects()
{
    if (InvalidatedBB(entry) || InvalidatedBB(merge))
        return false;

    bool changed = false;

    // We only work on empty left and right blocks for now.
    if (! IsEmptyBB(left) || ! IsEmptyBB(right))
        return changed;

    // Check all the phis, and change any of the ones that receive their value
    // from left and right into selects on the entry's condition.
    for (BasicBlock::iterator instI = merge->begin(), instE = merge->end(); instI != instE; /* empty */) {
        Instruction* inst = instI;
        ++instI;

        PHINode* pn = dyn_cast<PHINode>(inst);
        if (!pn)
            break;

        // We only want phis from left and right
        if (pn->getNumIncomingValues() != 2) {
            continue;
        }

        Value* leftVal = NULL;
        Value* rightVal = NULL;

        for (int i = 0; i < 2; ++i) {
            if (pn->getIncomingBlock(i) == left)
                leftVal = pn->getIncomingValue(i);
            else if (pn->getIncomingBlock(i) == right)
                rightVal = pn->getIncomingValue(i);
        }

        if (! leftVal || ! rightVal) {
            continue;
        }

        // If we've made it this far, we have a phi we want to convert into a
        // select.
        ReplaceInstWithInst(pn, SelectInst::Create(GetCondition(entry), leftVal, rightVal, "select"));
        changed = true;
    }

    return changed;
}
