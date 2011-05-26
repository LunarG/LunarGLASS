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

    SetDifference(merges, mergesExcludes);


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

    // See if the dominance frontiers contain things not in merges, and that aren't
    // each other.


    // If it's if-else, then the right dom front should contain and only
    // contain left. Similarly for if-then. If it's if-then-else, then we have
    // to look at each of the dominance frontiers and make sure they don't
    // contain anything that isn't in merges.
    if (isIfElse())
        incompleteMerges = ! (rightDomFront.count(left) && rightDomFront.size() == 1);
    else if (isIfThen())
        incompleteMerges = ! (leftDomFront.count(right) && leftDomFront.size() == 1);
    else {
        incompleteMerges = false;

        for (DominanceFrontier::DomSetType::iterator bb = rightDomFront.begin(), e = rightDomFront.end(); bb != e; ++bb) {
            incompleteMerges |= ! merges.count(*bb);
        }

        for (DominanceFrontier::DomSetType::iterator bb = leftDomFront.begin(), e = leftDomFront.end(); bb != e; ++bb) {
            incompleteMerges |= ! merges.count(*bb);
        }
    }

    // Remove the stageExit and stageEpilogue blocks from merges
    merges.erase(stageEpilogue);
    merges.erase(stageExit);

    // If merges now contains one element, then the conditional has a merge
    // point
    if (merges.size() == 1)
        merge = *merges.begin();
    else
        merge = NULL;

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
