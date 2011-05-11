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
    clear();
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
        if (! isIfElse()) {
            GetDominatedChildren(domTree, left, kids);

        }

        if (! isIfThen()) {
            GetDominatedChildren(domTree, right, kids);

        }

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

bool Conditional::createMergeSelects()
{
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
