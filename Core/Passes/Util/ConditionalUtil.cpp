//===- ConditionalUtil.cpp - Utility functions for conditionals -----------===//
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

#include "Exceptions.h"
#include "Passes/Util/ConditionalUtil.h"
#include "Passes/Util/DominatorsUtil.h"

using namespace llvm;
using namespace gla_llvm;


// If a cross edge (determined by the existance of multiple merge points) can be
// eliminated through a simple duplication of a small basic block, this will
// return the block to be duplicated. Returns 0 otherwise.
static BasicBlock* DetermineSimpleBlockToDuplicate(ArrayRef<BasicBlock*> merges, DominatorTreeBase<BasicBlock>& postDomTree)
{
    // We need to have two merges
    if (merges.size() != 2)
        return 0;

    // One of which must post dominate the other
    BasicBlock* postDom = GetDominatorInList(merges, postDomTree);
    if (! postDom)
        return 0;

    // The post-dominatee singly branches to the post-dominator
    BasicBlock* postDominatee = postDom == merges.front() ? merges.back() : merges.front();
    if (postDominatee->getTerminator()->getNumSuccessors() != 1
     || postDominatee->getTerminator()->getSuccessor(0) != postDom)
        return 0;

    // The post-dominatee is a small block
    // TODO: Abstract notion of "small"
    if (postDominatee->getInstList().size() > 20)
        return 0;

    // We have found the block to duplicate.
    return postDominatee;
}

void Conditional::recalculate()
{
    // For now, since we don't have a conditional tree, if the entry is not
    // linked to a function, then clear us out
    if (InvalidatedBB(entry)) {
        clear();
        return;
    }

    leftChildren.clear();
    rightChildren.clear();

    // Re-calculate the dominance frontiers for the left and right children
    // Note: LLVM's DominanceFrontier is deprecated, go in the direction of not needing it
    // TODO: is there any issue with a stale domTree?
    DominanceFrontier::DomSetType& leftDomFront  = domFront->find(left)->second;
    DominanceFrontier::DomSetType& rightDomFront = domFront->find(right)->second;
    leftDomFront.clear();
    rightDomFront.clear();

    // For nodes with no predecessor blocks, the domTree has no entry.
    // TODO: is there any issue with a stale domTree?
    if (domTree->getNode(left))
        domFront->calculate(*domTree, domTree->getNode(left));
    if (domTree->getNode(right))
        domFront->calculate(*domTree, domTree->getNode(right));

    // Calculate merges
    GetMergePoints(right, left, *domFront, merges);

    // Filter and exclude blocks from merges.
    Loop* loop = loopInfo->getLoopFor(entry);
    if (loop) {
        Filter mergesFilter = Filter(loop->block_begin(), loop->block_end());
        SetIntersect(merges, mergesFilter);
    }

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
}

// Eliminate cross-edges, if feasible
bool Conditional::eliminateCrossEdges()
{
    if (! hasPotentialCrossEdge())
        return false;

    // Simplest case: We have two merges, one of which post dominates the
    // other and the post-dominatee is a small block that simply branches to
    // the post-dominator. In this case, we can duplicate the basic
    // block. This allows us to create better code with simpler control flow
    // and avoid the more heavy-weight general-purpose solutions for this
    // common scenario.
    BasicBlock* duplicate =
        DetermineSimpleBlockToDuplicate(SmallVector<BasicBlock*,8>(merges.begin(), merges.end()),
                                        *postDomTree->DT);
    if (duplicate) {
        BasicBlock* newBB = DuplicateBasicBlock(duplicate);
        merges.erase(duplicate);

        // Update the dominator tree with the new blocks.
        // OK to have no predecessors, or one, but not >=2.
        assert(newBB &&
               (pred_begin(newBB) == pred_end(newBB) || newBB->getSinglePredecessor()) &&
               newBB->getTerminator()->getNumSuccessors() == 1);

        // Try to eliminate the duplicate-merge block if possible
        bool noDupMerge = TryToSimplifyUncondBranchFromEmptyBlock(*succ_begin(newBB));
        if (! noDupMerge)
            gla::UnsupportedFunctionality("unable to remove the dup-merge block");

        // TODO: compile-time performance: Update the dom tree as we go instead of having to recalculate
        domTree->DT->recalculate(*duplicate->getParent());
    } else if (false /* able to determine conditions */) {
    } else {
        return false;
    }

    recalculate();

    return true;
}


bool Conditional::createMergeSelects()
{
    if (InvalidatedBB(entry) || InvalidatedBB(merge))
        return false;

    bool changed = false;

    // We only work on empty conditionals
    if (! isEmptyConditional())
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

        BasicBlock* leftIncoming = isIfElse() ? entry : left;
        BasicBlock* rightIncoming = isIfThen() ? entry : right;

        for (int i = 0; i < 2; ++i) {
            if (pn->getIncomingBlock(i) == leftIncoming)
                leftVal = pn->getIncomingValue(i);
            else if (pn->getIncomingBlock(i) == rightIncoming)
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
