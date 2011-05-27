//===- BasicBlockUtil.h - Utility functions for basic blocks --------------===//
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
// Provides utility functions for BasicBlocks
//
//===----------------------------------------------------------------------===//

#ifndef BASICBLOCK_UTIL_H
#define BASICBLOCK_UTIL_H

#include "llvm/BasicBlock.h"
#include "llvm/Instructions.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Analysis/DominanceFrontier.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Support/CFG.h"

#include "Passes/Util/ADT.h"

namespace gla_llvm {
    using namespace llvm;

    // Whether bb is NULL or has no instructions
    inline bool InvalidatedBB(const BasicBlock* bb)
    {
        return ! bb || bb->empty();
    }

    // Whether a basic block has no constituent instructions, other than
    // it's phi-nodes and terminator.
    inline bool IsEmptyBB(const BasicBlock* bb)
    {
        return bb->getFirstNonPHIOrDbg() == bb->getTerminator();
    }

    inline bool AreEmptyBB(SmallVectorImpl<BasicBlock*>& bbs)
    {
        if (bbs.empty())
            return true;

        for (SmallVectorImpl<BasicBlock*>::iterator i = bbs.begin(), e = bbs.end(); i != e; ++i)
            if (! IsEmptyBB(*i))
                return false;

        return true;
    }

    // const version
    inline bool AreEmptyBB(SmallVectorImpl<const BasicBlock*>& bbs)
    {
        if (bbs.empty())
            return true;

        for (SmallVectorImpl<const BasicBlock*>::iterator i = bbs.begin(), e = bbs.end(); i != e; ++i)
            if (! IsEmptyBB(*i))
                return false;

        return true;
    }

    // Whether from unconditionaly branches to to.
    inline bool UncondBranchesTo(const BasicBlock* from, const BasicBlock* to)
    {
        const BranchInst* bi = dyn_cast<BranchInst>(from->getTerminator());
        return bi && bi->isUnconditional() && (bi->getSuccessor(0) == to);
    }

    // Whether the block terminates in a conditional branch
    inline bool IsConditional(const BasicBlock* bb)
    {
        if (const BranchInst* bi = dyn_cast<BranchInst>(bb->getTerminator()))
            return bi->isConditional();

        return false;
    }

    // Whether the block terminates in a unconditional branch
    inline bool IsUnconditional(const BasicBlock* bb)
    {
        return !IsConditional(bb);
    }

    // If the block terminates in a conditional branch, get that condition, else
    // return NULL
    inline Value* GetCondition(const BasicBlock* bb)
    {
        const BranchInst* br = dyn_cast<BranchInst>(bb->getTerminator());
        if (! br || br->isUnconditional())
            return NULL;

        return br->getCondition();
    }

    // Two BasicBlocks are equivalent if they are the same BB, or one
    // unconditionally branches to the other and is empty. Equivalence thus is
    // transitive, but currently unimplemented for this function.
    inline bool AreEquivalent(const BasicBlock* bb1, const BasicBlock* bb2)
    {
        return bb1 == bb2
            || (IsEmptyBB(bb1) && (UncondBranchesTo(bb1, bb2)))
            || (IsEmptyBB(bb2) && (UncondBranchesTo(bb2, bb1)));
    }

    // Collect all the phi nodes in bb into a phis
    template<unsigned Size>
    inline void GetPHINodes(const BasicBlock* bb, SmallPtrSet<const PHINode*, Size>& phis)
    {
        for (BasicBlock::const_iterator i = bb->begin(), e = bb->end(); i != e; ++i)
            if (const PHINode* pn = dyn_cast<PHINode>(i))
                phis.insert(pn);
    }

    inline bool HasPHINodes(const BasicBlock* bb)
    {
        for (BasicBlock::const_iterator i = bb->begin(), e = bb->end(); i != e; ++i)
            if (isa<PHINode>(i))
                return true;

        return false;
    }

    // Whether block A is a predecessor of B
    inline bool IsPredecessor(const BasicBlock* pred, const BasicBlock* succ)
    {
        for (const_pred_iterator i = pred_begin(succ), e = pred_end(succ); i != e; ++i)
            if (*i == pred)
                return true;

        return false;
    }

    // Whether the passed block has no predecessors (and is not the entry).
    inline bool IsNoPredecessorBlock(BasicBlock* bb)
    {
        // Not the entry, and no predecessors
        return &*bb->getParent()->begin() != bb && pred_begin(bb) == pred_end(bb);
    }

    // Drop all references, clear it's instruction list, and remove from the 
    // Parent
    inline void UnlinkBB(BasicBlock* bb)
    {
        bb->dropAllReferences();
        bb->getInstList().clear();
        // TODO: change to eraseFromParent whem structures are capable of 
        // updating themselves
        bb->removeFromParent();
        assert(bb->empty());
    }

    // Remove bb from each successor's predecessor list, from it's function, and
    // drop all references. Clears its instruction list.
    inline void EraseBB(BasicBlock* bb)
    {
        for (succ_iterator i = succ_begin(bb), e = succ_end(bb); i != e; ++i)
            (*i)->removePredecessor(bb);

        UnlinkBB(bb);
    }

    // Prune bb from its function, and from the dominator tree. This will
    // unlink/erase bb, and any block that it dominates, updating the dominator
    // tree as it goes along. It may be advisable to call this only with a
    // no-predecessor block.
    // TODO: consider first checking to see if it's no-predecessor, and doing
    // nothing otherwise.
    // O(n*erase(BasicBlock)) where n is the size of the subgraph to be removed,
    // and erase(BasicBlock) is the cost of erasing a basic block from a function.
    inline void PruneCFG(BasicBlock* bb, DominatorTree& dt)
    {
        DomTreeNode* dtn = dt.getNode(bb);
        if (! dtn)
            return;

        SmallVector<DomTreeNode*,32> workList;
        for (df_iterator<DomTreeNode*> i =  GraphTraits<DomTreeNode*>::nodes_begin(dtn), e =  GraphTraits<DomTreeNode*>::nodes_end(dtn); i != e; ++i) {
            workList.push_back(*i);
        }

        // Efficiency note: Since we're operating in depth-first pre-order,
        // erasing the domTreeNode is constant for all but the first iteration,
        // where it's linear in the number of the immediate dominator's
        // children.
        // TODO: consider finding a clever way of seeing if we need bother
        // removing ourselves as a predecessor while erasing the bb.
        for (SmallVector<DomTreeNode*,32>::iterator dtn = workList.begin(), e = workList.end(); dtn != e; ++dtn) {
            (*dtn)->clearAllChildren();
            BasicBlock* toRemove = (*dtn)->getBlock();
            dt.eraseNode(toRemove);
            EraseBB(toRemove);
        }
    }

    // Remove bb if it's a no-predecessor block, and continue on to its
    // successors. Returns the number removed. If a DominatorTree is passes in,
    // it will update it as it removes blocks.
    // TODO: extract into .cpp file if it remains big (and no-inline).
    // TODO: document complexity
    inline bool RecursivelyRemoveNoPredecessorBlocks(BasicBlock* bb, DominatorTree* dt = 0)
    {
        if (! IsNoPredecessorBlock(bb))
            return false;

        // If we're given a dominator tree, then we can just prune
        if (dt) {
            PruneCFG(bb, *dt);
            return true;
        }

        SmallVector<BasicBlock*, 16> toRemove;
        toRemove.push_back(bb);

        while (! toRemove.empty()) {
            BasicBlock* next = toRemove.pop_back_val();
            for (succ_iterator sI = succ_begin(next), sE = succ_end(next); sI != sE; ++sI) {
                (*sI)->removePredecessor(bb);
                if (IsNoPredecessorBlock(*sI))
                    toRemove.push_back(*sI);
            }

            UnlinkBB(bb);
        }

        return true;
    }

    // Whether bb's dominance frontier contains any of the targets, and only the
    // targets. It must contain at least 1.
    // TODO: document complexity
    inline bool ContainedDominanceFrontier(const BasicBlock* bb, SmallVectorImpl<const BasicBlock*>& targets, DominanceFrontier& df)
    {
        int num = 0;

        BasicBlock* unconstBB = const_cast<BasicBlock*>(bb); // Necessary
        DominanceFrontier::DomSetType dst = df.find(unconstBB)->second;

        for (SmallVectorImpl<const BasicBlock*>::iterator target = targets.begin(), e = targets.end(); target != e; ++target) {
            BasicBlock* unconstTarget = const_cast<BasicBlock*>(*target); // Necessary
            if (dst.count(unconstTarget))
                ++num;
        }

        return num != 0 && dst.size() == num;
    }


    // Const version of properlyDominates
    inline bool ProperlyDominates(const BasicBlock* dominator, const BasicBlock* dominatee, const DominatorTree& dt)
    {
        BasicBlock* unconstTor = const_cast<BasicBlock*>(dominator); // Necessary
        BasicBlock* unconstTee = const_cast<BasicBlock*>(dominatee); // Necessary

        return dt.properlyDominates(unconstTor, unconstTee);
    }

    // Gather up all the children of the passed basic block that are dominated
    // by it.
    inline void GetDominatedChildren(const DominatorTree& dt, BasicBlock* bb, SmallVectorImpl<BasicBlock*>& children)
    {
        // Get the node in the tree for bb
        DomTreeNode* n = dt.getNode(bb);
        assert(n);

        for (df_iterator<DomTreeNode*> i = GraphTraits<DomTreeNode*>::nodes_begin(n), e = GraphTraits<DomTreeNode*>::nodes_end(n); i != e; ++i) {
            children.push_back((*i)->getBlock());
        }
    }

    // const version
    inline void GetDominatedChildren(const DominatorTree& dt, const BasicBlock* bb, SmallVectorImpl<const BasicBlock*>& children)
    {
        // Get the node in the tree for bb
        DomTreeNode* n = dt.getNode(const_cast<BasicBlock*>(bb)); // Safe
        assert(n);

        for (df_iterator<DomTreeNode*> i = GraphTraits<DomTreeNode*>::nodes_begin(n), e = GraphTraits<DomTreeNode*>::nodes_end(n); i != e; ++i) {
            children.push_back((*i)->getBlock());
        }
    }

    // Returns the specified successor block. If bb does not end in a
    // BranchInst, or i is out of range, returns NULL
    inline const BasicBlock* GetSuccessor(int i, const BasicBlock* bb)
    {
        const BranchInst* br = dyn_cast<BranchInst>(bb->getTerminator());
        if (! br || (br->isUnconditional() && i != 0) || (br->isConditional() && i != 0 && i != 1))
            return NULL;

        return br->getSuccessor(i);
    }

    // Add all the merge points of the given basic blocks to the (empty) merges
    // set. Clears merges first if non-empty.
    // Note: body must appear in this header file, or else explicit
    // instantiations must be given.
    // O(n*m*log(m)) where n is the size of bbVec, and m is the size of the
    // average dominance frontier for each bb in bbVec.
    template<unsigned S>
    void GetMergePoints(SmallVectorImpl<BasicBlock*>& bbVec, DominanceFrontier& domFront,
                        SmallPtrSet<BasicBlock*, S>& merges)
    {
        merges.clear();

        if (bbVec.size() == 0) {
            return;
        }

        merges.insert(bbVec[0]);

        if (bbVec.size() == 1) {
            return;
        }

        // Initialize merges to contain the first domFront (and the first
        // element)
        DominanceFrontier::DomSetType firstDomFront = (*domFront.find(bbVec[0])).second;
        for (DominanceFrontier::DomSetType::iterator i = firstDomFront.begin(), e = firstDomFront.end(); i != e; ++i) {
            merges.insert(*i);
        }

        // Take the set_intersection of them all (starting on the second one)
        SmallVectorImpl<BasicBlock*>::iterator bb = bbVec.begin(), e = bbVec.end();
        ++bb;
        for (/* empty */; bb != e; ++bb) {
            // If merges contains bb itself, then we need to add it to our set
            // of merge points so that each block can itself be a merge point.
            bool contains = merges.count(*bb);

            SetIntersect(merges, (*domFront.find(*bb)).second);

            // Restore bb to being in merges, since a basic block wont be in its
            // own dominance frontier.
            if (contains)
                merges.insert(*bb);

        }

        return;
    }

    // TODO: const version

    template<unsigned S>
    inline void GetMergePoints(BasicBlock* bb1, BasicBlock* bb2, DominanceFrontier& domFront, SmallPtrSet<BasicBlock*, S>& merges)
    {
        SmallVector<BasicBlock*,2> bbs;
        bbs.push_back(bb1);
        bbs.push_back(bb2);
        GetMergePoints(bbs, domFront, merges);
    }

} // end namespace gla_llvm

#endif // BASICBLOCK_UTIL_H

