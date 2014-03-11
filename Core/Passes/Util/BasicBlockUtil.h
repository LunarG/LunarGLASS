//===- BasicBlockUtil.h - Utility functions for basic blocks --------------===//
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
// Provides utility functions for BasicBlocks
//
//===----------------------------------------------------------------------===//

#ifndef BASICBLOCK_UTIL_H
#define BASICBLOCK_UTIL_H

#pragma warning(push, 1)
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Instructions.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Analysis/DominanceFrontier.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Support/CFG.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#pragma warning(pop)

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

    inline bool AreEmptyBB(const SmallVectorImpl<BasicBlock*>& bbs)
    {
        if (bbs.empty())
            return true;

        for (SmallVectorImpl<BasicBlock*>::const_iterator i = bbs.begin(), e = bbs.end(); i != e; ++i)
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
            else
                break;
    }

    inline bool HasPHINodes(const BasicBlock* bb)
    {
        for (BasicBlock::const_iterator i = bb->begin(), e = bb->end(); i != e; ++i)
            if (isa<PHINode>(i))
                return true;
            else
                break;

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
        if (! dtn) {
            DeleteDeadBlock(bb);
            return;
        }

        // Simplest case: no dominated blocks
        if (dtn->getNumChildren() == 0) {
            dtn->clearAllChildren();
            BasicBlock* toRemove = dtn->getBlock();
            dt.eraseNode(toRemove);
            DeleteDeadBlock(toRemove);

            return;
        }

        Function* f = bb->getParent();

        // Otherwise, traverse the CFG in forwards order (i.e. reverse
        // post-order), gather the dominated blocks to be pruned.
        SmallVector<BasicBlock*,32> workList;

        typedef ReversePostOrderTraversal<BasicBlock*> RPOTType;
        RPOTType rpot(dtn->getBlock());
        for (RPOTType::rpo_iterator i = rpot.begin(), e = rpot.end(); i != e; ++i) {
            // Stop as soon as we get to a block that bb doesn't dominate (the
            // beginning of the rest of the program)
            if (! dt.dominates(bb, *i))
                break;

            workList.push_back(*i);
        }

        for (SmallVector<BasicBlock*,32>::iterator i = workList.begin(),
                 e = workList.end(); i != e; ++i) {
            BasicBlock* toRemove = *i;
            assert(pred_begin(toRemove) == pred_end(toRemove)
                   && "Block with live predecessors dominated by no-predecessor block");

            DeleteDeadBlock(toRemove);
        }

        dt.getBase().recalculate(*f);
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
    inline void GetDominatedChildren(DomTreeNode* dtn, BasicBlock* bb, SmallVectorImpl<BasicBlock*>& children)
    {
        if (dtn) {
            for (df_iterator<DomTreeNode*> i = GraphTraits<DomTreeNode*>::nodes_begin(dtn), e = GraphTraits<DomTreeNode*>::nodes_end(dtn); i != e; ++i) {
                children.push_back((*i)->getBlock());
            }
        }
    }
    inline void GetDominatedChildren(DominatorTree& dt, BasicBlock* bb, SmallVectorImpl<BasicBlock*>& children)
    {
        GetDominatedChildren(dt.getNode(bb), bb, children);
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

    // TODO LLVM 3.2: refine to compute the needed partial dominance
    // frontier, and use it.

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


    // Hoist all the instructions from src to just before dst. The caller should
    // make sure that hoisting is valid before calling this.
    inline void Hoist(BasicBlock* src, Instruction* dst)
    {
        for (BasicBlock::iterator instI = src->begin(), instE = src->end(); instI != instE; /* empty */) {
            Instruction* i = instI;
            ++instI;

            if (i == src->getTerminator())
                break;

            i->moveBefore(dst);
        }

        assert(IsEmptyBB(src));
    }

    // Basic Block version of the above --- move before dst's terminator
    inline void Hoist(BasicBlock* src, BasicBlock* dst)
    {
        Hoist(src, dst->getTerminator());
    }


    // Duplicate the given basic block, having one of the predecessors go
    // through the duplicate instead. Updates all information and creates a
    // merge block to phi the values into. After duplication and updating, the
    // blocks are then simplified. Returns the newly created duplicate
    BasicBlock* DuplicateBasicBlock(BasicBlock* toDuplicate);

} // end namespace gla_llvm

#endif // BASICBLOCK_UTIL_H

