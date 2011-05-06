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

namespace gla_llvm {
    using namespace llvm;

    // Whether a basic block has no constituent instructions, other than
    // it's phi-nodes and terminator.
    inline bool IsEmptyBB(const BasicBlock* bb)
    {
        return bb->getFirstNonPHIOrDbg() == bb->getTerminator();
    }

    inline bool AreEmptyBB(SmallVectorImpl<const BasicBlock*>& bbs)
    {
        for (SmallVectorImpl<const BasicBlock*>::iterator i = bbs.begin(), e = bbs.end(); i != e; ++i)
            if (! IsEmptyBB(*i))
                return false;

        return true;
    }

    // Whether from unconditionaly branches to to.
    inline bool UncondBranchesTo(BasicBlock* from, BasicBlock* to)
    {
        BranchInst* bi = dyn_cast<BranchInst>(from->getTerminator());
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

    // Remove bb if it's a no-predecessor block, and continue on to its
    // successors. Returns the number removed.
    inline int RecursivelyRemoveNoPredecessorBlocks(BasicBlock* bb)
    {
        if (! IsNoPredecessorBlock(bb))
            return 0;

        int removed = 0;
        SmallVector<BasicBlock*, 16> toRemove;
        toRemove.push_back(bb);

        while (! toRemove.empty()) {
            BasicBlock* next = toRemove.pop_back_val();
            for (succ_iterator sI = succ_begin(next), sE = succ_end(next); sI != sE; ++sI) {
                (*sI)->removePredecessor(bb);
                if (IsNoPredecessorBlock(*sI))
                    toRemove.push_back(*sI);
            }

            next->dropAllReferences();
            next->getParent()->getBasicBlockList().erase(bb);
            ++removed;
        }

        return removed;
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
        for (df_iterator<BasicBlock*> i = df_begin(bb), e = df_end(bb); i != e; ++i) {
            if (! dt.dominates(bb, *i))
                continue;

            children.push_back(*i);
        }
    }
    inline void GetDominatedChildren(const DominatorTree& dt, const BasicBlock* bb, SmallVectorImpl<const BasicBlock*>& children)
    {
        for (df_iterator<const BasicBlock*> i = df_begin(bb), e = df_end(bb); i != e; ++i) {
            if (! dt.dominates(bb, *i))
                continue;

            children.push_back(*i);
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

    // Return the single merge point of the given basic blocks.  Returns null if
    // there is no merge point, or if there are more than 1 merge points.  Note
    // that the presense of backedges or exitedges may cause there to be
    // multiple potential merge points.
    BasicBlock* GetSingleMergePoint(SmallVectorImpl<BasicBlock*>&, DominanceFrontier&);


} // end namespace gla_llvm

#endif // BASICBLOCK_UTIL_H

