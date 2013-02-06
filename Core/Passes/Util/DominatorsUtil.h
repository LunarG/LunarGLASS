//===- DominatorsUtil.h - Utilities for (post)dominator trees/frontiers ---===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (c) 2010-2013 LunarG, Inc.
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
// Provides utility functions for (post)dominator trees and frontiers
//
//===----------------------------------------------------------------------===//

#ifndef GLA_DOMINATORSUTIL_H
#define GLA_DOMINATORSUTIL_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/PostDominators.h"

#include "Passes/Util/BasicBlockUtil.h"

namespace gla_llvm {
    using namespace llvm;

    // Get all the (non-terminator) (post)dominated instructions, and put them into
    // result.
    // Complexity: Linear in the number of instructions in the dominator subtree
    inline void GetAllDominatedInstructions(Instruction* inst, DominatorTreeBase<BasicBlock>& dt, std::vector<Instruction*>& result)
    {
        BasicBlock* origBlock = inst->getParent();

        SmallVector<BasicBlock*, 32> domBlocks;
        GetDominatedChildren(dt.getNode(origBlock), origBlock, domBlocks);

        // Add in the non-terminator instructions (post)dominated by the given instruction
        BasicBlock::InstListType& instList = origBlock->getInstList();

        // Start from the beginning for post-dominators, start at the end for
        // dominators. I don't know of a way to type-unify iplist's iterator
        // with reverse_iterator, hence the separate loops.
        if (dt.isPostDominator()) {
            for (BasicBlock::iterator i = instList.begin(); &*i != inst; ++i) {
                if (i->isTerminator())
                    continue;

                result.push_back(i);
            }
        } else {
            for (iplist<Instruction>::reverse_iterator i = instList.rbegin(); &*i != inst; ++i) {
                if (i->isTerminator())
                    continue;

                result.push_back(&*i);
            }
        }

        // Add in all the non-terminator instructions of all the (post)
        // dominated blocks.
        for (SmallVector<BasicBlock*,32>::iterator bbI = domBlocks.begin(), bbE = domBlocks.end(); bbI != bbE; ++bbI) {
            // Skip the original block
            if (*bbI == origBlock)
                continue;

            for (BasicBlock::iterator instI = (*bbI)->begin(), instE = (*bbI)->end(); instI != instE; ++instI) {
                if (instI->isTerminator())
                    continue;

                result.push_back(instI);
            }
        }
    }

    // Given a list of nodes (e.g. basic blocks), find the one that
    // [post]dominates all the others. Returns 0 if no such node exists
    // template<class BasicBlock>
    inline BasicBlock* GetDominatorInList(ArrayRef<BasicBlock*> nodes, DominatorTreeBase<BasicBlock>& dt)
    {
        if (nodes.size() == 0)
            return 0;

        // Two-pass algorithm.

        // First, make a pass finding a candidate (a node that dominates the
        // rest of the list). Nodes trivially dominate themselves.
        BasicBlock* candidate = nodes.front();
        for (ArrayRef<BasicBlock*>::iterator i = nodes.begin(), e = nodes.end(); i != e; ++i) {
            if (! dt.dominates(candidate, *i))
                candidate = *i;
        }

        // Second, verify the candidate dominates the entire list. Nodes
        // trivially dominate themselves.
        for (ArrayRef<BasicBlock*>::iterator i = nodes.begin(), e = nodes.end(); i != e; ++i) {
            if (! dt.dominates(candidate, *i))
                return 0;
        }

        return candidate;
    }

    // For a given block, compute the [post]dominance frontier. This computes
    // the [post]dominance frontier by looping over the leaves of the
    // [post]dominator tree for the given block, and gathering up the
    // successors/predecessors. Note that this traverses the entire
    // [post]dominator tree, and should be called sparingly. If a user wishes to
    // know the frontiers for many/most basic blocks, it may be more efficient
    // to calculate the frontiers for the entire function up front, rather than
    // lazily as is done here.
    inline void ComputeDominanceFrontier(BasicBlock* bb, DominatorTreeBase<BasicBlock>& dt,
                                         /* output */ SmallVectorImpl<BasicBlock*>& df)
    {
        DomTreeNode* dtn = dt.getNode(bb);

        // Loop over all the leaves trying to find blocks in the dominance
        // frontier
        for (po_iterator<DomTreeNode*> i = po_begin(dtn), e = po_end(dtn); i != e; ++i) {
            if (0 != i->getNumChildren())
                continue;

            BasicBlock* leaf = (*i)->getBlock();
            if (dt.isPostDominator()) {
                // Add in all the predecessors
                df.append(pred_begin(leaf), pred_end(leaf));
            } else {
                // Add in all the successors
                df.append(succ_begin(leaf), succ_end(leaf));
            }
        }

#ifndef NDEBUG
        // Make sure nothing's dominated
        for (SmallVectorImpl<BasicBlock*>::iterator i = df.begin(), e = df.end(); i != e; ++i) {
            assert(! dt.dominates(bb, *i) && "dominated dominance frontier entry?");
        }
#endif
    }

} // end namespace gla_llvm

#endif /* GLA_DOMINATORSUTIL_H */
