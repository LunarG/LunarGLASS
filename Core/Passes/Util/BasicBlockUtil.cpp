//===- BasicBlockUtil.cpp - Utility functions for basic blocks ------------===//
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
// Provides utility functions for BasicBlocks
//
//===----------------------------------------------------------------------===//

#include "llvm/IRBuilder.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/Local.h"

#include "Passes/Util/BasicBlockUtil.h"

namespace gla_llvm {
using namespace llvm;

// Duplicate the given basic block, redirecting one of the old predecessors
// through the duplicate instead. Updates all information and creates a merge
// block to phi the values into. After duplication and updating, the blocks are
// then simplified.
BasicBlock* DuplicateBasicBlock(BasicBlock* toDuplicate)
{
    // First off, make the merge block. This block will exist to hold a phi node
    // for every value in the duplicated basic block.
    BasicBlock* merge = toDuplicate->splitBasicBlock(toDuplicate->getTerminator(), "dup-merge");

    // Now, clone the block
    ValueToValueMapTy vMap;
    BasicBlock* clone = CloneBasicBlock(toDuplicate, vMap, "-dup", toDuplicate->getParent());

    // Unfortunately, cloning does not update the operands of the instructions
    // in the new block to use the newly cloned prior instructions. Update the
    // operands manually.
    for (BasicBlock::iterator instI = clone->begin(), instE = clone->end(); instI != instE; ++instI) {
        // Check each operand to see if it should be updated
        for (unsigned int i = 0; i < instI->getNumOperands(); ++i) {
            Value* opnd = instI->getOperand(i);
            if (vMap.count(opnd))
                instI->setOperand(i, vMap[opnd]);
        }
    }

    // Add phi nodes to the merge block phi-ing every cloned value.
    IRBuilder<> builder(merge, merge->begin());
    SmallPtrSet<BasicBlock*, 3> internalBlocks;
    internalBlocks.insert(merge);
    internalBlocks.insert(toDuplicate);
    internalBlocks.insert(clone);
    for (BasicBlock::iterator instI = toDuplicate->begin(), instE = toDuplicate->getTerminator();
         instI != instE; ++instI) {
        PHINode* phi = builder.CreatePHI(instI->getType(), 2);
        phi->addIncoming(instI, toDuplicate);
        phi->addIncoming(vMap[instI], clone);

        // Replace all (external) uses of the old value with the result of the
        // new phi.
        for (Instruction::use_iterator uI = instI->use_begin(), uE = instI->use_end(); uI != uE; ++uI) {
            Instruction* userInst = dyn_cast<Instruction>(*uI);
            if (! userInst || internalBlocks.count(userInst->getParent()))
                continue;

            uI->replaceUsesOfWith(instI, phi);
        }
    }

    // Break off one predecessor, and have it go through the clone instead
    BasicBlock* pred = *pred_begin(toDuplicate);
    toDuplicate->removePredecessor(pred);

    BranchInst* br = dyn_cast<BranchInst>(pred->getTerminator());
    assert(br && br->getNumSuccessors() <= 2 &&
           (br->getSuccessor(0) == toDuplicate || br->getSuccessor(1) == toDuplicate));

    br->setSuccessor(br->getSuccessor(0) == toDuplicate ? 0 : 1, clone);

    // Resolve the phis in the cloned block to be the value coming in through
    // its only predecessor
    assert(pred == clone->getSinglePredecessor());
    for (BasicBlock::iterator instI = clone->begin(), instE = clone->end(); instI != instE; ++instI) {
        PHINode* phi = dyn_cast<PHINode>(instI);
        if (! phi)
            break;

        phi->replaceAllUsesWith(phi->getIncomingValueForBlock(pred));
    }

    // Try to simplify all the instructions (especially since many phis
    // are now trivial)
    SimplifyInstructionsInBlock(toDuplicate);
    SimplifyInstructionsInBlock(clone);
    SimplifyInstructionsInBlock(merge);

    return clone;
}

} // end namespace gla_llvm

