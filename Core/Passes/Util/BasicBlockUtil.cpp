//===- BasicBlockUtil.cpp - Utility functions for basic blocks ------------===//
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

#include "llvm/IR/IRBuilder.h"
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

    // Break off a predecessor if one exists (it may have none), and
    // have it go through the clone instead
    if (pred_begin(toDuplicate) != pred_end(toDuplicate)) {
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
    }

    // Try to simplify all the instructions (especially since many phis
    // are now trivial)
    SimplifyInstructionsInBlock(toDuplicate);
    SimplifyInstructionsInBlock(clone);
    SimplifyInstructionsInBlock(merge);

    return clone;
}

} // end namespace gla_llvm

