//===- EarlyReturnElim.cpp - Canonicalize the CFG for LunarGLASS ----------===//
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
// Author:  Greg Fischer, LunarG
//
//===----------------------------------------------------------------------===//
//
// Eliminate Early Returns for LunarGLASS. Specifically, convert
// early returns into equivalent control flow in non-main functions.
// This is done so that the control flow in non-main functions is reducible. 
// This allows the control flow to be converted into structured 
// control flow constructs. It also makes subsequent analysis and 
// optimizations more efficient and effective.
//
// Assumes that basic blocks are ordered such that if B1 dominates B2,
// B1 precedes B2, and if B2 post-dominates B1, B1 precedes B2. Also,
// if B1 dominates B2, it also dominates all blocks between B1 and B2, and
// if B2 post-dominates B1, it post-dominates all blocks between B2 and B1.
//
// TODO: Handle early returns in loops and functions with switches
//
//===----------------------------------------------------------------------===//

#pragma warning(push, 1)
#include "llvm/Pass.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Support/CFG.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Local.h"
#pragma warning(pop)

#include "Passes/PassSupport.h"
#include "Passes/Analysis/IdentifyStructures.h"
#include "Passes/Util/BasicBlockUtil.h"
#include "Passes/Util/FunctionUtil.h"
#include "Passes/Util/InstructionUtil.h"

#include "Exceptions.h"

using namespace llvm;
using namespace gla_llvm;

namespace  {
    class EarlyReturnElim : public FunctionPass {
    public:
        // Standard pass stuff
        static char ID;

        EarlyReturnElim() : FunctionPass(ID)
        {
            initializeEarlyReturnElimPass(*PassRegistry::getPassRegistry());
        }

        virtual bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;

    protected:
        LoopInfo* loopInfo;
        DominatorTree* domTree;
        IdentifyStructures* idStructs;

        BasicBlock* exit;
        BasicBlock* epilogue;

    };
} // end namespace

bool EarlyReturnElim::runOnFunction(Function& F)
{
    if (IsMain(F)) 
        return false;

    bool changed = false;

    loopInfo  = &getAnalysis<LoopInfo>();
    domTree   = &getAnalysis<DominatorTree>();
    idStructs = &getAnalysis<IdentifyStructures>();

    BasicBlock *finalbb = NULL;
    AllocaInst *retBool = 0;
    AllocaInst *retVal = 0;

    // Process blocks in reverse order
    for (Function::BasicBlockListType::reverse_iterator bbI = F.getBasicBlockList().rbegin(), e = F.getBasicBlockList().rend(); bbI != e; /* empty */ ) {

        BasicBlock *obb = &*bbI++;
        ReturnInst *retInst = dyn_cast<ReturnInst>(obb->getTerminator());
        if (! retInst)
	    continue;

        // Remember final return block, but do not process it unless early
        // returns are found
        if (! finalbb) {
            finalbb = obb;
            continue;
        }

        // TODO: For now, skip returns inside loops
        Loop *L = loopInfo->getLoopFor(obb);
        if (L)
            continue;
        
        changed = true;

        // Early return found. If not already done, create temps on stack to 
        // remember return state and value
        if (! retBool) {
            BasicBlock::iterator insertPt = F.getEntryBlock().begin();
            retBool = new AllocaInst(Type::getInt1Ty(F.getContext()), 0, 
                                     "earlyretbool",
                                     insertPt);
            (void) new StoreInst(ConstantInt::getFalse(F.getContext()), 
                                     retBool, insertPt);

            if (retInst->getNumOperands() > 0) {
                Value *retOpnd = retInst->getOperand(0);
                retVal = new AllocaInst(retOpnd->getType(), 0, 
                                         "earlyretval",
                                         insertPt);

                // Split final block into two: store of return value in first
                // block and actual return in second.
                Instruction *finalRet = finalbb->getTerminator();
                Value *finalRetOpnd = finalRet->getOperand(0);
                BasicBlock *nbb = finalbb->splitBasicBlock(
                                   finalRet,
                                   "return.split");
                (void) new StoreInst(finalRetOpnd, retVal, finalbb->getTerminator());
                LoadInst *loadRetVal = new LoadInst(retVal, 
                                                    "earlyretval.load", 
                                                    false, finalRet);
                (void) ReturnInst::Create(F.getContext(), loadRetVal, finalRet);
                finalRet->eraseFromParent();
                finalbb = nbb;
            } else {
                // split return into its own block
                finalbb = finalbb->splitBasicBlock(finalbb->getTerminator(),
                                   "return.split");
            }
        }
        BasicBlock *pbb = obb;

        // If the early return is in a loop, set the early return variables, 
        // turn the exit into a break and process the loop's break block. Add
        // conditional break on early return boolean and continue to process 
        // break blocks while inside a loop.
        // TODO: add code to handle early return inside loop
        if (L) {
            gla::UnsupportedFunctionality("early return in loop");
        }

        // Process blocks until we have iterated out of all containing
        // conditionals and are at final block.
        while (pbb != finalbb) {
            BasicBlock *mbb = 0;
            if (pbb == obb) {
                // process original return block if not processed yet
                // set early return variables and branch past controlling
                // conditional's exit block or branch to final return block
                if (retInst->getNumOperands() > 0) {
                    Value *retOpnd = retInst->getOperand(0);
                    (void) new StoreInst(retOpnd, retVal, retInst);
                }
                (void) new StoreInst(ConstantInt::getTrue(F.getContext()), 
                                     retBool, retInst);
                Conditional *cond = 0;
                for (Function::BasicBlockListType::reverse_iterator bbI2 = bbI; bbI2 != e; bbI2++ ) {
                    BasicBlock *cbb = &*bbI2;
                    cond = idStructs->getConditional(cbb);
                    if (!cond)
                        continue;
                    if (cond->hasChild(pbb))
                        break;
                }

                BasicBlock *splitbb = cond->getExitBlock();
                if (splitbb) {
                    // skip past any blocks added during this transform
                    // ie whose successor has one predecessor
                    BasicBlock *succbb = splitbb->getTerminator()->getSuccessor(0);
                    pred_iterator PI = pred_begin(succbb);
                    PI++;
                    while (PI == pred_end(succbb)) {
                        splitbb = succbb;
                        succbb = splitbb->getTerminator()->getSuccessor(0);
                        PI = pred_begin(succbb);
                        PI++;
                    }

                    mbb = splitbb->splitBasicBlock(splitbb->getTerminator(), 
                                               splitbb->getName() + ".split");
                } else {
                    mbb = finalbb;
                }
                retInst->eraseFromParent();
                (void) BranchInst::Create(mbb, pbb);
            } else {
                // process conditional merge block
                // add conditional branch on early return boolean past
                // controlling conditional's exit block or to return block

                BasicBlock::InstListType::iterator I = pbb->getInstList().begin();
                LoadInst *pld = dyn_cast<LoadInst>(&*I);
                if (pld && dyn_cast<AllocaInst>(pld->getOperand(0)) == retBool)
                    // If the first instruction is a load of the return
                    // boolean, then this path has already been processed
                    // and we are done processing this return.
                    break;

                BasicBlock *fbb = pbb->splitBasicBlock(pbb->begin(),
                                       pbb->getName()+".split");
                Conditional *cond = 0;
                for (Function::BasicBlockListType::reverse_iterator bbI2 = bbI; bbI2 != e; bbI2++ ) {
                    BasicBlock *cbb = &*bbI2;
                    cond = idStructs->getConditional(cbb);
                    if (!cond)
                        continue;
                    if (cond->hasChild(pbb))
                        break;
                    cond = 0;
                }

                BasicBlock *sbb = cond ? cond->getExitBlock() : 0;
                if (sbb) {
                    mbb = sbb->splitBasicBlock(sbb->getTerminator(),
                                       sbb->getName()+".split");
                } else {
                    mbb = finalbb;
                }
                LoadInst *loadRetVal = new LoadInst(retBool, 
                                                    pbb->getName()+".loadret", 
                                                    false, 
                                                    pbb->getTerminator());
                (void) BranchInst::Create(mbb, fbb, loadRetVal, pbb->getTerminator());
                pbb->getTerminator()->eraseFromParent();
            }

            // process next conditional merge block if not yet at final return
            pbb = mbb;
            if (pbb != finalbb)
                pbb = pbb->getTerminator()->getSuccessor(0);
        }
    }

    return changed;
}

void EarlyReturnElim::getAnalysisUsage(AnalysisUsage& AU) const
{
    // TODO: Preservation analysis info (e.g. using BasicBlockUtils.h's methods)

    AU.addRequired<LoopInfo>();
    AU.addRequired<DominatorTree>();
    AU.addRequired<IdentifyStructures>();
}

void EarlyReturnElim::print(raw_ostream&, const Module*) const
{
    return;
}

char EarlyReturnElim::ID = 0;
INITIALIZE_PASS_BEGIN(EarlyReturnElim,
                      "early-return-elim",
                      "Eliminate Early Returns for LunarGLASS",
                      false,  // Whether it looks only at CFG
                      false); // Whether it is an analysis pass
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_DEPENDENCY(DominatorTree)
INITIALIZE_PASS_DEPENDENCY(IdentifyStructures)
INITIALIZE_PASS_END(EarlyReturnElim,
                      "early-return-elim",
                      "Eliminate Early Returns for LunarGLASS",
                      false,  // Whether it looks only at CFG
                      false); // Whether it is an analysis pass


FunctionPass* gla_llvm::createEarlyReturnElimPass()
{
    return new EarlyReturnElim();
}
