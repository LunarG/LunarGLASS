//===- CanonicalizeCFG.cpp - Canonicalize the CFG for LunarGLASS ----------===//
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
//===----------------------------------------------------------------------===//
//
// Canonicalize the CFG for LunarGLASS, this includes the following:
//   * All basic blocks without predecessors are removed.
//
//   * Restore stage-exit and stage-epilogue blocks (GVN combines them if it
//     can).
//
//   * Pointless phi nodes are removed (invalidating LCSSA).
//
//   * Merge unconditional branch chains
//
//   * Split shared coditional merge blocks
//
//===----------------------------------------------------------------------===//

#pragma warning(push, 1)
#include "llvm/Pass.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/DominanceFrontier.h"
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
    class CanonicalizeCFG : public FunctionPass {
    public:
        // Standard pass stuff
        static char ID;

        CanonicalizeCFG() : FunctionPass(ID)
        {
            initializeCanonicalizeCFGPass(*PassRegistry::getPassRegistry());
        }

        virtual bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;

    protected:
        bool convertConstantBranches(Function& F);

        bool eliminateCrossEdges(Function& F);

        bool removeNoPredecessorBlocks(Function& F);

        bool removeUnneededPHIs(Function& F);

        bool restoreMainBlocks(Function& F);

        bool splitSharedMergeBlocks(Function& F);

        LoopInfo* loopInfo;
        DominatorTree* domTree;
        IdentifyStructures* idStructs;

        BasicBlock* exit;
        BasicBlock* epilogue;

    };
} // end namespace

bool CanonicalizeCFG::runOnFunction(Function& F)
{
    bool changed = false;

    loopInfo  = &getAnalysis<LoopInfo>();
    domTree   = &getAnalysis<DominatorTree>();
    idStructs = &getAnalysis<IdentifyStructures>();

    changed |= convertConstantBranches(F);

    // Try to eliminate cross-edges
    changed |= eliminateCrossEdges(F);

    // Split shared merge blocks in conditionals. Shared merge blocks can arise
    // from the elimination of loops that originally contained breaks.
    changed |= splitSharedMergeBlocks(F);

    // TODO: compile-time performance: combine the removing of no-predecessor blocks with the removing of
    // phis into a single traversal

    changed |= removeNoPredecessorBlocks(F);

    // TODO: compile-time performance: do it in one pass
    while (removeUnneededPHIs(F)) {
        changed = true;
    }

    // Merge unconditional branch chains
    for (Function::iterator bbI = F.begin(), e = F.end(); bbI != e; /* empty */ ) {
        BasicBlock *bb = bbI++;
        changed |= MergeBlockIntoPredecessor(bb);
    }

    if (IsMain(F)) {
        changed |= restoreMainBlocks(F);
    }

    return changed;
}

bool CanonicalizeCFG::eliminateCrossEdges(Function& F)
{
    bool changed = false;

    for (IdentifyStructures::conditional_iterator i = idStructs->conditional_begin(),
             e = idStructs->conditional_end(); i != e; ++i) {
        changed |= (*i).second->eliminateCrossEdges();
    }

    // Check to see if there are any remaining cross-edges
    for (IdentifyStructures::conditional_iterator i = idStructs->conditional_begin(),
             e = idStructs->conditional_end(); i != e; ++i) {
        if ((*i).second->hasPotentialCrossEdge()) {
            gla::UnsupportedFunctionality("general-case cross edges (requires artificial loop)");
        }
    }

    return changed;
}


bool CanonicalizeCFG::splitSharedMergeBlocks(Function& F)
{
    bool changed = false;

    // Loop over the conditionals, and split the merge block if shared. Redirect
    // all the predecessors dominated by the entry to the new merge block. Note
    // that this can traverse the conditionals in any order, i.e. outermost-
    // inward, inwardmost-outer, or any combination, and .

    for (IdentifyStructures::conditional_iterator i = idStructs->conditional_begin(),
             e = idStructs->conditional_end(); i != e; ++i) {
        changed |= (*i).second->splitSharedMerge();
    }


    return changed;
}

bool CanonicalizeCFG::convertConstantBranches(Function& F)
{
    bool changed = false;

    for (Function::iterator bbI = F.begin(), bbE = F.end(); bbI != bbE; ++bbI) {
        changed |= ConstantFoldTerminator(bbI);
    }

    return changed;
}

bool CanonicalizeCFG::restoreMainBlocks(Function& F)
{
    assert(CountReturnBlocks(F) == 1 && "main function does not have exactly 1 return block");

    // Try to get existing exit and epilogue. If they exist, no need to restore.
    exit     = GetMainExit(F);
    epilogue = GetMainEpilogue(F);
    if (exit && epilogue)
        return false;

    BasicBlock* ret = GetReturnBlock(F);
    bool bothRestored = ! (exit || epilogue);

    // Create the exit if it's missing, and sink the ret into it
    if (! exit) {
        // Get an interator to the terminator, and split the block based on it
        BasicBlock::iterator term = ret->end();
        --term;

        ret->splitBasicBlock(term, "stage-exit");
        exit = GetMainExit(F);
        assert(exit);
    }

    // If there's no existing epilogue, then there was no early returns. One and
    // only one of the exit block's predecessors should be the effective
    // epilogue, so split it.
    if (! epilogue) {
        // If we're restoring both, we can merely split the original return
        // block yet again, otherwise we have to actually search for the
        // non-discarding predecessor of exit
        if (bothRestored) {
            BasicBlock::iterator term = ret->end();
            --term;
            ret->splitBasicBlock(term, "stage-epilogue");
            epilogue = GetMainEpilogue(F);
            assert(epilogue);
        } else {
            for (pred_iterator bbI = pred_begin(exit), e = pred_end(exit); bbI != e; ++bbI) {
                BasicBlock* bb = *bbI;
                if (HasDiscard(bb)) {
                    continue;
                }

                BasicBlock::iterator term = bb->end();
                --term;

                bb->splitBasicBlock(term, "stage-epilogue");
                epilogue = GetMainEpilogue(F);
                assert(epilogue);
                break;
            }
            assert(epilogue && "no non-discarding shader exit path");
        }
    }

    return true;
}

bool CanonicalizeCFG::removeUnneededPHIs(Function& F)
{
    SmallVector<PHINode*, 64> deadPHIs;
    for (Function::iterator bbI = F.begin(), bbE = F.end(); bbI != bbE; ++bbI) {
        for (BasicBlock::iterator instI = bbI->begin(), instE = bbI->end(); instI != instE; ++instI) {
            PHINode* pn = dyn_cast<PHINode>(instI);
            if (!pn)
                break;

            Value* v = pn->hasConstantValue();
            if (!v)
                continue;

            pn->replaceAllUsesWith(v);

            // Remove it
            deadPHIs.push_back(pn);
        }
    }

    for (SmallVector<PHINode*, 64>::iterator i = deadPHIs.begin(), e = deadPHIs.end(); i != e; ++i) {
        (*i)->eraseFromParent();
    }

    return ! deadPHIs.empty();
}

bool CanonicalizeCFG::removeNoPredecessorBlocks(Function& F)
{
    bool changed = false;

    for (Function::iterator bbI = F.begin(), bbE = F.end(); bbI != bbE; ++bbI) {

        // Removing blocks seems to invalidate the iterator, so start over
        // again. Note that the for-loop incrementation is ok, as all functions
        // have an entry block and we don't mind incrementing past it right
        // away.

        // TODO: compile-time performance: do it in one pass, perhaps by just having a set/vector of all
        // the blocks in the function. Currently O(n*m) where m is the number of
        // no-predecessor subgraphs.
        if (RecursivelyRemoveNoPredecessorBlocks(bbI, domTree)) {
            changed = true;
            bbI = F.begin();
        }
    }

    return changed;
}

void CanonicalizeCFG::getAnalysisUsage(AnalysisUsage& AU) const
{
    // TODO: Preservation analysis info (e.g. using BasicBlockUtils.h's methods)

    AU.addRequired<LoopInfo>();
    AU.addRequired<DominatorTree>();
    AU.addRequired<IdentifyStructures>();
}

void CanonicalizeCFG::print(raw_ostream&, const Module*) const
{
    return;
}

char CanonicalizeCFG::ID = 0;
INITIALIZE_PASS_BEGIN(CanonicalizeCFG,
                      "canonicalize-cfg",
                      "Canonicalize the CFG for LunarGLASS",
                      false,  // Whether it looks only at CFG
                      false); // Whether it is an analysis pass
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_DEPENDENCY(DominatorTree)
INITIALIZE_PASS_DEPENDENCY(IdentifyStructures)
INITIALIZE_PASS_END(CanonicalizeCFG,
                    "canonicalize-cfg",
                    "Canonicalize the CFG for LunarGLASS",
                    false,  // Whether it looks only at CFG
                    false); // Whether it is an analysis pass


FunctionPass* gla_llvm::createCanonicalizeCFGPass()
{
    return new CanonicalizeCFG();
}
