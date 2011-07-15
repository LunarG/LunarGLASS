//===- CanonicalizeCFG.cpp - Canonicalize the CFG for LunarGLASS ----------===//
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
//===----------------------------------------------------------------------===//

#include "llvm/Pass.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/DominanceFrontier.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Local.h"

#include "Passes/PassSupport.h"
#include "Passes/Util/BasicBlockUtil.h"
#include "Passes/Util/FunctionUtil.h"
#include "Passes/Util/InstructionUtil.h"

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
        bool removeNoPredecessorBlocks(Function& F);

        bool removeUnneededPHIs(Function& F);

        bool restoreMainBlocks(Function& F);

        LoopInfo* loopInfo;
        DominatorTree* domTree;

        BasicBlock* exit;
        BasicBlock* epilogue;

    };
} // end namespace

bool CanonicalizeCFG::runOnFunction(Function& F)
{
    bool changed = false;

    loopInfo = &getAnalysis<LoopInfo>();
    domTree  = &getAnalysis<DominatorTree>();

    // TODO: combine the removing of no-predecessor blocks with the removing of
    // phis into a single traversal

    changed |= removeNoPredecessorBlocks(F);

    // TODO: do it in one pass
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

bool CanonicalizeCFG::restoreMainBlocks(Function& F)
{
    assert(CountReturnBlocks(F) == 1 && "main function does not have exactly 1 return block");

    // Try to get existing exit and epilogue. If they exist, no need to restore.
    exit     = GetMainExit(F);
    epilogue = GetMainEpilogue(F);
    if (exit && epilogue)
        return false;

    BasicBlock* ret = GetReturnBlock(F);

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
        bool hasOutput = false;
        for (pred_iterator bbI = pred_begin(exit), e = pred_end(exit); bbI != e; ++bbI) {
            BasicBlock* bb = *bbI;
            for (BasicBlock::iterator instI = bb->begin(), e = bb->end(); instI != e; ++instI) {
                if (IsOutputInstruction(instI)) {
                    hasOutput = true;
                    break;
                }
            }

            if (! hasOutput) {
                continue;
            }

            BasicBlock::iterator term = bb->end();
            --term;

            bb->splitBasicBlock(term, "stage-epilogue");
            epilogue = GetMainEpilogue(F);
            assert(epilogue);
            break;
        }
        assert(hasOutput && "no FWriteData in shader (as a predecessor to the exit)");
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

        // TODO: do it in one pass, perhaps by just having a set/vector of all
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
    AU.addRequired<LoopInfo>();
    AU.addRequired<DominatorTree>();
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
INITIALIZE_PASS_END(CanonicalizeCFG,
                    "canonicalize-cfg",
                    "Canonicalize the CFG for LunarGLASS",
                    false,  // Whether it looks only at CFG
                    false); // Whether it is an analysis pass


FunctionPass* gla_llvm::createCanonicalizeCFGPass()
{
    return new CanonicalizeCFG();
}
