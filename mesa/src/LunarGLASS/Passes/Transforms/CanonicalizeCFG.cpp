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
//   * All single predecessor/single successor sequences of basic blocks are
//     condensed into one block. Currently unimplemented.
//
//   * Pointless phi nodes are removed (invalidating LCSSA).
//
//===----------------------------------------------------------------------===//

#include "llvm/Pass.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Local.h"

#include "Transforms.h"
#include "Util.h"

#include "Passes/Util/BasicBlockUtil.h"

using namespace llvm;
using namespace gla_llvm;

namespace  {
    class CanonicalizeCFG : public FunctionPass {
    public:
        // Standard pass stuff
        static char ID;
        CanonicalizeCFG() : FunctionPass(ID) {}
        virtual bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;

    protected:
        bool removeNoPredecessorBlocks(Function& F);

        bool removeUnneededPHIs(Function& F);

        LoopInfo* loopInfo;
        DominatorTree* domTree;

    };
} // end namespace

bool CanonicalizeCFG::runOnFunction(Function& F)
{
    bool changed = false;

    loopInfo = &getAnalysis<LoopInfo>();
    domTree  = &getAnalysis<DominatorTree>();

    changed |= removeNoPredecessorBlocks(F);

    // TODO: do it in one pass
    while (removeUnneededPHIs(F)) {
        changed = true;
    }

    return changed;
}

bool CanonicalizeCFG::removeUnneededPHIs(Function& F)
{
    SmallVector<PHINode*, 64> deadPHIs;
    for (Function::iterator bbI = F.begin(), bbE = F.end(); bbI != bbE; ++bbI) {
        for (BasicBlock::iterator instI = bbI->begin(), instE = bbI->end(); instI != instE; ++instI) {
            PHINode* pn = dyn_cast<PHINode>(instI);
            if (!pn)
                break;

            Value* v = pn->hasConstantValue(domTree);
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

        // Removing it seems to invalidate the iterator, so start over
        // again. Note that the for-loop incrementation is ok, as all functions
        // have an entry block and we don't mind incrementing past it right
        // away.
        // TODO: do it in one pass, perhaps by just having a set/vector of all
        // the blocks in the function
        if (RemoveNoPredecessorBlock(bbI)) {
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
INITIALIZE_PASS(CanonicalizeCFG,
                "canonicalize-cfg",
                "Canonicalize the CFG for LunarGLASS",
                false,  // Whether it preserves the CFG
                false); // Whether it is an analysis pass
FunctionPass* gla_llvm::createCanonicalizeCFGPass()
{
    return new CanonicalizeCFG();
}
