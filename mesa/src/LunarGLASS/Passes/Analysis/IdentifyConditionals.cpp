//===- IdentifyCondtionals.cpp - Identify the structural conditionals -----===//
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
// Identify the structural conditionals, and collect information about them,
// including their classification (e.g. if they're an if-then-else), and their
// merge points.
//
//===----------------------------------------------------------------------===//

#include "IdentifyConditionals.h"

#include "llvm/ADT/DenseMap.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Support/raw_ostream.h"

#include "Util.h"

#include "Passes/Util/BasicBlockUtil.h"

using namespace llvm;
using namespace gla_llvm;

bool IdentifyConditionals::runOnFunction(Function &F)
{
    DominanceFrontier& domFront = getAnalysis<DominanceFrontier>();
    DominatorTree& domTree = getAnalysis<DominatorTree>();

    for (Function::iterator bb = F.begin(), e = F.end(); bb != e; ++bb) {

        // First, exclude all the non conditional branches
        const BranchInst* branchInst = dyn_cast<BranchInst>(bb->getTerminator());
        if (!branchInst)
            continue;
        if (branchInst->isUnconditional())
            continue;

        BasicBlock* left  = branchInst->getSuccessor(0);
        BasicBlock* right = branchInst->getSuccessor(1);

        std::pair<const BasicBlock*, Conditional*> pair(bb, new Conditional(bb, left, right, domFront, domTree));

        conditionals.insert(pair);
    }

    return false;
}


void IdentifyConditionals::getAnalysisUsage(AnalysisUsage& AU) const
{
    AU.addRequired<DominanceFrontier>();
    AU.addRequired<DominatorTree>();
    AU.setPreservesAll();
    return;
}

void IdentifyConditionals::print(raw_ostream&, const Module*) const
{
    return;
}

void IdentifyConditionals::releaseMemory()
{
    for (DenseMap<const BasicBlock*, Conditional*>::iterator i = conditionals.begin(), e = conditionals.end(); i != e; ++i) {
        delete i->second;
    }
    conditionals.clear();
}

char IdentifyConditionals::ID = 0;
INITIALIZE_PASS(IdentifyConditionals,
                "identify-conditionals",
                "Identify the conditional expressions",
                true,  // Whether it preserves the CFG
                true); // Whether it is an analysis pass
