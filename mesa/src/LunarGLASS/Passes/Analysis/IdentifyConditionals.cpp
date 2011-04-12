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

#include "LunarGLASSLlvmInterface.h"

using namespace llvm;

bool IdentifyConditionals::runOnFunction(Function &F)
{
    DominanceFrontier& domFront = getAnalysis<DominanceFrontier>();

    for (Function::iterator bb = F.begin(), e = F.end(); bb != e; ++bb) {

        // First, exclude all the non conditional branches
        const BranchInst* branchInst = dyn_cast<BranchInst>(bb->getTerminator());
        if (!branchInst)
            continue;
        if (branchInst->isUnconditional())
            continue;

        // If there's not a single merge point exclude this bb
        BasicBlock* merge = gla::Util::getSingleMergePoint(bb, domFront);
        if (!merge)
            continue;

        BasicBlock* left  = branchInst->getSuccessor(0);
        BasicBlock* right = branchInst->getSuccessor(1);

        std::pair<const BasicBlock*, const Conditional*> pair(bb, new Conditional(bb, merge, left, right, &domFront));

        conditionals.insert(pair);
    }

    return false;
}


void IdentifyConditionals::getAnalysisUsage(AnalysisUsage& AU) const
{
    AU.addRequired<DominanceFrontier>();
    AU.setPreservesAll();
    return;
}

void IdentifyConditionals::print(raw_ostream&, const Module*) const
{
    return;
}

void IdentifyConditionals::releaseMemory()
{
    for (DenseMap<const BasicBlock*, const Conditional*>::iterator i = conditionals.begin(), e = conditionals.end(); i != e; ++i) {
        delete i->second;
    }
    conditionals.clear();
}

// Whether from unconditionaly branches to to.
inline static bool uncondBranchesTo(BasicBlock* from, BasicBlock* to)
{
    BranchInst* bi = dyn_cast<BranchInst>(from->getTerminator());
    return bi && bi->isUnconditional() && (bi->getSuccessor(0) == to);
}

// Whether a basic block has no constituent instructions, other than
// it's phi-nodes and terminator.
inline static bool isEmptyBB(const llvm::BasicBlock* bb)
{
    return bb->getFirstNonPHIOrDbg() == bb->getTerminator();
}


bool Conditional::isEmptyConditional() const
{
    if (!isSelfContained())
        return false;

    // Todo: test the case when the then or else have underlying subgraphs

    bool isLeftEmpty  = (left  == merge) || (uncondBranchesTo(left, merge)  && isEmptyBB(left));
    bool isRightEmpty = (right == merge) || (uncondBranchesTo(right, merge) && isEmptyBB(right));

    return isLeftEmpty && isRightEmpty;
}

bool Conditional::isSelfContained() const
{
    DominanceFrontier::DomSetType leftDomFront  = domFront->find(left)->second;
    DominanceFrontier::DomSetType rightDomFront = domFront->find(right)->second;

    bool leftPure  = (left  == merge) || (leftDomFront.count(merge)  && leftDomFront.size() == 1);
    bool rightPure = (right == merge) || (rightDomFront.count(merge) && rightDomFront.size() == 1);

    return leftPure && rightPure;

}

char IdentifyConditionals::ID = 0;
INITIALIZE_PASS(IdentifyConditionals,
                "identify-conditionals",
                "Identify the conditional expressions",
                true,  // Whether it preserves the CFG
                true); // Whether it is an analysis pass
