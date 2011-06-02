//===- IdentifyStructures.cpp - Identify structures in a structured-CFG ---===//
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
// Identify structures in a structured-CFG. Possible structures and their
// properties include:
//
//   * Conditional expressions are identified and a Conditional (from
//     ConditionalUtil.h) is created fo reach one. A mapping from entry blocks
//     to Conditional is provided. Future work is to provide a conditional tree,
//     such that a change to one can call the recalculate methods of it's
//     parents.
//
//   * Loops are identified and a LoopWrapper (from LoopUtil.h) is created for
//     each one. A mapping between basic blocks and the innermost LoopWrapper is
//     provided.
//
//   * Copy-out and Function exit blocks are identified for when the CFG belongs
//     the function "main".
//
//===----------------------------------------------------------------------===//

#include "Passes/Analysis/IdentifyStructures.h"
#include "Passes/Util/FunctionUtil.h"

using namespace gla_llvm;
using namespace llvm;

bool IdentifyStructures::runOnFunction(Function &F)
{
    loopInfo                    = &getAnalysis<LoopInfo>();
    DominanceFrontier& domFront = getAnalysis<DominanceFrontier>();
    DominatorTree& domTree      = getAnalysis<DominatorTree>();

    // Set up stageExit and mainCopyOut
    stageExit   = GetMainExit(F);
    mainCopyOut = GetMainEpilogue(F);

    // Identify conditionals
    for (Function::iterator bb = F.begin(), e = F.end(); bb != e; ++bb) {
        // Identify the conditionals

        // Create and map the conditional

        // First, exclude all the non conditional branches
        const BranchInst* branchInst = dyn_cast<BranchInst>(bb->getTerminator());
        if (!branchInst)
            continue;
        if (branchInst->isUnconditional())
            continue;

        BasicBlock* left  = branchInst->getSuccessor(0);
        BasicBlock* right = branchInst->getSuccessor(1);

        Loop* loop = loopInfo->getLoopFor(bb);

        // TODO: consider extending functionality to include a LoopWrapper for
        // the loop that the conditional's entry block is in.
        conditionals.insert(std::make_pair(bb, new Conditional(bb, left, right, domFront, domTree, loop)));
    }

    // Identify and create loopwrappers
    for (Function::iterator bb = F.begin(), e = F.end(); bb != e; ++bb) {
        // Create and map the LoopWrappers
        Loop* loop = loopInfo->getLoopFor(bb);
        if (! loop || loop->getHeader() != bb)
            continue;

        BasicBlock* latch = loop->getLoopLatch();

        // A simple latch block means that the loop can be represented without
        // continues, and eases analysis. The backedge block, to be simple, much
        // either be exiting, or have a single predecessor, or else it has
        // exactly two predecessors and is the sole merge block for the
        // conditional whose entry block is the latch's immediate dominator. Any
        // latch that is preserved from the original source (that is,
        // loop-simplify didn't need to create a new one), is always simple.
        bool simpleLatch = IsConditional(latch)
                        || latch->getSinglePredecessor()
                        || latch == getConditional(domTree.getNode(latch)->getIDom()->getBlock())->getMergeBlock();


        LoopWrapper* lw = loop ? new LoopWrapper(loop, &domFront, simpleLatch) : 0;
        loopWrappers.insert(make_pair(bb, lw));
    }

    return false;
}

void IdentifyStructures::getAnalysisUsage(AnalysisUsage& AU) const
{
    AU.addRequired<DominanceFrontier>();
    AU.addRequired<DominatorTree>();
    AU.addRequired<LoopInfo>();
    AU.setPreservesAll();
    return;
}

void IdentifyStructures::print(raw_ostream&, const Module*) const
{
    return;
}

void IdentifyStructures::releaseMemory()
{
    for (DenseMap<const BasicBlock*, Conditional*>::iterator i = conditionals.begin(), e = conditionals.end(); i != e; ++i) {
        delete i->second;
    }
    conditionals.clear();

    for (DenseMap<const BasicBlock*, LoopWrapper*>::iterator i = loopWrappers.begin(), e = loopWrappers.end(); i != e; ++i) {
        delete i->second;
    }
    loopWrappers.clear();
}

IdentifyStructures::~IdentifyStructures()
{
    assert(conditionals.empty());
    assert(loopWrappers.empty());
}

char IdentifyStructures::ID = 0;
INITIALIZE_PASS_BEGIN(IdentifyStructures,
                      "identify-structures",
                      "Identify the structures in a structured-cfg",
                      false,  // Whether it looks only at CFG
                      true); // Whether it is an analysis pass
INITIALIZE_PASS_DEPENDENCY(DominanceFrontier)
INITIALIZE_PASS_DEPENDENCY(DominatorTree)
INITIALIZE_PASS_END(IdentifyStructures,
                    "identify-structures",
                    "Identify the structures in a structured-cfg",
                    false,  // Whether it looks only at CFG
                    true);  // Whether it is an analysis pass

