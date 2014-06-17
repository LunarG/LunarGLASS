//===- IdentifyStructures.cpp - Identify structures in a structured-CFG ---===//
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
    loopInfo                       = &getAnalysis<LoopInfo>();
    DominanceFrontier& domFront    = getAnalysis<DominanceFrontier>();
    DominatorTree& domTree         = getAnalysis<DominatorTree>();
    PostDominatorTree& postDomTree = getAnalysis<PostDominatorTree>();
    ScalarEvolution& scalarEvo     = getAnalysis<ScalarEvolution>();

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

        // TODO: loops: consider extending functionality to include a LoopWrapper for
        // the loop that the conditional's entry block is in.
        conditionals.insert(std::make_pair(bb, new Conditional(bb, left, right, domFront, domTree, postDomTree,
                                                               *loopInfo, this)));
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


        LoopWrapper* lw = loop ? new LoopWrapper(loop, &domFront, &scalarEvo, simpleLatch) : 0;
        loopWrappers.insert(make_pair(bb, lw));
    }

    // Ask scalarEvo to release its block references.  Other
    // transforms might delete the blocks, and we don't want any
    // outstanding asserting value handles.
    scalarEvo.releaseMemory();
    
    return false;
}

void IdentifyStructures::getAnalysisUsage(AnalysisUsage& AU) const
{
    AU.addRequired<DominanceFrontier>();
    AU.addRequired<DominatorTree>();
    AU.addRequired<PostDominatorTree>();
    AU.addRequired<LoopInfo>();
    AU.addRequired<ScalarEvolution>();
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
    releaseMemory();
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
INITIALIZE_PASS_DEPENDENCY(PostDominatorTree)
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolution)
INITIALIZE_PASS_END(IdentifyStructures,
                    "identify-structures",
                    "Identify the structures in a structured-cfg",
                    false,  // Whether it looks only at CFG
                    true);  // Whether it is an analysis pass

