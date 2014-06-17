//===- FlattenConditionalAssignments.cpp - Flatten conditional assignments ===//
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
// Flatten conditional assignments into select instructions.
//
// if (p)
//    v = foo;
// else
//    v = bar;
//
// Becomes something like:
//
// %v = select <ty>, i1 p, <ty> foo, <ty> bar
//
// This pass may modify the CFG. After all (or zero) conditional assignments
// have been removed, it will then see if the conditional is blank, and if so,
// remove it. Currently the flattening itself is unimplemented, just the code
// simplification and removal of empty conditionals
//
//===----------------------------------------------------------------------===//


#pragma warning(push, 1)
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Analysis/DominanceFrontier.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Local.h"
#pragma warning(pop)

#include "Passes/PassSupport.h"
#include "Passes/Analysis/IdentifyStructures.h"
#include "Passes/Util/BasicBlockUtil.h"
#include "Passes/Util/InstructionUtil.h"

using namespace llvm;
using namespace gla_llvm;

namespace  {

    // Whether an instruction shouldn't be hoisted for conditional
    // flattening, e.g. side-effects or texture calls.
    bool DontHoist(const Instruction& inst)
    {
        // TODO: Expand, possibly with backend queries
        return inst.mayWriteToMemory() || IsTextureInstruction(&inst);
    }

    class FlattenCondAssn : public FunctionPass {
    public:
        // Standard pass stuff
        static char ID;

        FlattenCondAssn() : FunctionPass(ID)
        {
            initializeFlattenCondAssnPass(*PassRegistry::getPassRegistry());
        }

        virtual bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;

    protected:
        IdentifyStructures* idStructures;
        DominatorTree* domTree;

        bool flattenConds();

        bool createSelects(Conditional* cond)
        {
            return cond->createMergeSelects();
        }

        bool simplifyAndRemoveDeadCode(Conditional* cond)
        {
            return cond->simplifyInsts();
        }

        bool removeEmptyConditional(Conditional* cond)
        {
            cond->recalculate();
            return cond->removeIfEmpty() || cond->removeIfUnconditional();
        }

        // Whether we should hoist instructions for this conditional, i.e. if
        // it's a simple conditional that's under the threshHold.
        bool shouldHoist(Conditional* c) const
        {
            if (! c->isSimpleConditional())
                return false;

            // TODO: soon: Make more robust, more intrinsic aware, and guided by
            // backend queries
            int threshHold = 20;

            return  checkHoistableInstructions(c) && checkThreshHold(c, threshHold);

        }

        // Hoist the instructions in the given (simple) conditional into the
        // entry block.
        void hoistInsts(Conditional*);

        // Whether the given conditional's then and else block sizes are
        // less than the threshHold
        bool checkThreshHold(Conditional* c, int threshHold) const
        {
            int thenSize = c->isIfElse() ? 0 : c->getThenBlock()->size() - 1;
            int elseSize = c->isIfThen() ? 0 : c->getElseBlock()->size() - 1;

            return threshHold > thenSize + elseSize;
        }

        // Whether the given conditional's then and else blocks' instructions
        // are hoistable or desirable to hoist
        bool checkHoistableInstructions(Conditional* c) const
        {
            bool thenOk = c->isIfElse() ? true : ! Any(c->getThenBlock()->getInstList(), DontHoist);
            bool elseOk = c->isIfThen() ? true : ! Any(c->getElseBlock()->getInstList(), DontHoist);

            return thenOk && elseOk;
        }

    };
} // end namespace

bool FlattenCondAssn::runOnFunction(Function& F)
{
    domTree = &getAnalysis<DominatorTree>();
    idStructures = &getAnalysis<IdentifyStructures>();

    bool changed = false;

    // TODO: Refactor this file to eliminate some of these loops
    while (flattenConds())
        changed = true;

    return changed;
}

void FlattenCondAssn::hoistInsts(Conditional* cond)
{
    // Hoist them
    if (! cond->isIfElse()) {
        Hoist(cond->getThenBlock(), cond->getEntryBlock());
    }
    if (! cond->isIfThen()) {
        Hoist(cond->getElseBlock(), cond->getEntryBlock());
    }

    cond->recalculate();
    assert(cond->isEmptyConditional() && "non-empty post-hoist");
}

bool FlattenCondAssn::flattenConds()
{
    bool changed = false;

    for (IdentifyStructures::conditional_iterator i = idStructures->conditional_begin(), e = idStructures->conditional_end(); i != e; ++i) {
        Conditional* cond = i->second;
        cond->recalculate();

        if (!cond || !cond->getMergeBlock())
            continue;

        const BasicBlock* entry = cond->getEntryBlock();

        // See if the then, else, and merge blocks are all dominated by the
        // entry. If this doesn't hold, we're not interested in it.
        if ( ! (   domTree->dominates(entry, cond->getThenBlock())
                && domTree->dominates(entry, cond->getElseBlock())
                && domTree->dominates(entry, cond->getMergeBlock()))) {
            continue;
        }

        // Check to see if we should hoist instructions, and if so then hoist
        // them
        if (shouldHoist(cond))
            hoistInsts(cond);

        // Move all the conditional assignments out, iteratively in case of some
        // interdependence
        while (createSelects(cond))
            changed = true;

        // Simplify/Eliminate instructions in the branches
        while (simplifyAndRemoveDeadCode(cond))
            changed = true;

        // Remove empty conditionals.
        if (removeEmptyConditional(cond)) {
            changed = true;
        }
    }

    return changed;
}

void FlattenCondAssn::getAnalysisUsage(AnalysisUsage& AU) const
{
    AU.addRequired<DominatorTree>();
    AU.addRequired<IdentifyStructures>();

    return;
}

void FlattenCondAssn::print(raw_ostream&, const Module*) const
{
    return;
}

char FlattenCondAssn::ID = 0;
INITIALIZE_PASS_BEGIN(FlattenCondAssn,
                      "flatten-conditional-assignments",
                      "Flatten conditional assignments into select instructions",
                      false,  // Whether it looks only at CFG
                      false); // Whether it is an analysis pass
INITIALIZE_PASS_DEPENDENCY(DominatorTree)
INITIALIZE_PASS_DEPENDENCY(IdentifyStructures)
INITIALIZE_PASS_END(FlattenCondAssn,
                    "flatten-conditional-assignments",
                    "Flatten conditional assignments into select instructions",
                    false,  // Whether it looks only at CFG
                    false); // Whether it is an analysis pass

FunctionPass* gla_llvm::createFlattenConditionalAssignmentsPass()
{
    return new FlattenCondAssn();
}
