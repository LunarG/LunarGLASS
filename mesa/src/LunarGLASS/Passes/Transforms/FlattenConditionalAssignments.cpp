//===- FlattenConditionalAssignments.cpp - Flatten conditional assignments ===//
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


#include "llvm/BasicBlock.h"
#include "llvm/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Analysis/DominanceFrontier.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Local.h"

#include "Passes/PassSupport.h"
#include "Passes/Analysis/IdentifyConditionals.h"
#include "Passes/Util/BasicBlockUtil.h"

using namespace llvm;
using namespace gla_llvm;

namespace  {
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
        IdentifyConditionals* idConds;
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
    };
} // end namespace

bool FlattenCondAssn::runOnFunction(Function& F)
{
    domTree = &getAnalysis<DominatorTree>();
    idConds = &getAnalysis<IdentifyConditionals>();

    bool changed = false;

    // TODO: Refactor this file to eliminate some of these loops
    while (flattenConds())
        changed = true;

    return changed;
}

bool FlattenCondAssn::flattenConds()
{
    bool changed = false;

    for (IdentifyConditionals::iterator i = idConds->begin(), e = idConds->end(); i != e; ++i) {
        Conditional* cond = i->second;

        if (!cond)
            continue;

        const BasicBlock* entry = cond->getEntryBlock();

        // See if the then, else, and merge blocks are all dominated by the
        // entry. If this doesn't hold, we're not interested in it.
        if ( ! (   domTree->dominates(entry, cond->getThenBlock())
                && domTree->dominates(entry, cond->getThenBlock())
                && domTree->dominates(entry, cond->getMergeBlock())))
            continue;

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
            idConds->nullConditional(entry);
        };
    }

    return changed;
}

void FlattenCondAssn::getAnalysisUsage(AnalysisUsage& AU) const
{
    AU.addRequired<DominatorTree>();
    AU.addRequired<IdentifyConditionals>();

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
                      true,   // Whether it preserves the CFG
                      false); // Whether it is an analysis pass
INITIALIZE_PASS_DEPENDENCY(DominatorTree)
INITIALIZE_PASS_DEPENDENCY(IdentifyConditionals)
INITIALIZE_PASS_END(FlattenCondAssn,
                    "flatten-conditional-assignments",
                    "Flatten conditional assignments into select instructions",
                    true,   // Whether it preserves the CFG
                    false); // Whether it is an analysis pass

FunctionPass* gla_llvm::createFlattenConditionalAssignmentsPass()
{
    return new FlattenCondAssn();
}
