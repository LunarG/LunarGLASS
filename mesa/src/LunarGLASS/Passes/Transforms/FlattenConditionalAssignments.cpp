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
// remove it.
//
//===----------------------------------------------------------------------===//


#include "llvm/BasicBlock.h"
#include "llvm/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Local.h"

#include "LunarGLASSLlvmInterface.h"
#include "Transforms.h"

#include "Passes/Analysis/IdentifyConditionals.h"

using namespace llvm;
using namespace gla_llvm;


namespace  {
    class FlattenCondAssn : public FunctionPass {
    public:
        // Standard pass stuff
        static char ID;
        FlattenCondAssn() : FunctionPass(ID) {}
        virtual bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;

    protected:
        IdentifyConditionals* idConds;
        DominatorTree* domTree;

        bool moveCondAssns(const Conditional*);
        bool removeDeadCode(const Conditional*);
        bool removeEmptyConditional(const Conditional*);

        bool flattenConds();
    };
} // end namespace

bool FlattenCondAssn::runOnFunction(Function& F)
{
    domTree = &getAnalysis<DominatorTree>();
    idConds = &getAnalysis<IdentifyConditionals>();

    bool changed = false;

    while (flattenConds())
        changed = true;

    return changed;
}

bool FlattenCondAssn::flattenConds()
{
    bool changed = false;

    for (IdentifyConditionals::const_iterator i = idConds->begin(), e = idConds->end(); i != e; ++i) {
        const Conditional* cond = i->second;

        if (!cond)
            continue;

        BasicBlock* entry = cond->getEntryBlock();

        // See if the then, else, and merge blocks are all dominated by the
        // entry. If this doesn't hold, we're not interested in it.
        if ( ! (   domTree->dominates(entry, cond->getThenBlock())
                && domTree->dominates(entry, cond->getThenBlock())
                && domTree->dominates(entry, cond->getMergeBlock())))
            continue;

        // // Move all the conditional assignments out, iteratively in case of some
        // // interdependence
        // while (moveCondAssns(cond))
        //     changed = true;

        // Eliminate any and all remaining dead code in the branches
        while (removeDeadCode(cond))
            changed = true;

        // Remove empty conditionals.
        if (removeEmptyConditional(cond)) {
            changed = true;
            idConds->nullConditional(entry);
        };
    }

    return changed;
}

// bool FlattenCondAssn::moveCondAssns(const Conditional* cond)
// {
//     BasicBlock* merge = cond->getMergeBlock();

//     // For each phi node, try to move the definitions into the merge block
//     for (BasicBlock::iterator i = merge->begin(); PHINode* pn = dyn_cast<PHINode>(i); ++i) {
//         // <do stuff>
//     }


//     return false;
// }

bool FlattenCondAssn::removeDeadCode(const Conditional* cond)
{
    bool changed = false;
    changed |= SimplifyInstructionsInBlock(cond->getEntryBlock());
    changed |= SimplifyInstructionsInBlock(cond->getThenBlock());
    changed |= SimplifyInstructionsInBlock(cond->getElseBlock());

    // TODO: Remove dead code in the then and else subgraphs

    // <do stuff>
    return false;
}

bool FlattenCondAssn::removeEmptyConditional(const Conditional* cond)
{
    if (!cond->isSelfContained() || !cond->isEmptyConditional())
        return false;

    // BasicBlock* left  = cond->getThenBlock();
    // BasicBlock* right = cond->getElseBlock();
    BasicBlock* merge = cond->getMergeBlock();
    BasicBlock* entry = cond->getEntryBlock();

    BranchInst* entryBranch = dyn_cast<BranchInst>(entry->getTerminator());
    assert(entryBranch);

    bool changed = true;

    ReplaceInstWithInst(entryBranch, BranchInst::Create(merge));

    changed |= SimplifyInstructionsInBlock(entry);
    changed |= SimplifyInstructionsInBlock(merge);

    // TODO: Be more hygenic by removing the empty subgraphs ourselves

    // Future work: merge the block into the predecessor, and update any
    // affected conditionals
    // MergeBasicBlockIntoOnlyPred(merge);

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
INITIALIZE_PASS(FlattenCondAssn,
                "flatten-conditional-assignments",
                "Flatten conditional assignments into select instructions",
                true,   // Whether it preserves the CFG
                false); // Whether it is an analysis pass

FunctionPass* gla_llvm::createFlattenConditionalAssignmentsPass()
{
    return new FlattenCondAssn();
}
