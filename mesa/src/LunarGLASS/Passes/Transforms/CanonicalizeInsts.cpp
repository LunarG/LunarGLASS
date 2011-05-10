//===- CanonicalizeInsts.cpp - Canonicalize instructions for LunarGLASS ---===//
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
// Canonicalize instructions for LunarGLASS, this includes the following:
//
//   * fcmp ord %foo <some-constant> --> fcmp oeq %foo %foo
//
//===----------------------------------------------------------------------===//

#include "llvm/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "Passes/PassSupport.h"

using namespace llvm;
using namespace gla_llvm;

namespace  {
    class CanonicalizeInsts : public FunctionPass {
    public:
        static char ID;

        CanonicalizeInsts() : FunctionPass(ID)
        {
            initializeCanonicalizeInstsPass(*PassRegistry::getPassRegistry());
        }

        virtual bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;

    private:
        CanonicalizeInsts(const CanonicalizeInsts&); // do not implement
        void operator=(const CanonicalizeInsts&); // do not implement
    };
} // end namespace

bool CanonicalizeInsts::runOnFunction(Function& F)
{
    bool changed = false;

    for (Function::iterator bbI = F.begin(), bbE = F.end(); bbI != bbE; ++bbI) {
        for (BasicBlock::iterator instI = bbI->begin(), instE = bbI->end(); instI != instE; /* empty */) {
            Instruction* inst = instI;
            ++instI;

            // TODO: explore: fcmp ord %foo %bar --> fcmp oeq %foo %foo ; fcmp oeq %bar %bar

            // fcmp ord %foo <some-constant> --> fcmp oeq %foo %foo
            FCmpInst* cmp = dyn_cast<FCmpInst>(inst);
            if (cmp && cmp->getPredicate() == CmpInst::FCMP_ORD) {
                // See if/which one of the aguments is constant
                bool isLeftConstant  = isa<Constant>(cmp->getOperand(0));
                bool isRightConstant = isa<Constant>(cmp->getOperand(1));

                if (isLeftConstant ^ isRightConstant) {
                    Value* arg = cmp->getOperand(isLeftConstant ? 1 : 0);
                    FCmpInst* newCmp = new FCmpInst(CmpInst::FCMP_OEQ, arg, arg, "ord");
                    ReplaceInstWithInst(cmp, newCmp);
                }
            }
        }
    }

    return changed;
}

void CanonicalizeInsts::getAnalysisUsage(AnalysisUsage& AU) const
{
    return;
}

void CanonicalizeInsts::print(raw_ostream&, const Module*) const
{
    return;
}


char CanonicalizeInsts::ID = 0;
INITIALIZE_PASS_BEGIN(CanonicalizeInsts,
                      "canonicalize-insts",
                      "Canonicalize instructions for LunarGLASS",
                      true,   // Whether it preserves the CFG
                      false); // Whether it is an analysis pass
INITIALIZE_PASS_END(CanonicalizeInsts,
                    "canonicalize-insts",
                    "Canonicalize instructions for LunarGLASS",
                    true,   // Whether it preserves the CFG
                    false); // Whether it is an analysis pass


FunctionPass* gla_llvm::createCanonicalizeInstsPass()
{
    return new CanonicalizeInsts();
}

