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
//===----------------------------------------------------------------------===//

#include "llvm/Pass.h"
#include "llvm/Support/CFG.h"

#include "CanonicalizeCFG.h"

using namespace llvm;

namespace  {
    class CanonicalizeCFG : public FunctionPass {
    public:
        // Standard pass stuff
        static char ID;
        CanonicalizeCFG() : FunctionPass(ID) {}
        virtual bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;
    };
} // end namespace

bool CanonicalizeCFG::runOnFunction(Function& F)
{
    // Loop over all but the entry block
    for (Function::iterator bbI = ++F.begin(), bbE = F.end(); bbI != bbE; ++bbI) {

        // If the block has no predecessors, remove it from the function
        if (pred_begin(bbI) == pred_end(bbI)) {
            for (succ_iterator sI = succ_begin(bbI), sE = succ_end(bbI); sI != sE; ++sI) {
                (*sI)->removePredecessor(bbI);
            }
            bbI->dropAllReferences();
            bbI = F.getBasicBlockList().erase(bbI);
        }
    }

    return true;
}

void CanonicalizeCFG::getAnalysisUsage(AnalysisUsage& AU) const
{
    return;
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
FunctionPass* llvm::createCanonicalizeCFGPass()
{
    return new CanonicalizeCFG();
}
