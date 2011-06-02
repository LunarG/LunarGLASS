//===- IdentifyStructures.h - Identify structures in a structured-CFG -----===//
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

#ifndef GLA_IDENTIFY_STRUCTURES_H
#define GLA_IDENTIFY_STRUCTURES_H

#include "llvm/ADT/DenseMap.h"

#include "Passes/PassSupport.h"
#include "Passes/Util/ConditionalUtil.h"
#include "Passes/Util/LoopUtil.h"

namespace gla_llvm {
    using namespace llvm;

    class IdentifyStructures : public FunctionPass {
    public:
        IdentifyStructures()
            : FunctionPass(ID)
            , loopInfo(0)
            , mainCopyOut(0)
            , stageExit(0)
        {
            initializeIdentifyStructuresPass(*PassRegistry::getPassRegistry());
        }

        ~IdentifyStructures();

        // Iterators for contained conditionals
        typedef DenseMap<const BasicBlock*, Conditional*>::iterator conditional_iterator;
        conditional_iterator conditional_begin() { return conditionals.begin(); }
        conditional_iterator conditional_end()   { return conditionals.end(); }
        bool                 conditional_empty() { return conditionals.empty(); }

        // Returns the Conditional that the passed BasicBlock is the entry for
        Conditional* getConditional(const BasicBlock* entry) const { return conditionals.lookup(entry); }

        // Returns the innermost loop that bb is in
        LoopWrapper* getLoopFor(const BasicBlock* bb) const { return loopWrappers.lookup(bb); }

        bool isMain() const { return stageExit; } // stageExit is only defined when in main

        bool isMainExit(const BasicBlock* bb)    const { return stageExit    && bb == stageExit; }
        bool isMainCopyOut(const BasicBlock* bb) const { return mainCopyOut && bb == mainCopyOut; }

        const BasicBlock* getMainExit()    const { return stageExit; }
        const BasicBlock* getMainCopyOut() const { return mainCopyOut; }

        // Standard pass stuff
        static char ID;


        bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        void getAnalysisUsage(AnalysisUsage&) const;
        void releaseMemory();

    private:
        DenseMap<const BasicBlock*, Conditional*> conditionals;

        DenseMap<const BasicBlock*, LoopWrapper*> loopWrappers;

        LoopInfo* loopInfo;
        const BasicBlock* mainCopyOut;
        const BasicBlock* stageExit;

        IdentifyStructures(const IdentifyStructures&); // do not implement
        void operator=(const IdentifyStructures&);     // do not implement
    };

} // end namespace gla_llvm


#endif // GLA_IDENTIFY_STRUCTURES_H
