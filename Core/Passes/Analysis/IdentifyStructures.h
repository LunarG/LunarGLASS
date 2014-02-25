//===- IdentifyStructures.h - Identify structures in a structured-CFG -----===//
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
//     such that a change to one can call the recalculate methods of its
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

        bool isMain() const { return stageExit != 0; } // stageExit is only defined when in main

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
