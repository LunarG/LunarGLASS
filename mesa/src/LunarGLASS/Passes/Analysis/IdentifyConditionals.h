//===- IdentifyCondtionals.h - Identify the conditional expressions -------===//
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
// Identify the structured conditional expressions, and collect information
// about them, including their classification (e.g. if they're an if-then-else),
// and their merge points.
//
//===----------------------------------------------------------------------===//

#ifndef IDENTIFY_CONDITIONALS_H
#define IDENTIFY_CONDITIONALS_H

#include "llvm/BasicBlock.h"
#include "llvm/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Analysis/DominanceFrontier.h"
#include "llvm/Analysis/Dominators.h"

#include "Passes/PassSupport.h"
#include "Passes/Util/ConditionalUtil.h"

namespace gla_llvm {
    using namespace llvm;

    class IdentifyConditionals : public FunctionPass {
    private:
        DenseMap<const BasicBlock*, Conditional*> conditionals;
    public:
        // Iterators for this class provide a pair of <entryBlock*, Conditional*>
        typedef DenseMap<const BasicBlock*, Conditional*>::iterator iterator;
        iterator begin() { return conditionals.begin(); }
        iterator end()   { return conditionals.end(); }
        bool     empty() { return conditionals.empty(); }

        // Returns the Conditional that the passed BasicBlock is the entry for
        Conditional* getConditional(const BasicBlock* entry) const { return conditionals.lookup(entry); }
        Conditional* operator[](const BasicBlock* entry)     const { return getConditional(entry); }

        // Erase the conditional from our analysis. Does not modify the actual program.
        void eraseConditional(const BasicBlock* bb) { conditionals.erase(bb); }

        // Set the underlying conditional to NULL (so as to potentially keep iterators valid)
        void nullConditional(const BasicBlock* bb) { conditionals[bb] = NULL; }

        // Standard pass stuff
        static char ID;

        IdentifyConditionals() : FunctionPass(ID)
        {
            initializeIdentifyConditionalsPass(*PassRegistry::getPassRegistry());
        }


        virtual bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;
        virtual void releaseMemory();
    };
} // end namespace gla_llvm

#endif // IDENTIFY_CONDITIONALS_H
