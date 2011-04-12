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

namespace llvm {

    class Conditional;
    class DominanceFrontier;

    class IdentifyConditionals : public FunctionPass {
    private:
        DenseMap<const BasicBlock*, const Conditional*> conditionals;
    public:
        // Iterators for this class provide a pair of <entryBlock*, Conditional*>
        typedef DenseMap<const BasicBlock*, const Conditional*>::const_iterator const_iterator;
        const_iterator begin() const { return conditionals.begin(); }
        const_iterator end()   const { return conditionals.end(); }
        bool           empty() const { return conditionals.empty(); }

        // Returns the Conditional that the passed BasicBlock is the entry for
        const Conditional* getConditional(const BasicBlock* entry) const { return conditionals.lookup(entry); }
        const Conditional* operator[](const BasicBlock* entry)     const { return getConditional(entry); }

        // Erase the conditional from our analysis. Does not modify the actual program.
        void eraseConditional(const BasicBlock* bb) { conditionals.erase(bb); }

        // Set the underlying conditional to NULL (so as to potentially keep iterators valid)
        void nullConditional(const BasicBlock* bb) { conditionals[bb] = NULL; }

        // Standard pass stuff
        static char ID;
        IdentifyConditionals() : FunctionPass(ID) {}
        virtual bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;
        virtual void releaseMemory();
    };

    // Class providing analysis inquiries about a conditional expression.
    class Conditional {
    private:
        // IdentifyConditionals is the only one allowed to construct a Conditional
        friend class IdentifyConditionals;

        Conditional(BasicBlock* entryBlock, BasicBlock* mergeBlock,
                    BasicBlock* thenBlock, BasicBlock* elseBlock,
                    DominanceFrontier* frontiers)
            : entry(entryBlock)
            , merge(mergeBlock)
            , left(thenBlock)
            , right(elseBlock)
            , domFront(frontiers)
        { }

        BasicBlock* entry;
        BasicBlock* merge;
        BasicBlock* left;
        BasicBlock* right;

        DominanceFrontier* domFront;

    public:
        // Whether there is no "then" block, only an "else" one. This may be
        // useful information, e.g. if a transformation wishes to invert a
        // condition and flip branches around.
        bool isIfElse() const { return left == merge; }

        bool isIfThen() const { return right == merge; }

        bool isIfThenElse() const { return !(isIfElse() || isIfThen()); }

        // Whether the conditional is self-contained. This means that each the
        // then and else blocks, if they are distinct from the merge block,
        // point to subgraphs whose only exit is the merge block.
        bool isSelfContained() const;

        // Whether the conditional is empty. An empty conditional is a
        // self-contained conditional in which the then and else subgraphs, if
        // they exist, are all empty blocks. Currently unimplemented in the case
        // of then or else subgraphs, for which is currently will return false.
        bool isEmptyConditional() const;

        BasicBlock* getEntryBlock() const { return entry; }
        BasicBlock* getMergeBlock() const { return merge; }
        BasicBlock* getThenBlock()  const { return left; }
        BasicBlock* getElseBlock()  const { return right; }

        BranchInst* getBranchInst() const { return dyn_cast<BranchInst>(entry->getTerminator()); }

        Value* getCondition() const { return getBranchInst()->getCondition(); };
    };


} // end namespace llvm

#endif // IDENTIFY_CONDITIONALS_H
