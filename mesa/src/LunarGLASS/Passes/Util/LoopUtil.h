//===- Loops.h - Utility functions and wrappers for loops -----------------===//
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
// Provide a utility wrapper around llvm::Loop, and utility functions for
// dealing with and analyzing loops
//
//===----------------------------------------------------------------------===//

#ifndef LOOP_UTIL_H
#define LOOP_UTIL_H

#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/LoopInfo.h"

#include "Passes/Util/ADT.h"
#include "Passes/Util/BasicBlockUtil.h"

#include <stack>

namespace gla_llvm {
    using namespace llvm;

    // Loop wrapper providing more queries/information
    class LoopWrapper {
    public:
        LoopWrapper(Loop* l, DominanceFrontier* df /*, ScalarEvolution* se*/)
            : loop(l)
            , domFront(df)
            // , scalarEvo(se)
            , header(loop->getHeader())
            , latch(loop->getLoopLatch())
            , preservedBackedge(IsConditional(latch) || latch->getSinglePredecessor())
            , inductiveVar(loop->getCanonicalInductionVariable())
            , tripCount(loop->getTripCount())
        {
            loop->getUniqueExitBlocks(exits);

            // TODO: update to consider the presence of return
            exitMerge = GetSingleMergePoint(exits, *domFront);
        }

        // Accessors
        BasicBlock* getHeader()    const { return header; }
        BasicBlock* getLatch()     const { return latch; }
        BasicBlock* getExitMerge() const { return exitMerge; }
        bool preservesBackedge()   const { return preservedBackedge; }

        // Wrapped functionality
        unsigned getLoopDepth()                  const { return loop->getLoopDepth(); }
        bool isLoopExiting(const BasicBlock* bb) const { return loop->isLoopExiting(bb); }
        PHINode* getCanonicalInductionVariable() const { return inductiveVar; }
        bool contains(const BasicBlock* bb)      const { return loop->contains(bb); }
        Loop::block_iterator block_begin()       const { return loop->block_begin(); }
        Loop::block_iterator block_end()         const { return loop->block_end(); }
        Value* getTripCount()                    const { return tripCount; }

        // Note, we may want to move away from only wrapping LoopInfo and wrap
        // ScalarEvolution as well


        // New functionality

        // TODO: cache the results of the more complicated queries

        // Is the loop simple inductive one. A simple inductive loop is one
        // where the backedge is preserved, a canonical induction variable
        // exists, and the execution count is known statically (e.g. there are
        // no breaks or continues)
        bool isSimpleInductive() const
        {
            // TODO: extend functionality to support early exit and continue.
            return preservedBackedge && loop->getCanonicalInductionVariable() && loop->getTripCount();
        }

        // Is the loop a simple conditional loop. A simple conditional loop is a
        // loop whose header is a conditionally exiting block, and the condition
        // is some comparison operator whose operands are extracts, constants,
        // or loads. Furthermore, all other instructions in the header can only
        // be phis.
        bool isSimpleConditional() const
        {
            // It has to be conditional, comparison operator and the header has
            // to be exiting
            Value* v = GetCondition(header);
            if (! v || ! isLoopExiting(header))
                return false;

            CmpInst* cond = dyn_cast<CmpInst>(v);
            if (! cond)
                return false;

            // There can't be any other non-phi instructions in the block, so
            // keep track of the number of instructions we've seen.
            int count = 2;      // Already looked at the branch and condition

            // They have to be constants, extracts (vector swizzle), or loads
            // (uniforms). Add them to our count if they are in the header
            for (Instruction::const_op_iterator i = cond->op_begin(), e = cond->op_end(); i != e; ++i) {
                if (! (isa<ExtractElementInst>(i) || isa<LoadInst>(i) || isa<Constant>(i)))
                    return false;
                if (Instruction* inst = dyn_cast<Instruction>(i))
                    if (inst->getParent() == header)
                        ++count;
            }
            // TODO: handle when the cmp's arguments are phis

            // Add up the phis to the count
            for (BasicBlock::const_iterator i = header->begin(), e = header->end(); i != e; ++i) {
                if (! isa<PHINode>(i))
                    break;
                ++count;
            }

            // Our total has to be the number of instructions in the header.
            return header->size() == count;
        }

        // Whether the loop is a canonical, structured loop.  In a canonical,
        // structured loop there should only be one latch.  Tf the latch is
        // conditional (and thus preserved), the other branch should be an
        // exiting branch (enforces bottom-latching semantics).  If there are
        // multiple exit blocks, they should all eventually merge to a single
        // point that lies in the intersection of each of their dominance
        // frontiers (enforces structured flow control).
        bool isCanonical() const
        {
            return header && latch && exitMerge
                && (IsUnconditional(latch) || loop->isLoopExiting(latch));
        }

        // If the loop is simple inductive, then returns the instruction
        // recomputing the exit condition, else returns NULL
        Instruction* getInductiveExitCondition() const
        {
            if (! isSimpleInductive())
                return NULL;

            BasicBlock* exit = loop->getExitingBlock();
            assert(exit);

            BranchInst* br = dyn_cast<BranchInst>(exit->getTerminator());
            assert(br && br->isConditional());

            return dyn_cast<Instruction>(br->getCondition());
        }

        // If the loop is simple inductive, then returns the instruction doing
        // the increment on the inductive variables, else returns NULL
        Instruction* getIncrement() const
        {
            if (! isSimpleInductive())
                return NULL;
            assert(inductiveVar);

            int index = inductiveVar->getBasicBlockIndex(latch);
            assert(index >= 0);

            return dyn_cast<Instruction>(inductiveVar->getIncomingValue(index));
        }

        // TODO: refactor into mask or enum or interface
        // Returns the successor number (0 or 1) of the exiting edge from an exiting
        // block. Returns -1 if none exit, and 2 if they both exit.
        int exitSuccNumber(const BasicBlock* bb) const
        {
            if (! isLoopExiting(bb))
                return -1;

            const BranchInst* br = dyn_cast<BranchInst>(bb->getTerminator());
            assert(br);

            if (br->isUnconditional())
                return 0;

            if (! contains(br->getSuccessor(0)) && ! contains(br->getSuccessor(1)))
                return 2;

            // Else return 0 or 1 indicating which one is not contained
            return contains(br->getSuccessor(0));
        }

        ~LoopWrapper() { }

    protected:
        // LoopInfo* loopInfo;
        Loop* loop;
        DominanceFrontier* domFront;
        ScalarEvolution* scalarEvo;

        BasicBlock* header;
        BasicBlock* latch;

        bool preservedBackedge;

        PHINode* inductiveVar;
        Value*   tripCount;

        SmallVector<BasicBlock*, 4> exits;

        BasicBlock* exitMerge;

    private:
        LoopWrapper(const LoopWrapper&);       // do not implement
        void operator=(const LoopWrapper&);    // do not implement

    };

    class LoopStack {
    public:
        LoopStack()
        { }

        ~LoopStack()
        {
            clear();
        }

        void pushNewLoop(const BasicBlock* bb)
        {
            st.push(new LoopWrapper(loopInfo->getLoopFor(bb), domFront));
        }

        LoopWrapper* top() { return st.top(); }

        int size() { return st.size(); }

        void clear()
        {
            while (size())
                pop();
        }

        void pop()
        {
            LoopWrapper* lw = top();
            st.pop();
            delete lw;
        }

        void setDominanceFrontier(DominanceFrontier* df) { domFront = df; }
        void setLoopInfo(LoopInfo* li)                   { loopInfo = li; }
        // void setScalarEvolution(ScalarEvolution* se)     { scalarEvo = se; }


    protected:
        DominanceFrontier* domFront;
        LoopInfo* loopInfo;
        // ScalarEvolution* scalarEvo;

        std::stack<LoopWrapper*> st;


    private:
        LoopStack(const LoopStack&);      // do not implement
        void operator=(const LoopStack&); // do not implement

    };


} // end namespace llvm_gla


#endif // LOOP_UTIL_H
