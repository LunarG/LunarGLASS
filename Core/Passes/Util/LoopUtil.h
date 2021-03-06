//===- Loops.h - Utility functions and wrappers for loops -----------------===//
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
// Provide a utility wrapper around llvm::Loop, and utility functions for
// dealing with and analyzing loops
//
//===----------------------------------------------------------------------===//

#ifndef GLA_LOOP_UTIL_H
#define GLA_LOOP_UTIL_H

#pragma warning(push, 1)
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#pragma warning(pop)

#include "Passes/Util/ADT.h"
#include "Passes/Util/BasicBlockUtil.h"

#include <stack>

namespace gla_llvm {
    using namespace llvm;
    class LoopStack;

    // Loop wrapper providing more queries/information
    class LoopWrapper {
    public:
        LoopWrapper(Loop* loop, DominanceFrontier* df, ScalarEvolution* sev, bool simpleBE)
            : domFront(df)
            , loop(loop)
            , scalarEvo(sev)
            , simpleLatch(simpleBE)
            , header(loop->getHeader())
            , latch(loop->getLoopLatch())
            , blocks(loop->getBlocks().begin(), loop->getBlocks().end())
            , uniqueExiting(loop->getExitingBlock())
            , inductiveVar(loop->getCanonicalInductionVariable())
            // Scalar Evolution's trip count for the header block will sometimes be
            // 1 higher than the loop body's true trip count. This is
            // because it is counting the number of times that the exit
            // condition may be tested, and the exit block could be at the
            // top or rotated to the bottom.
            , tripCount(scalarEvo->getSmallConstantTripCount(loop, header))
            , increment(1)
            , staticBound(0)
            , dynamicBound(0)
            , loopCompare(0)
            , loopPredicate(llvm::CmpInst::ICMP_NE)
            , loopDepth(loop->getLoopDepth())
            , forLoopComputed(false)
            , simpleConditional(-1)
            , function(loop->getHeader()->getParent())
            , isMain(IsMain(*function))
            , stageExit(0)
            , stageEpilogue(0)
        {
            if (isMain) {
                stageExit    = GetMainExit(*function);
                stageEpilogue = GetMainEpilogue(*function);
            }

            loop->getUniqueExitBlocks(exits);

            SmallPtrSet<BasicBlock*,3> merges;
            GetMergePoints(exits, *domFront, merges);
            merges.erase(stageExit);
            merges.erase(stageEpilogue);

            // See if any of the exit subgraphs go directly to stage-epilogue or
            // stage-exit
            discards = false;
            returns  = false;
            for (SmallVector<BasicBlock*,4>::iterator bb = exits.begin(), e = exits.end(); bb != e; ++bb) {
                if (!returns && domFront->find(*bb)->second.count(stageEpilogue)) {
                    returns = true;
                }
                if (!discards && domFront->find(*bb)->second.count(stageExit)) {
                    discards = true;
                }
            }

            assert(merges.size() <= 1 && "Unstructured control flow");

            if (merges.size() == 1)
                exitMerge = *merges.begin();
            else
                exitMerge = NULL;
        }

        // Accessors
        BasicBlock* getHeader()    const { return header; }
        BasicBlock* getLatch()     const { return latch; }
        BasicBlock* getExitMerge() const { return exitMerge; }

        // Queries
        bool isSimpleLatching() const { return simpleLatch; }
        bool hasDiscard()       const { return discards; }
        bool hasReturn()        const { return returns; }

        bool isLatch(const BasicBlock* bb)   const { return latch == bb; }
        bool isHeader(const BasicBlock* bb)  const { return header == bb; }
        bool isExiting(const BasicBlock* bb) const { return isLoopExiting(bb); }

        // Whether the given instruction is inside the loop and referenced
        // outside the loop. Instructions not in the loop are trivially
        // externally referenced.
        // Linear in the number of uses
        bool isExternallyReferenced(const Instruction* inst) const
        {
            if (! contains(inst))
                return false;

            if (inst->use_empty())
                return false;

            for (Value::const_use_iterator i = inst->use_begin(), e = inst->use_end(); i != e; ++i) {
                const Instruction* use = dyn_cast<const Instruction>(*i);
                assert(use);
                // TODO: loops: in some cases, it can appear in PHINodes where the phi
                // copies will be added prior to exiting the loop. Figure out
                // logic for this
                if (! contains(use))
                    return true;
            }

            return false;
        }


        // A loop-relevant block is one whose control-flow (e.g. successors)
        // makes this loop specially recognizable. This will be true for
        // headers, latches, and exiting blocks.
        bool isLoopRelevant(const BasicBlock* bb) const
        {
            return contains(bb) && (isExiting(bb) || isLatch(bb) || isHeader(bb));
        }

        // Provide interfaces from the Loop class
        unsigned getLoopDepth()                  const { return loopDepth; }
        const PHINode* getInductionVariable()    const { return inductiveVar; }
        bool     contains(const BasicBlock* bb)  const { return blocks.count(bb); }
        bool     contains(const Instruction* i)  const { return blocks.count(i->getParent()); }
        unsigned int getTripCount()              const { return tripCount; }
        int getStaticBound()                     const { return staticBound; }
        int getIncrement()                       const { return increment; }
        const Value* getDynamicBound()           const { return dynamicBound; }
        llvm::ICmpInst::Predicate getPredicate() const { return loopPredicate; }

        const SmallPtrSet<const BasicBlock*,16>& getBlocks() const { return blocks; }

        bool isLoopExiting(const BasicBlock* bb) const
        {
            for (succ_const_iterator i = succ_begin(bb), e = succ_end(bb); i != e; ++i)
                if (! contains(*i))
                    return true;

            return false;
        }

        void computeDynamicBound()
        {
            // Get exit condition for loops that are simple inductive except for not statically
            // knowing their trip count.
            // This is currently a very narrow dive into a common case, but hopefully there
            // is a more generic way of dealing with loop bounds.
            // TODO: loops: generalize and/or use better encapsulation of this way of finding 
            // a loop bound.
            if (! tripCount && inductiveVar && uniqueExiting) {
                const SCEV* exitCount = scalarEvo->getExitCount(loop, uniqueExiting);
                if (! isa<SCEVCouldNotCompute>(exitCount)) {
                    const SCEVNAryExpr* expr = dyn_cast<SCEVNAryExpr>(exitCount);
                    //errs() << *expr << " SCEVNaryExpr\n";
                    if (expr && expr->getNumOperands() == 2 && exitCount->getSCEVType() == scSMaxExpr) {
                        const SCEV* op1 = expr->getOperand(1);
                        if (op1->getSCEVType() == scUnknown)
                            dynamicBound = cast<SCEVUnknown>(op1)->getValue();
                    }
                }
            }
        }

        // See if we can find our own "inductive" variable: something that can be turned into a "for loop", possibly with a non-1 increment.
        // (LLVM's idea of inductive may be more restrictive than this.)
        void computeForLoop()
        {
            if (forLoopComputed)
                return;
            forLoopComputed = true;
            
            computeDynamicBound();

            // Dynamic bound implies incrementing up by 1 until getting to the bound
            if (dynamicBound)
                return;

            // Get the inductive variable and the increment
            const llvm::Value* cond = GetCondition(header);
            if (! cond) {
                inductiveVar = 0;
                return;
            }
            const llvm::CmpInst* loopCompare = dyn_cast<CmpInst>(cond);
            if (! loopCompare) {
                inductiveVar = 0;
                return;
            }

            if (! inductiveVar && uniqueExiting && isCanonical() && tripCount && isSimpleConditional()) {
                const llvm::Value* var = loopCompare->getOperand(0);
                const llvm::PHINode* phi = llvm::dyn_cast<llvm::PHINode>(var);

                // Only consider loops that use "Add" to increment the loop variable
                const llvm::Instruction* add = 0;
                if (phi && isHeader(phi->getParent())) {
                    // find argument that computes the increment
                    const llvm::Instruction* inst = dyn_cast<llvm::Instruction>(phi->getIncomingValue(0));
                    if (inst && inst->getOpcode() == llvm::Instruction::Add)
                        add = inst;
                    else {
                        inst = dyn_cast<llvm::Instruction>(phi->getIncomingValue(1));
                        if (inst && inst->getOpcode() == llvm::Instruction::Add)
                            add = inst;
                    }
                } else {
                    const llvm::Instruction* inst = dyn_cast<llvm::Instruction>(var);
                    if (inst && inst->getOpcode() == llvm::Instruction::Add) {
                        phi = dyn_cast<llvm::PHINode>(inst->getOperand(0));
                        if (phi && isHeader(phi->getParent()))
                            add = inst;
                    }
                }
                if (add) {
                    const llvm::ConstantInt *constantInt = llvm::dyn_cast<llvm::ConstantInt>(add->getOperand(1));
                    if (constantInt) {
                        inductiveVar = phi;
                        increment = (int)constantInt->getValue().getSExtValue();
                    }
                }
            }

            // Get the static bound
            if (inductiveVar) {
                // Even if the increment is one, the tripCount is sometimes the same as the exit condition's comparison
                // value and sometimes one different than the exit condition's comparison value,
                // depending on whether the exit is at the top or rotated to the bottom.
                // In both cases, the comparison value is the correct value to pass to
                // beginSimpleInductiveLoop(), which expects a body trip count.
                // (Both values are off-by-one when the loop body becomes dead code.)
                loopPredicate = loopCompare->getPredicate();
                switch (loopPredicate) {
                case llvm::CmpInst::ICMP_EQ:   loopPredicate = llvm::CmpInst::ICMP_NE;  break;
                case llvm::CmpInst::ICMP_SGT:
                case llvm::CmpInst::ICMP_UGT:  loopPredicate = llvm::CmpInst::ICMP_SLE; break;
                case llvm::CmpInst::ICMP_SGE:
                case llvm::CmpInst::ICMP_UGE:  loopPredicate = llvm::CmpInst::ICMP_SLT; break;
                default:
                    inductiveVar = 0;
                    return;
                }
 
                const llvm::ConstantInt *constantInt = llvm::dyn_cast<llvm::ConstantInt>(loopCompare->getOperand(1));
                if (constantInt)
                    staticBound = (int)constantInt->getValue().getSExtValue();
                else
                    inductiveVar = 0;
            }
        }

        // Is the loop a simple inductive one? A simple inductive loop is one
        // where the backedge is simple, a canonical induction variable exists,
        // and the execution count is known statically (e.g. there are no breaks
        // or continues). Or, there is a known upper bound and it would otherwise
        // be simple conditional.
        bool isSimpleInductive()
        {
            computeForLoop();

            return inductiveVar && uniqueExiting && (tripCount || (dynamicBound && isSimpleConditional()));
        }

        // Is the loop a simple conditional loop? A simple conditional loop is a
        // loop whose header is a conditionally exiting block, and the condition
        // is some comparison operator whose operands are extracts, constants,
        // or loads. Furthermore, all either other instructions in the header can only
        // be phis or we have to know the loop is always executed more than once.
        bool isSimpleConditional()
        {
            // Check the cache
            if (simpleConditional != -1)
                return simpleConditional > 0;

            // It has to be conditional, comparison operator and the header has
            // to be exiting
            Value* v = GetCondition(header);
            if (! v || ! isLoopExiting(header))
                return false;

            CmpInst* cond = dyn_cast<CmpInst>(v);
            if (! cond)
                return false;

            if (tripCount > 0) {
                simpleConditional = 1;
                return true;
            }

            // If we don't know the trip count, we don't know if the simple condition might 
            // skip the loop completely, which we can't allow if something interesting gets
            // done in the header, before the kick out.

            // There can't be any other non-phi instructions in the header, so
            // keep track of the number of instructions we've seen.
            int count = 2;      // Already looked at the branch and condition

            // They have to be phis, constants, extracts (vector swizzle), or loads
            // (uniforms).  Add non-phis to our count if they are in the header.
            for (Instruction::const_op_iterator i = cond->op_begin(), e = cond->op_end(); i != e; ++i) {
                if (isa<PHINode>(i)) {
                    // phis are okay
                } else if (isa<ExtractElementInst>(i) || isa<LoadInst>(i) || isa<Constant>(i)) {
                    // these are all okay
                    if (Instruction* inst = dyn_cast<Instruction>(i))
                        if (inst->getParent() == header)
                            ++count;
                } else
                    return false;
            }

            // Add up the header's phis to the count
            for (BasicBlock::const_iterator i = header->begin(), e = header->end(); i != e; ++i) {
                if (! isa<PHINode>(i))
                    break;
                ++count;
            }

            // Our total has to be the number of instructions in the header.
            simpleConditional = header->size() == count ? 1 : 0;

            return simpleConditional > 0;
        }

        // Whether the loop is a canonical, structured loop.  In a canonical,
        // structured loop there should only be one latch.  Tf the latch is
        // conditional (and thus simple), the other branch should be an
        // exiting branch (enforces bottom-latching semantics).  If there are
        // multiple exit blocks, they should all eventually merge to a single
        // point that lies in the intersection of each of their dominance
        // frontiers (enforces structured flow control).
        bool isCanonical() const
        {
            return header && latch && exitMerge && (IsUnconditional(latch) || isLoopExiting(latch));
        }

        // If the loop is simple inductive, then returns the instruction
        // recomputing the exit condition, else returns NULL
        Instruction* getInductiveExitCondition()
        {
            if (! isSimpleInductive())
                return NULL;

            // We currently only support inductive loops without early exit
            assert(uniqueExiting);

            BranchInst* br = dyn_cast<BranchInst>(uniqueExiting->getTerminator());
            assert(br && br->isConditional());

            return dyn_cast<Instruction>(br->getCondition());
        }

        // If the loop is simple inductive, then returns the instruction doing
        // the increment on the inductive variables, else returns NULL
        Instruction* getIncrementInst()
        {
            if (! isSimpleInductive())
                return NULL;
            assert(inductiveVar);

            int index = inductiveVar->getBasicBlockIndex(latch);
            assert(index >= 0);

            return dyn_cast<Instruction>(inductiveVar->getIncomingValue(index));
        }

        // TODO: loops: refactor into mask or enum or interface
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
        Loop* loop;

        // Note: LLVM's DominanceFrontier is deprecated, go in the direction of not needing it.
        // TODO: Remove the need for the dominance frontier, it's
        // only used for the merges and testing for returns/discards. The second
        // purpose can be generalized elsewhere.
        DominanceFrontier* domFront;

        ScalarEvolution* scalarEvo;

        bool simpleLatch;

        BasicBlock* header;
        BasicBlock* latch;

        SmallPtrSet<const BasicBlock*, 16> blocks;

        BasicBlock* uniqueExiting;

        const PHINode* inductiveVar;
        unsigned int tripCount;
        int increment;
        int staticBound;
        Value* dynamicBound;
        const llvm::CmpInst* loopCompare;
        llvm::CmpInst::Predicate loopPredicate;

        unsigned loopDepth;

        // Cache
        mutable bool forLoopComputed;
        mutable int simpleConditional;

        Function* function;
        bool isMain;

        SmallVector<BasicBlock*, 4> exits;

        bool discards;
        bool returns;

        BasicBlock* stageExit;
        BasicBlock* stageEpilogue;

        BasicBlock* exitMerge;

    private:
        LoopWrapper(const LoopWrapper&);       // do not implement
        void operator=(const LoopWrapper&);    // do not implement
    };

} // end namespace llvm_gla


#endif // GLA_LOOP_UTIL_H
