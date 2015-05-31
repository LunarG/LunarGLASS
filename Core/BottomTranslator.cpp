//===- BottomTranslator.cpp - Translate bottom IR to Generic IR -----------===//
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
// Author: John Kessenich, LunarG
// Author: Michael Ilseman, LunarG
//
// Translate bottom IR to another IR through an LLVM module pass
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Description of how flow control is handled:
//
// * Flow control is handled on a basic block by basic block level, rather than
//   on an instruction by instruction level.  This is because some constructs
//   like do-while loops need to know as soon as they begin what kind of flow
//   control construct they represent.
//
// * Each block is passed to handleBlock, which dispatches it depending on
//   available info.  If it's a loop header, latch, or exit, then it will
//   dispatch it to handleLoopBlock.  Otherwise, if it's a branch, dispatch to
//   handleBranchingBlock and if it's a switch dispatch to handleSwitchBlock.
//   If none of the above apply, then it will get passed
//   to handleReturnBlock.  handleBlock keeps track of blocks it's already seen,
//   so it wont process the same block twice, allowing it to be called by other
//   handlers when a certain basic block processing order must be maintained.
//   This also allows handleBlock to know when it's handling the last program
//   order (not neccessarily llvm order) block.
//
// * Loops in LLVM are a set of blocks, possibly identified with up to 3
//   properties: header, exiting, and latch.  A block that is a loop header
//   (there is only 1 per loop) has the loop's backedge branching to it.  It
//   also is the first block encountered in program or LLVM IR order.  An
//   exiting block is a block that exits the loop, and the block outside the
//   loop that the exiting block branches to is called an exit block.  A latch
//   is a block with a backedge.  Canonicalization (via loop-simplify or a
//   prerequisite for any of the loop optimizations) enforces that there is only
//   1 latch, and that all of the predecessors of an exit block are inside the
//   loop.  During canonicalization, if there are more than 1 latches, a new
//   (unconditional) latch block is created and the previous latches now branch
//   to it instead of the header.  Any blocks without one of the above
//   properties can be handled normally, as though they weren't even in a loop.
//
// * Loops are presented to the backend using the loop interfaces present in
//   PrivateManager.h.  Nested loops are currently not supported.
//
// * handleBranchingBlock handles it's instructions, and adds phi nodes if specified
//   by the backend. On an unconditional branch, it checks to see if the block
//   being branched is a subtree of the cfg and if so handles it, otherwise it
//   does nothing. On a conditional branch, it will find the earliest confluce
//   point, determine if it's an if-then-else construct, call the addIf
//   interface, and handle the then block. It also takes care of condition
//   inversion when the then branch is the confluence point. If the construct is
//   an if-then-else construct, it will then call the addElse interface and
//   handle the else block. Finally, it calls the addEndIf interface.
//
// * handleReturnBlock handles it's instructions, and calls the
//   handleReturnBlock interface
//
//===----------------------------------------------------------------------===//

// LLVM includes
#pragma warning(push, 1)
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/PassManager.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/DominanceFrontier.h"  // Note: this is deprecated, go in the direction of not needing it
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"
#pragma warning(pop)

#include <cstdio>
#include <string>
#include <map>
#include <vector>
#include <algorithm>

// LunarGLASS includes
#include "Exceptions.h"
#include "Backend.h"
#include "Util.h"
#include "PrivateManager.h"
#include "Options.h"
#include "metadata.h"

// LunarGLASS Passes and Utils
#include "Passes/PassSupport.h"
#include "Passes/Analysis/IdentifyStructures.h"
#include "Passes/Util/LoopUtil.h"
#include "Passes/Util/InstructionUtil.h"

using namespace llvm;
using namespace gla_llvm;

namespace {

    class BottomTranslator : public ModulePass {
    public:
        BottomTranslator() : ModulePass(ID), backEndTranslator(0), backEnd(0) { }

        BottomTranslator(gla::BackEndTranslator* bet, gla::BackEnd* be, gla::Manager* m)
            : ModulePass(ID)
            , backEndTranslator(bet)
            , backEnd(be)
            , manager(m)
            , outputInstOnly(be->addOutputInstructionsOnly())
        {
            initializeBottomTranslatorPass(*PassRegistry::getPassRegistry());
        }

        ~BottomTranslator()
        { }

        // Translate from LLVM CFG style to structured style.
        void declarePhiCopies(const Function*);

        // Add phi copies if the backend wants us to:

        // For all the successors of bb
        void addPhiCopies(const BasicBlock* bb);

        // For nextBB that are relevant for curBB
        void addPhiCopies(const BasicBlock* curBB, const BasicBlock* nextBB);

        // For the specified phi node for currBB
        void addPhiCopy(const PHINode* phi, const BasicBlock* curBB);

        // Module Pass implementation
        static char ID;
        bool runOnModule(Module&);
        void getAnalysisUsage(AnalysisUsage&) const;

    protected:
        gla::BackEndTranslator* backEndTranslator;
        gla::BackEnd* backEnd;
        gla::Manager* manager;
        gla::EFlowControlMode flowControlMode;
        bool useBreakOp;
        bool useContinueOp;
        bool useEarlyReturnOp;
        bool useDiscardOp;
        bool inMain;

        DominatorTree* domTree;
        DominanceFrontier* domFront;  // Note: this is deprecated, go in the direction of not needing it
        IdentifyStructures* idStructs;

        // Switch information, dynamically maintained while translating blocks
        // for the back end, on a stack of nested switch statements.
        // TODO: This needs fuller treatment, probably by becoming part of IdentifyStructures.
        //       It's slightly guess work now to deduce the switch's merge point, and in complex
        //       topologies, the heuristics used here and in handleBranchingBlock() breaks down.
        struct switchDesc {
            switchDesc(const llvm::BasicBlock* merge, const llvm::SwitchInst* inst) : mergeBlock(merge), switchInst(inst) { }
            const llvm::BasicBlock* mergeBlock;
            const llvm::SwitchInst* switchInst;
            bool fallthrough(const llvm::BasicBlock* b)
            {
                for (auto it = switchInst->case_begin(); it != switchInst->case_end(); ++it)
                    if (it.getCaseSuccessor() == b)
                        return true;
                if (mergeBlock && mergeBlock != switchInst->getDefaultDest() && b == switchInst->getDefaultDest())
                    return true;
                return false;
            }
        };
        llvm::SmallVector<switchDesc, 2> switchStack;

        bool lastBlock;

        // Whether we should call add on all instructions, or only on output
        // ones
        bool outputInstOnly;

        const BasicBlock* stageEpilogue;
        const BasicBlock* stageExit;

        // How many basic blocks are in the current function being handled
        int numBBs;

        SmallPtrSet<const BasicBlock*, 8> handledBlocks;

        // Data for handling loops
        std::stack<LoopWrapper*> loops;

        // Set everything up for handling a new loop
        void newLoop(const BasicBlock*);

        // Reset loop data
        void closeLoop() { loops.pop(); };

        // Handle and dispatch the given block, updating handledBlocks.
        void handleBlock(const BasicBlock*);

        // Handle and dispatch for an if block
        void handleIfBlock(const BasicBlock*);

        // Send off all the non-terminating instructions in a basic block to the
        // backend
        void handleNonTerminatingInstructions(const BasicBlock*, bool forceGlobals=false);

        void handleInstructions(const BasicBlock* bb, bool forceGlobals=false);

        // Call backEndTranslator's add.
        void addInstruction(const Instruction* inst, bool forceGlobal)
        {
            if (outputInstOnly && ! IsOutputInstruction(inst))
                return;

            // We're already handling discards by analyzing flow control, so no
            // need to pass them on
            if (IsDiscard(inst)) {
                if (! useDiscardOp && ! inMain) {
                    // TODO: functionality: turn discard in a function into flow control for back ends with no discard op
                    gla::UnsupportedFunctionality("discard from non-main function with no discard op in back end");
                } else
                    forceDiscard();

                return;
            }

            // TODO: loops: update the below to work with nested loops. Basically, it
            // will have to find the loop on the stack that corresponds to inst,
            // and query it rather than the top one.
            bool externallyReferenced = forceGlobal || (! loops.empty() && loops.top()->isExternallyReferenced(inst));
            backEndTranslator->addInstruction(inst, lastBlock, externallyReferenced);
        }

        void addIoDeclarations(Module& module, gla::EVariableQualifier qualifier, const char* categoryName)
        {
            const llvm::NamedMDNode* mdList = module.getNamedMetadata(categoryName);
            if (mdList) {
                for (unsigned int m = 0; m < mdList->getNumOperands(); ++m) {
                    const llvm::MDNode* mdNode = mdList->getOperand(m);
                    backEndTranslator->addIoDeclaration(qualifier, mdNode);
                }
            }
        }

        // Call add (except for phis when applicable) on all the instructions
        // provided in insts.
        void addInstructions(SmallVectorImpl<const Instruction*>& insts, bool forceGlobals=false);

        void handleSimpleInductiveInstructions(const BasicBlock* bb);

        void handleSimpleConditionalInstructions(const BasicBlock* bb) { return; }

        // Given a loop block, handle it. Dispatches to other methods and ends
        // up handling all blocks in the loop.
        void handleLoopBlock(const BasicBlock*, bool instructionsHandled = false);

        // Given a block ending in return, output it and the return
        void handleReturnBlock(const BasicBlock*);

        // Handle non-loop, non-switch control flow
        void handleBranchingBlock(const BasicBlock*);

        // Handle a block ending in a switch
        void handleSwitchBlock(const BasicBlock*);

        // Force the output of the current latch. Assumes it's only being called
        // when the source-level backedge is not simple.
        void forceOutputLatch();

        // Force the output of the return block, for early returns. Currently
        // only implemented for early returns in main
        void forceReturn();

        // Force a discard to happen.
        void forceDiscard();

        // If dominator properly dominates dominatee, then handle it, else do nothing
        bool attemptHandleDominatee(const BasicBlock* dominator, const BasicBlock* dominatee);

        // Sets up a the beginning of a loop for the loop at the top of our LoopStack.
        // Assumes there is something on top of our LoopStack.
        void setUpLoopBegin(const Value* condition);

        // Sets up the exit from the loop at the top of our LoopStack. Assumes
        // there is something on top of our LoopStack.
        void setUpLoopExit(const Value* condition, const BasicBlock* bb);

        // Sets up the simple latch for the loop at the top of our
        // LoopStack. Assumes there is something on top of our LoopStack
        void setUpSimpleLatch();

    };

    // TODO: memory model: worry about loads for address spaces that can be written to, as identical loads could read different values?
    // TODO: code quality: more phi aliasing: should this generalize to catch other cases?
    bool SameLoad(const Value* value1, const Value* value2)
    {
        // They are most likely instructions, so dyn_cast has no real performance loss
        const Instruction* inst1 = dyn_cast<const Instruction>(value1);
        const Instruction* inst2 = dyn_cast<const Instruction>(value2);

        if (! inst1 || inst1->getOpcode() != Instruction::Load ||
            ! inst2 || inst2->getOpcode() != Instruction::Load)
            return false;

        // We have two load instructions, see if they match
        if (inst1->getNumOperands() != inst2->getNumOperands())
            return false;

        for (unsigned op = 0; op < inst1->getNumOperands(); ++op)
            if (inst1->getOperand(op) != inst2->getOperand(op))
                return false;

        return true;
    }

    bool IsAliasingPhi(const PHINode* phi)
    {
        // Find out if all the sources of the phi are really the same thing, in which case 
        // we only want to add an alias for that thing.  This is important, for example,
        // in cases like phi'ing a sampler, and going to a high-level language, we don't want
        // this IR
        //
        //    if (...)
        //        %3 = load X
        //        texture(%3...
        //    else
        //        %4 = load X
        //    %5 = phi(%3, %4)
        //    texture(%5...
        //
        // to turn into this high-level code
        //
        //    if (...) {
        //        textureGrad(g_tColor, ...
        //        t1 = g_tColor;
        //    }  else {
        //        t1 = g_tColor;
        //    }
        //    texture(t1, ...
        //
        // Rather, we want the back end to know t1 is an alias for g_tColor (which was a load
        // from a uniform address space), so it can use g_tColor when it sees t1.
        bool alias = true;
        for (unsigned incoming = 1; incoming < phi->getNumIncomingValues(); ++incoming) {
            if (! SameLoad(phi->getIncomingValue(0), phi->getIncomingValue(incoming))) {
                alias = false;
                break;
            }
        }

        return alias;
    }

    void CreateSimpleInductiveLoop(LoopWrapper& loop, gla::BackEndTranslator& bet)
    {
        const PHINode* pn  = loop.getInductionVariable();
        assert(pn);

        if (loop.getTripCount())
            bet.beginForLoop(pn, loop.getPredicate(), loop.getStaticBound(), loop.getIncrement());
        else
            bet.beginSimpleInductiveLoop(pn, loop.getDynamicBound());
    }

    void CreateSimpleConditionalLoop(LoopWrapper& loop, const Value& condition, gla::BackEndTranslator& bet)
    {
        BasicBlock* header = loop.getHeader();
        assert(loop.isLoopExiting(header) && (loop.contains(GetSuccessor(0, header)) || 
                                              loop.contains(GetSuccessor(1, header))));

        const CmpInst* cmp = dyn_cast<CmpInst>(&condition);
        assert(cmp && cmp->getNumOperands() == 2 && cmp == GetCondition(header));

        bet.beginSimpleConditionalLoop(cmp, cmp->getOperand(0), cmp->getOperand(1),
                                       loop.contains(GetSuccessor(1, header)));
    }

} // end namespace

void BottomTranslator::declarePhiCopies(const Function* function)
{
    if (! backEnd->getRemovePhiFunctions())
        return;

    // basic blocks
    for (Function::const_iterator bb = function->begin(), E = function->end(); bb != E; ++bb) {

        // instructions in the basic block
        for (BasicBlock::const_iterator i = bb->begin(), e = bb->end(); i != e; ++i) {
            const Instruction* llvmInstruction = i;

            if (llvmInstruction->getOpcode() == Instruction::PHI) {
                if (IsAliasingPhi(dyn_cast<PHINode>(llvmInstruction)))
                    backEndTranslator->declarePhiAlias(llvmInstruction);
                else
                    backEndTranslator->declarePhiCopy(llvmInstruction);
            }
        }
    }
}

void BottomTranslator::addPhiCopies(const BasicBlock* bb)
{
    for (succ_const_iterator s = succ_begin(bb), e = succ_end(bb); s != e; ++s)
        addPhiCopies(bb, *s);
}

void BottomTranslator::addPhiCopies(const BasicBlock* curBB, const BasicBlock* nextBB)
{
    if (! backEnd->getRemovePhiFunctions())
        return;

    // For each llvm phi node, add a copy instruction
    for (BasicBlock::const_iterator i = nextBB->begin(), e = nextBB->end(); i != e; ++i) {
        const PHINode *phiNode = dyn_cast<PHINode>(i);

        if (phiNode)
            addPhiCopy(phiNode, curBB);
        else
            break;
    }
}

void BottomTranslator::addPhiCopy(const PHINode* phi, const BasicBlock* curBB)
{
    // Exclude phi copies for our inductive variables
    if (! loops.empty() && loops.top()->isSimpleInductive() && phi == loops.top()->getInductionVariable())
        return;

    bool alias = IsAliasingPhi(phi);

    // Find the operand whose predecessor is curBB.
    int predIndex = phi->getBasicBlockIndex(curBB);
    if (predIndex >= 0) {
        // then we found ourselves

        Value* src = phi->getIncomingValue(predIndex);
        if (! gla::IsUndef(src)) {
            if (alias)
                backEndTranslator->addPhiAlias(phi, src);
            else
                backEndTranslator->addPhiCopy(phi, src);
        }
    }
}

void BottomTranslator::handleInstructions(const BasicBlock* bb, bool forceGlobals)
{
    LoopWrapper* loop = NULL;

    if (! loops.empty())
        loop = loops.top();
    else {
        handleNonTerminatingInstructions(bb);
        return;
    }

    if (loop->isSimpleInductive() && (loop->isLatch(bb) || loop->isExiting(bb)))
        // Simple inductive latch/exiting block
        handleSimpleInductiveInstructions(bb);
    else if (loop->isSimpleConditional() && loop->isHeader(bb))
        // Simple conditional header
        handleSimpleConditionalInstructions(bb);
    else
        // Normal block
        handleNonTerminatingInstructions(bb, forceGlobals);
}

void BottomTranslator::handleNonTerminatingInstructions(const BasicBlock* bb, bool forceGlobals)
{
    // Add the non-terminating instructions
    for (BasicBlock::const_iterator i = bb->begin(), e = bb->getTerminator(); i != e; ++i) {
        const Instruction* inst = i;

        if (! (backEnd->getRemovePhiFunctions() && inst->getOpcode() == Instruction::PHI))
            addInstruction(i, forceGlobals);
    }
}

void BottomTranslator::addInstructions(SmallVectorImpl<const Instruction*>& insts, bool forceGlobals)
{
    for (SmallVectorImpl<const Instruction*>::iterator i = insts.begin(), e = insts.end(); i != e; ++i)
        if (! (backEnd->getRemovePhiFunctions() && (*i)->getOpcode() == Instruction::PHI))
            addInstruction(*i, forceGlobals);
}

void BottomTranslator::newLoop(const BasicBlock* bb)
{
    assert(idStructs->getLoopFor(bb) && "newLoop called on non-loop");

    loops.push(idStructs->getLoopFor(bb));

    // We'll have to handle the latch specially if the backedge is not simple.
    if (! loops.top()->isSimpleLatching())
        handledBlocks.insert(loops.top()->getLatch());
}

// Return true if the block topology was recognized and/or handled.
bool BottomTranslator::attemptHandleDominatee(const BasicBlock* dominator, const BasicBlock* dominatee)
{
    // If the dominatee is a early return or a discard, then force it's output
    // accordingly
    if (dominatee == stageExit)
        return true;
    if (dominatee == stageEpilogue) {
        forceReturn();
        return true;
    }

    if (ProperlyDominates(dominator, dominatee, *domTree)) {
        handleBlock(dominatee);
        return true;
    }

    return false;
}

void BottomTranslator::handleLoopBlock(const BasicBlock* bb, bool instructionsHandled)
{
    assert(loops.size() != 0 && "handleLoopBlock called on a new loop without newLoop being called");

    // TODO: loops: Test nested loops thoroughly

    const Value* condition = GetCondition(bb);

    LoopWrapper* loop  = loops.top();
    const BasicBlock* header = loop->getHeader();
    const BasicBlock* latch  = loop->getLatch();

    bool isHeader    = bb == header;
    bool isLatch     = bb == latch;
    bool isExiting   = loop->isExiting(bb);
    assert(isHeader || isLatch || isExiting);

    bool simpleLatch = loop->isSimpleLatching();

    // TODO: loops: have calculating exit merge take into account when some of the
    // exit blocks merge into a return (that is, you can have return statements
    // inside loops, with some exiting blocks going to it).

    assert(! isLatch || simpleLatch); // isLatch => simpleLatch

    // If it's a loop header, have the back-end add it
    if (isHeader)
        setUpLoopBegin(condition);

    // If the branch is conditional and not a latch nor exiting, we're dealing
    // with conditional (e.g. if-then-else) flow control from a header.
    // Otherwise handle its instructions ourselves.
    if (condition && ! (isLatch || isExiting)) {
        assert(idStructs->getConditional(bb));
        handleBranchingBlock(bb);
    } else if (! instructionsHandled)
        handleInstructions(bb);

    // If we're exiting, add the (possibly conditional) exit.
    if (isExiting) {
        setUpLoopExit(condition, bb);
    }

    if (isLatch) {
        assert(simpleLatch);
        assert(( !condition || isExiting) && "redundant assertion failed");

        setUpSimpleLatch();
    }

    // We've been fully handled by now, unless we're a header
    if (! isHeader)
        return;

    // If we're an unconditionally branching header (e.g. branching immediately
    // to an inner loop), add our target
    if (IsUnconditional(bb)) {
        handleBlock(GetSuccessor(0, bb));
    }

    // If we're simple-latching, schedule the latch (if it's not been seen
    // before)
    if (simpleLatch) {
        handleBlock(latch);
    }

    // By this point, we're a header and all of our blocks in our loop should of
    // been handled.
    assert(IsSubset(loop->getBlocks(), handledBlocks));

    const BasicBlock* exitMerge = loop->getExitMerge();

    const BasicBlock* exitBlock = NULL;

    if (isExiting) {
        int exitPos = loop->exitSuccNumber(bb);
        assert(exitPos != -1);
        if (exitPos == 2)
            gla::UnsupportedFunctionality("complex loop exits (two exit branches from same block)");

        exitBlock = GetSuccessor(exitPos, bb);
    }

    // Simple conditional loops should have exitBlock defined for them
    assert(! loop->isSimpleConditional() || exitBlock); // simple-conditional => exitBlock

    // If we're simple conditional, then we shouldn't be executing any
    // instructions before the merge point of the loop. Thus, exitBlock
    // should be the loop-merge. If we've identified an exitMerge, then it
    // must be the loop-merge, and must be equivalent to exitBlock
    assert((! loop->isSimpleConditional() || ! exitMerge || AreEquivalent(exitBlock, exitMerge)) && "unstructured conditional loop");

    backEndTranslator->endLoop();
    closeLoop();

    const BasicBlock* loopMerge = loop->isSimpleConditional() && ! exitMerge ? exitBlock : exitMerge;

    // Handle the exitBlock, if it's not the loopMerge
    if (exitBlock && loopMerge != exitBlock) {
        handleBlock(exitBlock);
    }

    // Schedule the handling of the loop-merge block now, to make sure it occurs
    // immediately. If we're conditional, then the loop-merge block is just
    // exitBlock. It should be ok to immediately schedule the loop-merge block,
    // because the exitMerge should be dominated by the loop header.
    if (loopMerge) {
        assert(ProperlyDominates(header, loopMerge, *domTree));
        handleBlock(loopMerge);
    }

    return;
}

void BottomTranslator::forceOutputLatch()
{
    const BasicBlock* latch = loops.top()->getLatch();
    bool simpleLatch  = loops.top()->isSimpleLatching();

    assert(latch && !simpleLatch);
    assert(handledBlocks.count(latch));

    assert(IsUnconditional(latch));

    handleInstructions(latch, true);

    addPhiCopies(latch);
}

void BottomTranslator::forceReturn()
{
    assert(stageEpilogue && stageExit && "non-main early return");

    handleInstructions(stageEpilogue);
    handleReturnBlock(stageExit);
}

void BottomTranslator::forceDiscard()
{
    backEndTranslator->addDiscard();
}

void BottomTranslator::handleIfBlock(const BasicBlock* bb)
{
    Conditional* cond = idStructs->getConditional(bb);
    assert(cond);
    assert(! cond->hasSharedMerge() && "Given shared merge");

    // We shouldn't be called for any loop-relevant block that isn't a header
    assert(loops.empty() || loops.top()->isHeader(bb) || ! loops.top()->isLoopRelevant(bb));

    bool invert = cond->isIfElse();

    // Add an if
    backEndTranslator->addIf(cond->getCondition(), invert);

    // Add the then block, flipping it if we're inverted
    if (invert) {
        handleBlock(cond->getElseBlock());
    } else {
        handleBlock(cond->getThenBlock());
    }

    // Add the else block, if we're an if-then-else.
    if (cond->isIfThenElse()) {
        backEndTranslator->addElse();
        handleBlock(cond->getElseBlock());
    }

    backEndTranslator->addEndif();

    // We'd like to now schedule the handling of the merge block. If there is no
    // merge block (e.g. there was a return/discard/break/continue), then we're
    // done.
    const BasicBlock* merge = cond->getMergeBlock();
    if (merge)
        handleBlock(merge);

    return;
}

void BottomTranslator::handleReturnBlock(const BasicBlock* bb)
{
    assert(isa<ReturnInst>(bb->getTerminator()));

    handleInstructions(bb);
    backEndTranslator->addInstruction(bb->getTerminator(), lastBlock);

    return;
}

void BottomTranslator::handleBranchingBlock(const BasicBlock* bb)
{
    // Handle its instructions and do phi-node removal if appropriate
    handleInstructions(bb);
    addPhiCopies(bb);

    // If it's unconditional, we'll want to handle any subtrees (and introduced
    // latches/discards/returns) that it points to.
    if (IsUnconditional(bb)) {
        const BasicBlock* succ = GetSuccessor(0,bb);
        // If it's the (non-simple) latch, handle it, else handle it if we dominate it (also includes returns/discards)
        if (! loops.empty() && ! loops.top()->isSimpleLatching() && succ == loops.top()->getLatch()) {
            handleBlock(succ);
            return;
        }

        if (attemptHandleDominatee(bb, succ))
            return;

        // see if it's the end of a switch case statement
        if (switchStack.size() > 0) {
            if (switchStack.back().fallthrough(succ))
                backEndTranslator->endCase(false);
            else if (switchStack.back().mergeBlock == nullptr) {
                // Speculate that this branches to the merge block of a switch statement.
                switchStack.back().mergeBlock = succ;
                backEndTranslator->endCase(true);
            } else if (switchStack.back().mergeBlock == succ)
                backEndTranslator->endCase(true);
            else
                gla::UnsupportedFunctionality("switch topology", gla::EATContinue);
        }

        return;
    }

    // If there's a loop, and we're loop-relevant and not a header, then handle
    // it as a loopblock, else handle it as an if block
    if (! loops.empty() && loops.top()->isLoopRelevant(bb) && ! loops.top()->isHeader(bb))
        handleLoopBlock(bb, true);
    else
        handleIfBlock(bb);

    return;
}

// Handle a block that ends with a switch branch.  This includes 
// handling each case, the default, and the merge block after the switch.
void BottomTranslator::handleSwitchBlock(const BasicBlock* bb)
{
    const SwitchInst* switchInstr = dyn_cast<SwitchInst>(bb->getTerminator());
    assert(switchInstr);

    handleInstructions(bb);
    addPhiCopies(bb);

    // open the structure switch
    backEndTranslator->addSwitch(switchInstr->getCondition());

    // Start out not knowing the merge block, but knowing we're in a switch
    switchStack.push_back(switchDesc(0, switchInstr));

    // visit each case (not the default)
    SwitchInst::ConstCaseIt caseIt = switchInstr->case_begin();
    for (; caseIt != switchInstr->case_end(); caseIt++) {
        backEndTranslator->addCase((int)caseIt.getCaseValue()->getSExtValue());
        handleBlock(caseIt.getCaseSuccessor());
    }

    // visit the default, which can be hard to distinguish from the merge block
    // (sometimes they are two different things, sometimes not)
    SwitchInst::ConstCaseIt defaultIt = switchInstr->case_default();

    // see if the default block is not the merge block
    const BasicBlock* defaultBlock = defaultIt.getCaseSuccessor();
    if (switchStack.back().mergeBlock == 0)
        switchStack.back().mergeBlock = defaultBlock;
    else if (defaultBlock != switchStack.back().mergeBlock) {
        backEndTranslator->addDefault();
        handleBlock(defaultBlock);
    }

    // close out the structured switch
    backEndTranslator->endSwitch();

    // handle merge block
    if (switchStack.back().mergeBlock)
        handleBlock(switchStack.back().mergeBlock);
    switchStack.pop_back();

    return;
}

void BottomTranslator::handleBlock(const BasicBlock* bb)
{
    assert(bb);

    // If handleBlock is called on a return or discard block, then perform the
    // copyout/return
    if (bb == stageExit || bb == stageEpilogue) {
        // Are we on the last block?
        if (handledBlocks.size() == numBBs)
            lastBlock = true;

        if (bb == stageEpilogue)
            forceReturn();

        return;
    }

    // If handleBlock is called on a non-simple latch, then force its output
    // to happen.
    // TODO: loops: keep around a "lastLoopBlock" analog so that final continues need
    // not be printed out
    if (! loops.empty() && loops.top()->getLatch() == bb && !loops.top()->isSimpleLatching()) {
        assert(IsUnconditional(bb));
        forceOutputLatch();
        backEndTranslator->addLoopBack(NULL, false);
        return;
    }

    if (handledBlocks.count(bb))
        return;
    handledBlocks.insert(bb);

    // Are we on the last block?
    if (handledBlocks.size() == numBBs)
        lastBlock = true;

    // TODO: loops: We have a notion of exiting block, so we want to treat it
    // specially, we could also have a notion of latching/returning/discarding
    // block, something that is either a latch/return/discard, or may branch to
    // one. Consider if there's value to this notion, e.g. it can be used as
    // (part of) a heuristic for whether we want to have an early return or else
    // wrap a body of code in a conditional in the backend.

    // If the block exhibits loop-relevant control flow, handle it specially
    LoopWrapper* loop = idStructs->getLoopFor(bb);
    if (! loop && ! loops.empty())
        loop = loops.top();

    if (loop && loop->isLoopRelevant(bb) && flowControlMode == gla::EFcmStructuredOpCodes) {
        if (loop->isHeader(bb))
            newLoop(bb);

        handleLoopBlock(bb);
        return;
    }

    // If the block's a branching block, handle it specially.
    if (isa<BranchInst>(bb->getTerminator())) {
        handleBranchingBlock(bb);
        return;
    }

    if (isa<SwitchInst>(bb->getTerminator())) {
        handleSwitchBlock(bb);
        return;
    }

    // Otherwise we have to be a block ending in a return statement
    if (! isa<ReturnInst>(bb->getTerminator()))
       gla::UnsupportedFunctionality("Non-supported terminator");

    handleReturnBlock(bb);
}

void BottomTranslator::handleSimpleInductiveInstructions(const BasicBlock* bb)
{
    assert(! loops.empty());

    // If we're simple inductive, then don't do the instructions computing
    // the inductive variable or the exit condition

    SmallVector<const Instruction*, 32> insts;
    const Instruction* lastInst  = bb->getTerminator();

    for (BasicBlock::const_iterator i = bb->begin(); &*i != lastInst; ++i) {
        if (&*i == loops.top()->getInductiveExitCondition() || &*i == loops.top()->getIncrementInst())
            continue;
        insts.push_back(i);
    }

    addInstructions(insts);
}

void BottomTranslator::setUpSimpleLatch()
{
    // If it's a latch, we must be simple and thus only need to add phi copies
    assert(! loops.empty() && loops.top()->isSimpleLatching());

    // Add phi copies (if applicable)
    addPhiCopies(loops.top()->getLatch(), loops.top()->getHeader());
}

void BottomTranslator::setUpLoopBegin(const Value* condition)
{
    assert(loops.size() != 0);
    LoopWrapper* loop = loops.top();

    // TODO: loops: add more loop constructs here
    // TODO: loops: stick body in here
    if (loop->isSimpleInductive()) {
        CreateSimpleInductiveLoop(*loop, *backEndTranslator);
    } else if (loop->isSimpleConditional()) {
        CreateSimpleConditionalLoop(*loop, *condition, *backEndTranslator);
    } else {
        backEndTranslator->beginLoop();
    }
}

void BottomTranslator::setUpLoopExit(const Value* condition, const BasicBlock* bb)
{
    assert(loops.size() != 0);
    LoopWrapper* loop = loops.top();

    assert(loop->isExiting(bb));

    // Don't output if it's simple-conditional (and the header) or
    // simple-inductive (and given the inductive exit condition)
    bool shouldOutput = ! ((loop->isSimpleInductive() && condition == loop->getInductiveExitCondition())
                        || (loop->isSimpleConditional() && loop->isHeader(bb) ));

    int exitPos = loop->exitSuccNumber(bb);
    assert(exitPos != -1);

    if (exitPos == 2)
        gla::UnsupportedFunctionality("complex loop exits (two exit branches from same block)");

    const BasicBlock* exit      = GetSuccessor(exitPos, bb);
    assert(exit);

    const BasicBlock* exitMerge = loop->getExitMerge();
    assert((exitMerge || loop->hasReturn() || loop->hasDiscard()) && "unstructured control flow");

    bool invertCondition = exitPos == 1;
    bool hasExitCode = exit != exitMerge;
    bool hasExitPhis = isa<PHINode>(exit->front());

    // We need to make an if-then to hold the break if there's exit code or exit
    // phis and there's a condition.
    bool wrapInConditional = condition && shouldOutput && (hasExitCode || hasExitPhis);

    // Set up the conditional if we have exit code or we have phi copies to
    // make, and add the exit block subgraph if it isn't the exit merge.
    if (wrapInConditional)
        backEndTranslator->addIf(condition, invertCondition);

    // Add phi copies (if applicable)
    addPhiCopies(bb, exit);

    // Output the exit if we should
    if (shouldOutput) {
        if (handledBlocks.count(exit)) {
            gla::UnsupportedFunctionality("complex loop exits (shared exit block)");
        }

        // Handle our intermediary exit graph, unless we go directly to
        // exitMerge.
        if (exit != exitMerge) {
            handleBlock(exit);
        }

        // Don't output the break statement if we have just outputted a return
        // statement. This is the case if exitBlock's dominance frontier
        // consists only of stage-epilogue or stage-exit.
        SmallVector<const BasicBlock*,2> targets;
        targets.push_back(stageExit);
        targets.push_back(stageEpilogue);
        if (! ContainedDominanceFrontier(exit, targets, *domFront)) {
            if (wrapInConditional) {
                backEndTranslator->addLoopExit();
            } else {
                backEndTranslator->addLoopExit(condition, invertCondition);
            }
        }

        if (wrapInConditional)
            backEndTranslator->addEndif();
    }

    // Immediately handle the other (non-exit) block if we dominate it.
    if (condition) {
        attemptHandleDominatee(bb, GetSuccessor(! exitPos, bb));
    }

}

bool BottomTranslator::runOnModule(Module& module)
{
    //
    // Query the back end about its flow control
    //
    backEnd->getControlFlowMode(flowControlMode, useBreakOp, useContinueOp, useEarlyReturnOp, useDiscardOp);
    if (flowControlMode == gla::EFcmExplicitMasking)
        gla::UnsupportedFunctionality("explicit masking in middle end");

    // allow back end to finish initialization, since its constructor was called too early
    backEndTranslator->start(module);

    //
    // Translate globals.
    //

    // add metadata
    // Do this before addGlobal(), so the back end knows what globals are really logical IO.
    addIoDeclarations(module, gla::EVQUniform, gla::UniformListMdName);
    addIoDeclarations(module, gla::EVQInput,   gla::InputListMdName);
    addIoDeclarations(module, gla::EVQOutput,  gla::OutputListMdName);

    // add global variables
    for (Module::const_global_iterator global = module.global_begin(), end = module.global_end(); global != end; ++global) {

        // See if it's a global constant (not a uniform)
        const llvm::PointerType* pointer = llvm::dyn_cast<llvm::PointerType>(global->getType());
        if (pointer && pointer->getAddressSpace() == gla::GlobalAddressSpace && global->isConstant())
            backEndTranslator->addGlobalConst(global);
        else
            backEndTranslator->addGlobal(global);
    }

    //
    // Translate code.
    //
    Module::iterator function, lastFunction;
    for (function = module.begin(), lastFunction = module.end(); function != lastFunction; ++function) {
        inMain = (function->getName().compare("main") == 0);
        if (function->isDeclaration()) {
            // TODO: functionality:  function calls: do we need to handle declarations of functions, or just definitions?
        } else {
            assert (loops.size() == 0);

            // Get/set the loop info
            domTree   = &getAnalysis<DominatorTree>        (*function);
            domFront  = &getAnalysis<DominanceFrontier>    (*function);  // Note: this is deprecated, go in the direction of not needing it
            idStructs = &getAnalysis<IdentifyStructures>   (*function);

            // Set up the function info
            stageEpilogue = idStructs->getMainCopyOut();
            stageExit    = idStructs->getMainExit();

            handledBlocks.clear();
            handledBlocks.insert(stageEpilogue);
            handledBlocks.insert(stageExit);

            // handle function's with bodies
            backEndTranslator->startFunctionDeclaration(function->getFunctionType(), function->getName());

            // paramaters and arguments
            for (Function::const_arg_iterator arg = function->arg_begin(), endArg = function->arg_end(); arg != endArg; ++arg) {
                Function::const_arg_iterator nextArg = arg;
                ++nextArg;
                backEndTranslator->addArgument(arg, nextArg == endArg);
            }

            backEndTranslator->endFunctionDeclaration();

            backEndTranslator->startFunctionBody();

            // Phi declaration pass
            if (backEnd->getDeclarePhiCopies())
                declarePhiCopies(function);

            lastBlock = false;

            // basic blocks

            numBBs = function->size();
            handleBlock(function->begin());

            // Only the first block should have to be handled, as every
            // subgraph knows how to handle itself.
            if (! IsSubset(function->getBasicBlockList(), handledBlocks))
                gla::UnsupportedFunctionality("control flow: not all blocks were translated", gla::EATContinue);

            backEndTranslator->endFunctionBody();
        }
    }

    //set_branchtargets(&v, currentInstructionructions, num_instructions);
    backEndTranslator->end(module);

    return false;
}

void BottomTranslator::getAnalysisUsage(AnalysisUsage& AU) const
{
    AU.addRequired<DominatorTree>();
    AU.addRequired<DominanceFrontier>();  // Note: this is deprecated, go in the direction of not needing it
    AU.addRequired<IdentifyStructures>();

    AU.setPreservesAll();
}

char BottomTranslator::ID = 0;
INITIALIZE_PASS_BEGIN(BottomTranslator,
                      "bottom-transl",
                      "LunarGLASS bottom translator pass",
                      false,  // Whether it looks only at CFG
                      false); // Whether it is an analysis pass
INITIALIZE_PASS_DEPENDENCY(DominatorTree)
INITIALIZE_PASS_DEPENDENCY(DominanceFrontier)
INITIALIZE_PASS_DEPENDENCY(IdentifyStructures)
INITIALIZE_PASS_END(BottomTranslator,
                    "bottom-transl",
                    "LunarGLASS bottom translator pass",
                    false,  // Whether it looks only at CFG
                    false); // Whether it is an analysis pass

// The below are the only externally exposed functionality

void gla::PrivateManager::translateBottomToTarget()
{
    PassManager passManager;
    // llvm will delete what we pass to add, so that has be newed
    passManager.add(new BottomTranslator(backEndTranslator, backEnd, this));
    passManager.run(*module);
}
