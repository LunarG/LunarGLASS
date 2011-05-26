//===- BottomTranslator.cpp - Translate bottom IR to Generic IR -----------===//
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
//   handleBranchingBlock.  If none of the above apply, then it will get passed
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
// * handleBranching handles it's instructions, and adds phi nodes if specified
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
#include "llvm/DerivedTypes.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/Intrinsics.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/PassManager.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/DominanceFrontier.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/LazyValueInfo.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/TypeSymbolTable.h"

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

// LunarGLASS Passes and Utils
#include "Passes/PassSupport.h"
#include "Passes/Analysis/IdentifyStructures.h"
#include "Passes/Util/LoopUtil.h"

using namespace llvm;
using namespace gla_llvm;

namespace {

    class BottomTranslator : public ModulePass {
    public:
        BottomTranslator() : ModulePass(ID)
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

        // For all the given phis
        template<unsigned Size>
        void addPhiCopies(SmallPtrSet<const PHINode*, Size>& phis, const BasicBlock* curBB);

        // For the specified phi node for currBB
        void addPhiCopy(const PHINode* phi, const BasicBlock* curBB);

        void setBackEndTranslator(gla::BackEndTranslator* bet) { backEndTranslator = bet; }
        void setBackEnd(gla::BackEnd* be)                      { backEnd = be; }

        // Module Pass implementation
        static char ID;
        bool runOnModule(Module&);
        void print(std::ostream&, const Module*) const;
        void getAnalysisUsage(AnalysisUsage&) const;

    protected:
        gla::BackEndTranslator* backEndTranslator;
        gla::BackEnd* backEnd;
        gla::EFlowControlMode flowControlMode;

        DominatorTree* domTree;
        DominanceFrontier* domFront;
        IdentifyStructures* idStructs;
        ScalarEvolution* scalarEvo;
        LazyValueInfo* lazyInfo;

        bool lastBlock;

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
        void handleNonTerminatingInstructions(const BasicBlock*);

        void handleInstructions(const BasicBlock* bb);

        // Call add (except for phis when applicable) on all the instructions
        // provided in insts.
        void addInstructions(SmallVectorImpl<const Instruction*>& insts);

        void handleSimpleInductiveInstructions(const BasicBlock* bb);

        void handleSimpleConditionalInstructions(const BasicBlock* bb) { return; }

        // Given a loop block, handle it. Dispatches to other methods and ends
        // up handling all blocks in the loop.
        void handleLoopBlock(const BasicBlock*, bool instructionsHandled = false);

        // Given a block ending in return, output it and the return
        void handleReturnBlock(const BasicBlock*);

        // Handle non-loop control flow
        void handleBranchingBlock(const BasicBlock*);

        // Force the output of the current latch. Assumes it's only being called
        // when the source-level backedge is not simple.
        void forceOutputLatch();

        // Force the output of the return block, for early returns. Currently
        // only implemented for early returns in main
        void forceReturn();

        // Force a discard to happen.
        void forceDiscard();

        // If dominator properly dominates dominatee, then handle it, else do nothing
        void attemptHandleDominatee(const BasicBlock* dominator, const BasicBlock* dominatee);

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

} // end namespace

static void CreateSimpleInductiveLoop(LoopWrapper& loop, gla::BackEndTranslator& bet)
{
    const PHINode* pn  = loop.getCanonicalInductionVariable();
    const Value* count = loop.getTripCount();
    assert(pn && count);

    int tripCount = gla::GetConstantInt(count);
    assert (tripCount  >= 0);

    if (gla::Options.debug && ! gla::Options.bottomIROnly) {
        errs() << "\ninductive variable:"   << *pn;
        errs() << "\n  trip count:        " << tripCount;
        errs() << "\n  increment:       "   << *loop.getIncrement();
        errs() << "\n  exit condition:  "   << *loop.getInductiveExitCondition();
        errs() << "\n";
    }

    bet.beginSimpleInductiveLoop(pn, tripCount);
}

static void CreateSimpleConditionalLoop(LoopWrapper& loop, const Value& condition, gla::BackEndTranslator& bet)
{
    BasicBlock* header = loop.getHeader();
    assert(loop.isLoopExiting(header) && (loop.contains(GetSuccessor(0, header))
                                          || loop.contains(GetSuccessor(1, header))));

    const CmpInst* cmp = dyn_cast<CmpInst>(&condition);
    assert(cmp && cmp->getNumOperands() == 2 && cmp == GetCondition(header));

    bet.beginSimpleConditionalLoop(cmp, cmp->getOperand(0), cmp->getOperand(1),
                                   loop.contains(GetSuccessor(1, header)));
}

void BottomTranslator::declarePhiCopies(const Function* function)
{
    if (! backEnd->getRemovePhiFunctions())
        return;

    // basic blocks
    for (Function::const_iterator bb = function->begin(), E = function->end(); bb != E; ++bb) {

        // instructions in the basic block
        for (BasicBlock::const_iterator i = bb->begin(), e = bb->end(); i != e; ++i) {
            const Instruction* llvmInstruction = i;

            if (llvmInstruction->getOpcode() == Instruction::PHI)
                backEndTranslator->declarePhiCopy(llvmInstruction);
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

    // for each llvm phi node, add a copy instruction
    for (BasicBlock::const_iterator i = nextBB->begin(), e = nextBB->end(); i != e; ++i) {
        const PHINode *phiNode = dyn_cast<PHINode>(i);

        if (phiNode) {
            addPhiCopy(phiNode, curBB);
        }
    }
}

template<unsigned Size>
void BottomTranslator::addPhiCopies(SmallPtrSet<const PHINode*, Size>& phis, const BasicBlock* curBB)
{
    for (typename SmallPtrSet<const PHINode*,Size>::iterator i = phis.begin(), e = phis.end(); i != e; ++i)
        addPhiCopy(*i, curBB);
}

void BottomTranslator::addPhiCopy(const PHINode* phi, const BasicBlock* curBB)
{
    // Find the operand whose predecessor is curBB.
    int predIndex = phi->getBasicBlockIndex(curBB);
    if (predIndex >= 0) {
        // then we found ourselves
        backEndTranslator->addPhiCopy(phi, phi->getIncomingValue(predIndex));
    }
}

void BottomTranslator::handleInstructions(const BasicBlock* bb)
{
    const LoopWrapper* loop = NULL;

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
        handleNonTerminatingInstructions(bb);
}

void BottomTranslator::handleNonTerminatingInstructions(const BasicBlock* bb)
{
    // Add the non-terminating instructions
    for (BasicBlock::const_iterator i = bb->begin(), e = bb->getTerminator(); i != e; ++i) {
        const Instruction* inst = i;

        if (! (backEnd->getRemovePhiFunctions() && inst->getOpcode() == Instruction::PHI))
            backEndTranslator->add(inst, lastBlock);
    }
}

void BottomTranslator::addInstructions(SmallVectorImpl<const Instruction*>& insts)
{
    for (SmallVectorImpl<const Instruction*>::iterator i = insts.begin(), e = insts.end(); i != e; ++i)
        if (! (backEnd->getRemovePhiFunctions() && (*i)->getOpcode() == Instruction::PHI))
            backEndTranslator->add(*i, lastBlock);
}

void BottomTranslator::newLoop(const BasicBlock* bb)
{
    assert(idStructs->getLoopFor(bb) && "newLoop called on non-loop");

    loops.push(idStructs->getLoopFor(bb));

    // We'll have to handle the latch specially if the backedge is not simple.
    if (! loops.top()->isSimpleLatching()) {
        handledBlocks.insert(loops.top()->getLatch());
    }
}

void BottomTranslator::attemptHandleDominatee(const BasicBlock* dominator, const BasicBlock* dominatee)
{
    // If the dominatee is a early return or a discard, then force it's output
    // accordingly
    if (dominatee == stageExit) {
        forceDiscard();
        return;
    }
    if (dominatee == stageEpilogue) {
        forceReturn();
        return;
    }

    if (ProperlyDominates(dominator, dominatee, *domTree)) {
        handleBlock(dominatee);
    }
}

void BottomTranslator::handleLoopBlock(const BasicBlock* bb, bool instructionsHandled)
{
    assert(loops.size() != 0 && "handleLoopBlock called on a new loop without newLoop being called");

    // TODO: Test nested loops thoroughly

    const Value* condition = GetCondition(bb);

    const LoopWrapper* loop  = loops.top();
    const BasicBlock* header = loop->getHeader();
    const BasicBlock* latch  = loop->getLatch();

    bool isHeader    = bb == header;
    bool isLatch     = bb == latch;
    bool isExiting   = loop->isExiting(bb);
    assert(isHeader || isLatch || isExiting);

    bool simpleLatch = loop->isSimpleLatching();

    // TODO: have calculating exit merge take into account when some of the
    // exit blocks merge into a return (that is, you can have return statements
    // inside loops, with some exiting blocks going to it).

    assert(! isLatch || simpleLatch); // isLatch => simpleLatch

    // If it's a loop header, have the back-end add it
    if (isHeader)
        setUpLoopBegin(condition);

    // If the branch is conditional and not a latch nor exiting, we're dealing
    // with conditional (e.g. if-then-else) flow control from a header.
    // Otherwise handle it's instructions ourselves.
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

    handleInstructions(latch);

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

    // We'd like to now shedule the handling of the merge block. If there is no
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
    backEndTranslator->add(bb->getTerminator(), lastBlock);

    return;
}

void BottomTranslator::handleBranchingBlock(const BasicBlock* bb)
{
    // Handle it's instructions and do phi node removal if appropriate
    handleInstructions(bb);
    addPhiCopies(bb);

    // If it's unconditional, we'll want to handle any subtrees (and introduced
    // latches/discards/returns) that it points to.
    if (IsUnconditional(bb)) {
        const BasicBlock* succ = GetSuccessor(0,bb);
        // If it's the (non-simple) latch, handle it, else handle it if we dominate it (also includes returns/discards)
        if (! loops.empty() && !loops.top()->isSimpleLatching() && succ == loops.top()->getLatch())
            handleBlock(succ);
        else
            attemptHandleDominatee(bb, succ);

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

void BottomTranslator::handleBlock(const BasicBlock* bb)
{
    assert(bb);

    // If handleBlock is called on a return or discard block, then perform the
    // copyout/return
    if (bb == stageExit || bb == stageEpilogue) {
        // Are we on the last block?
        if (handledBlocks.size() == numBBs)
            lastBlock = true;

        if (bb == stageExit)
            forceDiscard();
        else // bb == stageEpilogue
            forceReturn();

        return;
    }

    // If handleBlock is called on a non-simple latch, then force its output
    // to happen.
    // TODO: keep around a "lastLoopBlock" analog so that final continues need
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

    // TODO: We have a notion of exiting block, so we want to treat it
    // specially, we could also have a notion of latching/returning/discarding
    // block, something that is either a latch/return/discard, or may branch to
    // one. Consider if there's value to this notion, e.g. it can be used as
    // (part of) a heuristic for whether we want to have an early return or else
    // wrap a body of code in a conditional in the backend.

    // If the block exhibits loop-relevant control flow, handle it specially
    LoopWrapper* loop = idStructs->getLoopFor(bb);
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

    // Otherwise we're a block ending in a return statement
    assert(isa<ReturnInst>(bb->getTerminator()));
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
        if (&*i == loops.top()->getInductiveExitCondition() || &*i == loops.top()->getIncrement())
            continue;
        insts.push_back(i);
    }

    addInstructions(insts);
}

void BottomTranslator::setUpSimpleLatch()
{
    // If it's a latch, we must be simple and thus only need to add phi copies

    // Add phi copies (if applicable) excluding the one for the inductive variable
    SmallPtrSet<const PHINode*, 8> phis;
    GetPHINodes(loops.top()->getHeader(), phis);

    if (loops.top()->isSimpleInductive())
        phis.erase(loops.top()->getCanonicalInductionVariable());

    addPhiCopies(phis, loops.top()->getLatch());
}

void BottomTranslator::setUpLoopBegin(const Value* condition)
{
    assert(loops.size() != 0);
    LoopWrapper* loop = loops.top();

    // TODO: add more loop constructs here
    // TODO: stick body in here
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

    // Don't output if it's simple-conditional(and the header) or
    // simple-inductive (and given the inductive exit condition)
    bool shouldOutput = ! ((loop->isSimpleInductive() && condition == loop->getInductiveExitCondition())
                        || (loop->isSimpleConditional() && loop->isHeader(bb) ));

    int exitPos = loop->exitSuccNumber(bb);
    assert(exitPos != -1);
    if (exitPos == 2)
        gla::UnsupportedFunctionality("complex loop exits (two exit branches from same block)");

    const BasicBlock* exit      = GetSuccessor(exitPos, bb);
    assert(exit);

    // Set up the conditional, and add the exit block subgraph if it isn't
    // the exit merge.
    if (shouldOutput && condition)
        backEndTranslator->addIf(condition, exitPos == 1);

    // Add phi copies (if applicable)
    addPhiCopies(bb, exit);

    const BasicBlock* exitMerge = loop->getExitMerge();
    assert((exitMerge || loop->hasReturn() || loop->hasDiscard()) && "unstructured control flow");

    // Output the exit if we should
    if (shouldOutput) {
        if (handledBlocks.count(exit)) {
            gla::UnsupportedFunctionality("complex loop exits (shared exit block)");
        }

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
            backEndTranslator->addLoopExit();
        }

        if (condition)
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
    bool breakOp, continueOp, earlyReturnOp, discardOp;
    backEnd->getControlFlowMode(flowControlMode, breakOp, continueOp, earlyReturnOp, discardOp);
    if (flowControlMode == gla::EFcmExplicitMasking)
        gla::UnsupportedFunctionality("explicit masking in middle end");

    //
    // Translate named struct types.
    //
    const TypeSymbolTable& symbolTable = module.getTypeSymbolTable();
    for (TypeSymbolTable::const_iterator tableIter = symbolTable.begin(), tableEnd = symbolTable.end(); tableIter != tableEnd; ++tableIter)
        backEndTranslator->addStructType(tableIter->first, tableIter->second);

    //
    // Translate globals.
    //
    for (Module::const_global_iterator global = module.global_begin(), end = module.global_end(); global != end; ++global)
        backEndTranslator->addGlobal(global);

    //
    // Translate code.
    //
    Module::iterator function, lastFunction;
    for (function = module.begin(), lastFunction = module.end(); function != lastFunction; ++function) {
        if (function->isDeclaration()) {
            //?? do we need to handle declarations of functions, or just definitions?
        } else {
            assert (loops.size() == 0);

            // Get/set the loop info
            domTree   = &getAnalysis<DominatorTree>        (*function);
            domFront  = &getAnalysis<DominanceFrontier>    (*function);
            idStructs = &getAnalysis<IdentifyStructures>   (*function);

            // Set up the function info
            stageEpilogue = idStructs->getMainCopyOut();
            stageExit    = idStructs->getMainExit();

            handledBlocks.clear();
            handledBlocks.insert(stageEpilogue);
            handledBlocks.insert(stageExit);

            // handle function's with bodies

            backEndTranslator->startFunctionDeclaration(function->getFunctionType(), function->getNameStr());

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
            assert(IsSubset(function->getBasicBlockList(), handledBlocks));

            backEndTranslator->endFunctionBody();
        }
    }

    //set_branchtargets(&v, currentInstructionructions, num_instructions);
    backEndTranslator->print();

    return false;
}

void BottomTranslator::getAnalysisUsage(AnalysisUsage& AU) const
{
    AU.addRequired<DominatorTree>();
    AU.addRequired<DominanceFrontier>();
    AU.addRequired<IdentifyStructures>();
    // AU.addRequired<ScalarEvolution>();
    // AU.addRequired<LazyValueInfo>();

    AU.setPreservesAll();
}

char BottomTranslator::ID = 0;
INITIALIZE_PASS_BEGIN(BottomTranslator,
                      "bottom-transl",
                      "LunarGLASS bottom translator pass",
                      false,  // Whether it looks only at CFG
                      true); // Whether it is an analysis pass
INITIALIZE_PASS_DEPENDENCY(DominatorTree)
// INITIALIZE_PASS_DEPENDENCY(DominanceFrontier)
INITIALIZE_PASS_DEPENDENCY(IdentifyStructures)
INITIALIZE_PASS_END(BottomTranslator,
                    "bottom-transl",
                    "LunarGLASS bottom translator pass",
                    false,  // Whether it looks only at CFG
                    true); // Whether it is an analysis pass

static ModulePass* createBottomTranslatorPass(gla::BackEndTranslator* bet, gla::BackEnd* be)
{
    BottomTranslator* bot = new BottomTranslator();
    bot->setBackEndTranslator(bet);
    bot->setBackEnd(be);
    return bot;
}

// The below are the only externally exposed functionality

void gla::PrivateManager::translateBottomToTarget()
{
    if (! Options.bottomIROnly) {
        PassManager passManager;
        passManager.add(createBottomTranslatorPass(backEndTranslator, backEnd));
        passManager.run(*module);
    }
}

