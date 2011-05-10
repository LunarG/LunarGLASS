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

// TODO: - loops with return statements inside them

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
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"

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
#include "Passes/Analysis/IdentifyConditionals.h"
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

        void setLoopInfo(LoopInfo* li)                         { loopInfo = li; }
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

        LoopInfo* loopInfo;
        DominatorTree* domTree;
        DominanceFrontier* domFront;
        IdentifyConditionals* idConds;
        ScalarEvolution* scalarEvo;
        LazyValueInfo* lazyInfo;

        bool lastBlock;

        // How many basic blocks are in the current function being handled
        int numBBs;

        SmallPtrSet<const BasicBlock*, 8> handledBlocks;

        // Data for handling loops
        LoopStack* loops;

        // Set everything up for handling a new loop
        void newLoop(const BasicBlock*);

        // Reset loop data
        void closeLoop() { loops->pop(); };

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
        void handleLoopBlock(const BasicBlock*);

        // Given a block ending in return, output it and the return
        void handleReturnBlock(const BasicBlock*);

        // Handle non-loop control flow
        void handleBranchingBlock(const BasicBlock*);

        // Force the output of the current latch. Assumes it's only being called
        // when the source-level backedge is not preserved.
        void forceOutputLatch();

        // If dominator properly dominates dominatee, then handle it, else do nothing
        void attemptHandleDominatee(const BasicBlock* dominator, const BasicBlock* dominatee);

        // Sets up a the beginning of a loop for the loop at the top of our LoopStack.
        // Assumes there is something on top of our LoopStack.
        void setUpLoopBegin(const Value* condition);

        // Sets up the exit from the loop at the top of our LoopStack. Assumes
        // there is something on top of our LoopStack.
        void setUpLoopExit(const Value* condition, const BasicBlock* bb);

        // Sets up the preserved latch for the loop at the top of our
        // LoopStack. Assumes there is something on top of our LoopStack
        void setUpPreservedLatch();

    };

} // end namespace

static void CreateSimpleInductiveLoop(LoopWrapper& loop, gla::BackEndTranslator& bet)
{
    const PHINode* pn  = loop.getCanonicalInductionVariable();
    const Value* count = loop.getTripCount();
    assert(pn && count);

    int tripCount = gla::GetConstantInt(count);
    assert (tripCount  >= 0);

    if (gla::Options.debug) {
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

// Are all the blocks in the loop present in handledBlocks
template <unsigned SIZE>
static bool AreAllHandled(const LoopWrapper* loop, SmallPtrSet<const BasicBlock*, SIZE>& handledBlocks)
{
    for (Loop::block_iterator i = loop->block_begin(), e = loop->block_end(); i != e; ++i)
        if (! handledBlocks.count(*i)) {
            errs() << "Internal Error: Unhandled Basic Block "<< **i;
            return false;
        }

    return true;
}

template <unsigned SIZE>
static bool AreAllHandled(Function &f, SmallPtrSet<const BasicBlock*, SIZE>& handledBlocks)
{
    for (Function::const_iterator i = f.begin(), e = f.end(); i != e; ++i) {
        if (! handledBlocks.count(i)) {
            errs() << "Internal Error: Unhandled Basic Block "<< *i;
            return false;
        }
    }

    return true;
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

    if (! loops->empty())
        loop = loops->top();
    else {
        handleNonTerminatingInstructions(bb);
        return;
    }

    if (loop->isSimpleInductive() && (bb == loop->getLatch() || loop->isLoopExiting(bb)))
        // Simple inductive latch/exiting block
        handleSimpleInductiveInstructions(bb);
    else if (loop->isSimpleConditional() && bb == loop->getHeader())
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
    assert(loopInfo->getLoopFor(bb) && "newLoop called on non-loop");

    loops->pushNewLoop(bb);

    // We'll have to handle the latch specially if the backedge is not preserved.
    if (! loops->top()->preservesBackedge()) {
        handledBlocks.insert(loops->top()->getLatch());
    }
}

void BottomTranslator::attemptHandleDominatee(const BasicBlock* dominator, const BasicBlock* dominatee)
{
    if (ProperlyDominates(dominator, dominatee, *domTree)) {
        addPhiCopies(dominator, dominatee);
        handleBlock(dominatee);
    }
}

void BottomTranslator::handleLoopBlock(const BasicBlock* bb)
{
    assert(loops->size() != 0 && "handleLoopBlock called on a new loop without newLoop being called");

    // TODO: Test nested loops thoroughly

    const Value* condition = GetCondition(bb);

    const LoopWrapper* loop  = loops->top();
    const BasicBlock* header = loop->getHeader();
    const BasicBlock* latch  = loop->getLatch();

    bool isHeader  = bb == header;
    bool isLatch   = bb == latch;
    bool isExiting = loop->isLoopExiting(bb);
    assert(isHeader || isLatch || isExiting);

    bool preserved         = loop->preservesBackedge();

    // TODO: have calculating exit merge take into account when some of the
    // exit blocks merge into a return (that is, you can have return statements
    // inside loops, with some exiting blocks going to it).

    assert(! isLatch || preserved); // isLatch => preserved

    // If it's a loop header, have the back-end add it
    if (isHeader)
        setUpLoopBegin(condition);

    // If the branch is conditional and not a latch nor exiting, we're dealing
    // with conditional (e.g. if-then-else) flow control from a header.
    // Otherwise handle it's instructions ourselves.
    if (condition && ! (isLatch || isExiting)) {
        assert(idConds->getConditional(bb));
        handleBranchingBlock(bb);
    } else
        handleInstructions(bb);

    // If we're exiting, add the (possibly conditional) exit.
    if (isExiting) {
        setUpLoopExit(condition, bb);
    }

    if (isLatch) {
        assert(preserved);
        assert(( !condition || isExiting) && "redundant assertion failed");

        setUpPreservedLatch();
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
    assert(AreAllHandled(loop, handledBlocks));

    const BasicBlock* exitMerge = loop->getExitMerge();

    // TODO: hoist into own method
    // If we're simple conditional (and still a header), handle our exit block
    if (loop->isSimpleConditional()) {
        int exitPos = loop->exitSuccNumber(bb);
        assert(exitPos != -1);
        if (exitPos == 2)
            gla::UnsupportedFunctionality("complex loop exits (two exit branches from same block)");

        const BasicBlock* exitBlock = GetSuccessor(exitPos, bb);

        if (exitBlock != exitMerge) {
            if (handledBlocks.count(exitBlock)) {
                gla::UnsupportedFunctionality("complex loop exits (shared exit block) [2]");
            }

            handleBlock(exitBlock);
        }
    }

    backEndTranslator->endLoop();
    closeLoop();

    // Schedule the handling of the exit merge block now, to make sure it occurs
    // immediately. It should be ok to immediately schedule exitMerge, because
    // the exitMerge should be dominated by the loop header.
    assert(ProperlyDominates(header, exitMerge, *domTree));
    handleBlock(exitMerge);

    return;
}

void BottomTranslator::forceOutputLatch()
{
    const BasicBlock* latch = loops->top()->getLatch();
    bool preserved  = loops->top()->preservesBackedge();

    assert(latch && !preserved);
    assert(handledBlocks.count(latch));

    assert(IsUnconditional(latch));

    handleInstructions(latch);

    addPhiCopies(latch);
}

void BottomTranslator::handleIfBlock(const BasicBlock* bb)
{

    const Conditional* cond = idConds->getConditional(bb);

    // If we don't have a conditional entry for bb, then we're dealing with
    // conditionals with backedges/exits and other tricky control flow in
    // them. If they weren't dispatched to handleLoopBlock, then that means that
    // it's not exiting, and thus we must be a branch to a non-preserved latch
    if (! cond) {
        gla::UnsupportedFunctionality("complex continues in loops");

        // TODO: have idconditionals recognizing latching conditionals, then add
        // support for complex continues.

        return;
    }

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

    // We'd like to now shedule the handling of the merge block, just in case
    // the order we get the blocks in doesn't have it next.
    handleBlock(cond->getMergeBlock());

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
    // latches) that it points to.
    if (IsUnconditional(bb)) {
        if (domTree->dominates(bb, GetSuccessor(0, bb))
            || (loops->size() && !loops->top()->preservesBackedge() && GetSuccessor(0, bb) == loops->top()->getLatch()))
            handleBlock(GetSuccessor(0, bb));

        return;
    }

    handleIfBlock(bb);
    return;
}

void BottomTranslator::handleBlock(const BasicBlock* bb)
{
    // TODO: eliminate the below, replacing it with an assert, and do
    // checking/special handling in the callers of handleBlock
    if (!bb)
        return;

    // If handleBlock is called on a non-preserved latch, then force its output
    // to happen.
    // TODO: keep around a "lastLoopBlock" member so that final continues need
    // not be printed out
    if (loops->size() && loops->top()->getLatch() == bb && !loops->top()->preservesBackedge()) {
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

    // If the block exhibits loop-relevant control flow, handle it specially
    // TODO: change the below to use LoopUtil (and extend functionality in LoopUtil)
    Loop* loop = loopInfo->getLoopFor(bb);
    if (loop && (loop->getHeader() == bb || loop->getLoopLatch() == bb || loop->isLoopExiting(bb))
             && flowControlMode == gla::EFcmStructuredOpCodes) {

        if (loop->getHeader() == bb)
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
    assert(! loops->empty());

    // If we're simple inductive, then don't do the instructions computing
    // the inductive variable or the exit condition

    SmallVector<const Instruction*, 32> insts;
    const Instruction* lastInst  = bb->getTerminator();

    for (BasicBlock::const_iterator i = bb->begin(); &*i != lastInst; ++i) {
        if (&*i == loops->top()->getInductiveExitCondition() || &*i == loops->top()->getIncrement())
            continue;
        insts.push_back(i);
    }

    addInstructions(insts);
}

void BottomTranslator::setUpPreservedLatch()
{
    // If it's a latch, we must be preserved and thus only need to add phi copies

    // Add phi copies (if applicable) excluding the one for the inductive variable
    SmallPtrSet<const PHINode*, 8> phis;
    GetPHINodes(loops->top()->getHeader(), phis);

    if (loops->top()->isSimpleInductive())
        phis.erase(loops->top()->getCanonicalInductionVariable());

    addPhiCopies(phis, loops->top()->getLatch());
}

void BottomTranslator::setUpLoopBegin(const Value* condition)
{
    assert(loops->size() != 0);
    LoopWrapper* loop = loops->top();

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
    assert(loops->size() != 0);
    LoopWrapper* loop = loops->top();

    assert(loop->isLoopExiting(bb));

    const BasicBlock* exitMerge = loop->getExitMerge();
    if (! exitMerge)
        gla::UnsupportedFunctionality("complex exits/continues in loops");

    // Don't output if it's simple-conditional(and the header) or
    // simple-inductive (and given the inductive exit condition)
    bool shouldOutput = ! ((loop->isSimpleInductive() && condition == loop->getInductiveExitCondition())
                        || (loop->isSimpleConditional() && bb == loop->getHeader() ));

    int exitPos = loop->exitSuccNumber(bb);
    assert(exitPos != -1);
    if (exitPos == 2)
        gla::UnsupportedFunctionality("complex loop exits (two exit branches from same block)");

    const BasicBlock* exit      = GetSuccessor(exitPos, bb);

    // Set up the conditional, and add the exit block subgraph if it isn't
    // the exit merge.
    if (shouldOutput && condition)
        backEndTranslator->addIf(condition, exitPos == 1);

    // Add phi copies (if applicable)
    addPhiCopies(bb, exit);

    // Output the exit if we should
    if (shouldOutput) {
        if (handledBlocks.count(exit)) {
            gla::UnsupportedFunctionality("complex loop exits (shared exit block)");
        }

        if (exit != exitMerge) {
            handleBlock(exit);
        }

        backEndTranslator->addLoopExit();

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
    loops = new LoopStack();

    //
    // Query the back end about its flow control
    //
    bool breakOp, continueOp, earlyReturnOp, discardOp;
    backEnd->getControlFlowMode(flowControlMode, breakOp, continueOp, earlyReturnOp, discardOp);
    if (flowControlMode == gla::EFcmExplicitMasking)
        gla::UnsupportedFunctionality("explicit masking in middle end");

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

            // Get/set the loop info
            loopInfo  = &getAnalysis<LoopInfo>             (*function);
            domTree   = &getAnalysis<DominatorTree>        (*function);
            domFront  = &getAnalysis<DominanceFrontier>    (*function);
            idConds   = &getAnalysis<IdentifyConditionals> (*function);
            // scalarEvo = &getAnalysis<ScalarEvolution>      (*function);
            // lazyInfo  = &getAnalysis<LazyValueInfo>        (*function);

            loops->setDominanceFrontier(domFront);
            loops->setLoopInfo(loopInfo);
            // loops->setScalarEvolution(scalarEvo);

            // debug stuff
            if (gla::Options.debug && loopInfo->begin() != loopInfo->end()) {
                errs() << "\n\nLoop info:\n";        loopInfo->print(errs());
                // errs() << "\n\nScalar evolution:\n"; scalarEvo->print(errs());
            }

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
            assert(AreAllHandled(*function, handledBlocks));

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
    AU.addRequired<LoopInfo>();
    AU.addRequired<DominanceFrontier>();
    AU.addRequired<IdentifyConditionals>();
    // AU.addRequired<ScalarEvolution>();
    // AU.addRequired<LazyValueInfo>();

    AU.setPreservesAll();
}

char BottomTranslator::ID = 0;
INITIALIZE_PASS_BEGIN(BottomTranslator,
                      "bottom-transl",
                      "LunarGLASS bottom translator pass",
                      true,   // Whether it preserves the CFG
                      true); // Whether it is an analysis pass
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_DEPENDENCY(DominatorTree)
// INITIALIZE_PASS_DEPENDENCY(DominanceFrontier)
INITIALIZE_PASS_DEPENDENCY(IdentifyConditionals)
INITIALIZE_PASS_END(BottomTranslator,
                    "bottom-transl",
                    "LunarGLASS bottom translator pass",
                    true,   // Whether it preserves the CFG
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
    PassManager passManager;
    passManager.add(createBottomTranslatorPass(backEndTranslator, backEnd));
    passManager.run(*module);
}

