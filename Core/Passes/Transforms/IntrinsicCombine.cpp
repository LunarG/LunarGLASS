//===- IntrinsicCombine.cpp - Canonicalize instructions for LunarGLASS ---===//
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
// Combine and optimize over intrinsics to form fewer, simpler
// instructions. Analogous to InstCombine. Currently needs a BackEndPointer to
// do anything useful.
//
//   * Any instruction dominated or post-dominated by discard is DCEed
//
//   * Change (hoist) discards into discardConditionals which will reside in the
//     post-dominance frontier. TODO: place these discards right after the
//     condition is computed. TODO: only do based on backend query. TODO:
//     migrate the condition as high as it can go.
//
//   * Break up writeData/fWriteData of multi-inserts into multiple masked
//     writeData/fWriteDatas.
//
//   * Intrinsic elimination/evaluation
//     * Idempotent binary operators:
//         [Max|Min](a,a) ==> a
//     * Functions that return a constant over the same input:
//         Distance(a,a) ==> 0
//     * Partial evaluation of intrinsics
//         multiInsert(..., <c0 ... >, n, ...) ==> multiInsert(..., c_n, 0,...)
//
//
//   * TODO: Combine multiple successive fWrites to the same output but with
//     different masks into a single fWrite.
//
//   * TODO: hoist all reading of inputs or instructions involving constants
//     into the header, or at the very least try to migrate discard as far
//     upwards as possible
//
//   * TODO: various inter-intrinsic optimizations
//
//===----------------------------------------------------------------------===//

#include "llvm/GlobalVariable.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/Use.h"
#include "llvm/User.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "Passes/PassSupport.h"
#include "Passes/Immutable/BackEndPointer.h"
#include "Passes/Util/ConstantUtil.h"
#include "Passes/Util/DominatorsUtil.h"
#include "Passes/Util/FunctionUtil.h"
#include "Passes/Util/InstructionUtil.h"

// LunarGLASS helpers
#include "Exceptions.h"
#include "LunarGLASSTopIR.h"
#include "Util.h"


using namespace llvm;
using namespace gla_llvm;

namespace  {
    class IntrinsicCombine : public FunctionPass {
    public:
        static char ID;

        IntrinsicCombine() : FunctionPass(ID), numDiscardDCE(0)
        {
            initializeIntrinsicCombinePass(*PassRegistry::getPassRegistry());
        }

        virtual bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;

    private:
        // Gather all the (non-comparision) discards in the function. Returns
        // whether any were found.
        bool getDiscards(Function&);

        // Anything dominated or postdominated by a discard is dead.
        bool discardAwareDCE(Function&);

        // Discards can be turned into discardConditionals based on the
        // condition needed to reach them. The comparison comes from the
        // post-dominance frontier, and the discardConditional can go there
        // right before the branch. The branch then becomes an unconditional
        // branch to the non-discarding path.
        bool hoistDiscards(Function&);

        // Split up write-data of a multi-insert into multiple masked
        // write-datas.
        bool splitWriteData(IntrinsicInst* inst);

        // Visit an instruction, trying to optimize it
        bool visit(Instruction*);

        // Try to evaluate/eliminate the intrinsic call if possible
        bool evaluateIntrinsic(IntrinsicInst* intr);

        // Partial evaluations

        // MultiInsert
        bool partiallyEvaluateMultiInsert(IntrinsicInst* miIntr);

        // Empty out the deadList
        void emptyDeadList();

        typedef SmallVector<Instruction*, 4> DiscardList;

        // Hold on to a list of our discards
        DiscardList discards;

        // When combining/optimizing intrinsics, use the deadList to keep around
        // instructions to remove from the function. This allows iterators to be
        // preserved when iterating over instructions.
        std::vector<Instruction*> deadList;

        DominatorTree* domTree;
        PostDominatorTree* postDomTree;
        PostDominanceFrontier* postDomFront;

        Module* module;
        LLVMContext* context;

        BackEnd* backEnd;

        // Statistic info
        int numDiscardDCE;
        int numCombined;

        IntrinsicCombine(const IntrinsicCombine&); // do not implement
        void operator=(const IntrinsicCombine&); // do not implement
    };
} // end namespace

void IntrinsicCombine::emptyDeadList()
{
    for (std::vector<Instruction*>::iterator i = deadList.begin(), e = deadList.end(); i != e; ++i)
        (*i)->eraseFromParent();
}

bool IntrinsicCombine::getDiscards(Function& F)
{
    BasicBlock* exit = GetMainExit(F);
    assert(exit && "non-exiting shader, or non-canonicalized CFG");

    // Discards exist in exit predecessors
    for (pred_iterator bbI = pred_begin(exit), e = pred_end(exit); bbI != e; ++bbI)
        for (BasicBlock::iterator instI = (*bbI)->begin(), instE = (*bbI)->end(); instI != instE; ++instI)
            if (IsDiscard(instI))
                discards.push_back(instI);

    return ! discards.empty();
}


bool IntrinsicCombine::discardAwareDCE(Function& F)
{
    // TODO: Revise for side-effects that aren't killed by a discard

    // Build up deadList to be all the dominated and post-dominated instructions
    for (DiscardList::iterator i = discards.begin(), e = discards.end(); i != e; ++i) {
        GetAllDominatedInstructions(*i, *domTree->DT, deadList);
        // GetAllDominatedInstructions(*i, *postDomTree->DT, deadList); // See TODO
    }

    bool changed = deadList.empty();
    numDiscardDCE = deadList.size();

    // DCE: drop references.
    for (std::vector<Instruction*>::iterator i = deadList.begin(), e = deadList.end(); i != e; ++i) {
        (*i)->replaceAllUsesWith(UndefValue::get((*i)->getType()));
        (*i)->dropAllReferences();
    }

    // DCE: erase
    emptyDeadList();

    return changed;
}

bool IntrinsicCombine::hoistDiscards(Function& F)
{
    if (! backEnd->hoistDiscards()) {
        return false;
    }

    IRBuilder<> builder(*context);
    for (DiscardList::iterator i = discards.begin(), e = discards.end(); i != e; ++i) {
        PostDominanceFrontier::DomSetType pds = postDomFront->find((*i)->getParent())->second;
        assert(pds.size() == 1 && "Unknown flow control layout or unstructured flow control");

        BasicBlock* targetBlock = *pds.begin();
        BranchInst* br = dyn_cast<BranchInst>(targetBlock->getTerminator());
        assert(br && br->isConditional());

        // Find which branch is the discard
        bool isLeft = domTree->dominates(br->getSuccessor(0), (*i)->getParent());
        bool isRight = domTree->dominates(br->getSuccessor(1), (*i)->getParent());
        assert(! (isLeft && isRight) && "improper post-dominance frontier discovered");

        Value* cond = br->getCondition();

        builder.SetInsertPoint(br);

        // If we're the right branch, then we should negate the condition before
        // feeding it into discardConditional
        if (isRight) {
            cond = builder.CreateNot(cond);
        }

        const Type* boolTy = gla::GetBoolType(*context);
        builder.CreateCall(Intrinsic::getDeclaration(module, Intrinsic::gla_discardConditional, &boolTy, 1 ),
                           cond);

        // Make the branch now branch on a constant
        br->setCondition(isRight ? ConstantInt::getTrue(*context) : ConstantInt::getFalse(*context));

        // DCE the old discard
        (*i)->dropAllReferences();
        (*i)->eraseFromParent();
    }

    bool changed = ! discards.empty();
    discards.clear();

    return changed;
}

// Split up a write-data of a multi-inserts into multiple masked write-datas.
bool IntrinsicCombine::splitWriteData(IntrinsicInst* intr)
{
    assert(intr);

    if (! backEnd->splitWrites())
        return false;

    if (! IsOutputInstruction(intr))
        return false;

    ConstantInt* wm = dyn_cast<ConstantInt>(intr->getOperand(1));
    assert(wm && "Non-constant int writemask?");

    // Now see if the data is the result of a multi-insert
    Instruction* miInst = dyn_cast<Instruction>(intr->getOperand(2));
    if (! miInst || ! IsMultiInsert(miInst))
        return false;

    // Handle when the write-data already has a write mask
    if (wm != ConstantInt::get(wm->getType(), -1)) {
        // TODO: when the write-data already has a write mask
        return false;
    }

    SmallVector<Constant*, 4> selects;
    GetMultiInsertSelects(miInst, selects);

    SmallVector<Value*, 4> components;
    components.push_back(miInst->getOperand(2));
    components.push_back(miInst->getOperand(4));
    components.push_back(miInst->getOperand(6));
    components.push_back(miInst->getOperand(8));

    bool areAllScalar = AreScalar(components);
    if (! areAllScalar || IsDefined(miInst->getOperand(0))) {
        // TODO: non-scalar cases (including multi-insert into a value)
        return false;
    }

    IRBuilder<> builder(intr);
    for (unsigned int i = 0; i < 4; ++i) {
        if (! MultiInsertWritesComponent(miInst, i))
            continue;

        Value* arg = components[i];
        const Type* ty = arg->getType();
        Function* writeData = Intrinsic::getDeclaration(module, intr->getIntrinsicID(), &ty, 1);
        builder.CreateCall3(writeData, intr->getArgOperand(0), ConstantInt::get(wm->getType(), 1 << i), arg);
    }

    // Delete the old write
    intr->dropAllReferences();
    intr->eraseFromParent();

    return true;
}


bool IntrinsicCombine::runOnFunction(Function& F)
{
    releaseMemory();

    BackEndPointer* bep = getAnalysisIfAvailable<BackEndPointer>();
    if (! bep)
        return false;

    backEnd = *bep;

    domTree = &getAnalysis<DominatorTree>();
    postDomTree = &getAnalysis<PostDominatorTree>();
    postDomFront = &getAnalysis<PostDominanceFrontier>();

    module  = F.getParent();
    context = &F.getContext();

    bool changed = false;

    bool hasDiscards = getDiscards(F);

    // Perform discard optimizations (if present)
    if (hasDiscards) {
        changed |= discardAwareDCE(F);
        changed |= hoistDiscards(F);
    }

    // Visit each instruction, trying to optimize
    for (Function::iterator bbI = F.begin(), bbE = F.end(); bbI != bbE; ++bbI) {
        for (BasicBlock::iterator instI = bbI->begin(), instE = bbI->end(); instI != instE; /* empty */) {
            BasicBlock::iterator prev = instI;
            ++instI;
            changed |= visit(prev);
        }
    }

    emptyDeadList();

    return changed;
}

bool IntrinsicCombine::visit(Instruction* inst)
{
    IntrinsicInst* intr = dyn_cast<IntrinsicInst>(inst);
    if (! intr) {
        return false;
    }

    // See if we can trivially eliminate/evaluate the call
    if (evaluateIntrinsic(intr))
        return true;

    // Try to combine it
    // TODO: intrinsic combining

    // Write splitting
    if (splitWriteData(intr))
        return true;

    return false;
}

bool IntrinsicCombine::evaluateIntrinsic(IntrinsicInst* intr)
{
    Intrinsic::ID id = intr->getIntrinsicID();
    Value* result = 0;

    switch (id) {

    // Idempotent binary intrinsics
    case Intrinsic::gla_sMin:
    case Intrinsic::gla_uMin:
    case Intrinsic::gla_fMin:
    case Intrinsic::gla_sMax:
    case Intrinsic::gla_uMax:
    case Intrinsic::gla_fMax:
        if (intr->getArgOperand(0) == intr->getArgOperand(1))
            result = intr->getArgOperand(0);

        break;

    // Binary intrinsics that, when passed the same argument, return 0
    case Intrinsic::gla_fDistance:
        if (intr->getArgOperand(0) == intr->getArgOperand(1))
            result = Constant::getNullValue(intr->getType());

        break;

    // Partial evaluations:

    //  multiInsert
    case Intrinsic::gla_multiInsert:
    case Intrinsic::gla_fMultiInsert:
        return partiallyEvaluateMultiInsert(intr);

    } // end of switch (id)

    if (! result)
        return false;

    // Replace our call with our evaluated result
    intr->replaceAllUsesWith(result);
    intr->dropAllReferences();
    deadList.push_back(intr);

    return true;
}

bool IntrinsicCombine::partiallyEvaluateMultiInsert(IntrinsicInst* miIntr)
{
    // If any of the sources are constants of vector type, then we can replace
    // the source with the scalar component

    // Pair of constant argument, operand index
    typedef std::pair<Constant*, int> ConstOp;

    SmallVector<ConstOp, 4> constantVectorSources;
    unsigned wmask = GetConstantInt(miIntr->getOperand(1));
    for (int i = 0; i < 4; ++i) {
        if (! MultiInsertWritesComponent(wmask, i))
            continue;

        int operandIndex = (i+1) * 2;

        if (Constant* constantSource = dyn_cast<Constant>(miIntr->getArgOperand(operandIndex))) {
            if (constantSource->getType()->isVectorTy()) {
                constantVectorSources.push_back(std::make_pair(constantSource, operandIndex));
            }
        }
    }

    if (! constantVectorSources.size())
        return false;

    const FunctionType* miTypes = miIntr->getCalledFunction()->getFunctionType();
    const Type* declTys[6] = { miTypes->getReturnType(),
                               miTypes->getParamType(0),
                               miTypes->getParamType(2),
                               miTypes->getParamType(4),
                               miTypes->getParamType(6),
                               miTypes->getParamType(8),
    };

    for (SmallVector<ConstOp, 4>::iterator i = constantVectorSources.begin(), e = constantVectorSources.end();
         i != e; ++i) {

        // Find the component's value
        int component = gla::GetConstantInt(miIntr->getArgOperand(i->second + 1));
        Constant* constant = GetComponentFromConstant(i->first, component);
        assert(gla::IsScalar(constant));

        // Set it, updating the select bit
        miIntr->setArgOperand(i->second, constant);
        miIntr->setArgOperand(i->second+1, Constant::getNullValue(Type::getInt32Ty(*context)));

        // Update the function's parameter type
        assert(i->second % 2 == 0 && i->second >= 2 && i->second <= 8);
        declTys[i->second / 2 + 1] = constant->getType();
    }

    Function* newDecl = Intrinsic::getDeclaration(module, miIntr->getIntrinsicID(), declTys, 6);
    miIntr->setCalledFunction(newDecl);

    return true;
}


void IntrinsicCombine::getAnalysisUsage(AnalysisUsage& AU) const
{
    AU.addRequired<DominatorTree>();
    AU.addRequired<PostDominatorTree>();
    AU.addRequired<PostDominanceFrontier>();
    return;
}

void IntrinsicCombine::print(raw_ostream&, const Module*) const
{
    return;
}


char IntrinsicCombine::ID = 0;
INITIALIZE_PASS_BEGIN(IntrinsicCombine,
                      "intrinsic-combine",
                      "Combine intrinsics for LunarGLASS",
                      false,  // Whether it looks only at CFG
                      false); // Whether it is an analysis pass
INITIALIZE_PASS_DEPENDENCY(DominatorTree)
INITIALIZE_PASS_DEPENDENCY(PostDominatorTree)
INITIALIZE_PASS_DEPENDENCY(PostDominanceFrontier)
INITIALIZE_PASS_END(IntrinsicCombine,
                    "intrinsic-combine",
                    "Combine intrinsics for LunarGLASS",
                    false,  // Whether it looks only at CFG
                    false); // Whether it is an analysis pass

FunctionPass* gla_llvm::createIntrinsicCombinePass()
{
    return new IntrinsicCombine();
}

