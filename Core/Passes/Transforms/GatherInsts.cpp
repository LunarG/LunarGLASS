//===- GatherInsts.cpp - Gather multiple instructions into intrinsics -----===//
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
// Gather llvm instructions and LunarGLASS intrinsics into a single LunarGLASS
// intrinsic when able.
//
//   * trunc([mul|add|sub](ext(x), ext(y))); trunc([mul|add|sub](ext(x), ext(y)) >> 32)
//                          ==> [mul|add|sub][Extended|Carry|Borrow](x,y)
//
//   * div(x, y); rem(x, y) ==> ??? new div/rem combination ???
//
//   * fMax(fMin(0, x), 1)  ==> fSaturate(x)
//
//   * fMax(fMin(x, y), z)  ==> fClamp(x, y, z)
//
//
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Utils/Local.h"
#include "llvm/Pass.h"

#include "Passes/PassSupport.h"
#include "Passes/Immutable/BackEndPointer.h"
#include "Passes/Util/ConstantUtil.h"

// LunarGLASS helpers
#include "Exceptions.h"
#include "LunarGLASSTopIR.h"
#include "Util.h"

using namespace llvm;
using namespace gla_llvm;

namespace  {
    class GatherInsts : public FunctionPass {
    public:
        static char ID;

        GatherInsts() : FunctionPass(ID)
        {
            initializeGatherInstsPass(*PassRegistry::getPassRegistry());
        }

        virtual bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;

    private:
        // Delete the instructions in the deadSet
        bool dce(Function& F);

        // Optimize min(max(a,b),c) into saturate or clamp
        bool visitMinMaxPair(IntrinsicInst* fMin, IntrinsicInst* fMax);

        bool visit(Instruction* inst);
        bool visitIntrinsic(IntrinsicInst* intr);



        // Pattern matching methods

        // Try to match an extended binary operator (e.g. mulExteneded,
        // addCarry) pattern. Takes the bin-op as input. Returns whether the
        // bin-op matches a bin-op-extended pattern, along with whether it is
        // signed and the llvm values for the original (i32) operands and high
        // and low bits in the passed parameters
        // operand0/operand1/high/lowBits/isSigned, when present.
        bool matchBinOpExtended(/* inputs */  const BinaryOperator* binOp,
                               /* outputs */ Value*& operand0, Value*& operand1, Value*& highBits, Value*& lowBits,
                                             bool& isSigned);


        // Creator helpers

        bool createExtendedBinOp(Intrinsic::ID extId, BinaryOperator* binOp, Value* operand0, Value* operand1,
                                 Value* highBits, Value* lowBits);


        // Whether the passed value is a trunc from i64->i32
        bool is64To32BitTrunc(const TruncInst* trunc) const
        {
            return trunc && gla::GetBasicType(trunc->getSrcTy()) == i64Ty
                && gla::GetBasicType(trunc->getDestTy()) == i32Ty;
        }
        bool is64To32BitTrunc(const Value* truncInst) const
        {
            const TruncInst* trunc = dyn_cast<const TruncInst>(truncInst);
            return is64To32BitTrunc(trunc);
        }

        bool isExtendedOp(const Instruction* inst) const
        {
            return isExtendableOpcode(inst->getOpcode()) && gla::GetBasicType(inst->getType()) == i64Ty;
        }

        bool isExtendableOpcode(unsigned opcode) const
        {
            return opcode == Instruction::Mul || opcode == Instruction::Add || opcode == Instruction::Sub;
        }

        Intrinsic::ID getExtendedIntrinsicID(unsigned opcode, bool isSigned) const
        {
            assert(isExtendableOpcode(opcode));

            switch (opcode) {
            case Instruction::Add:
                if (isSigned)
                    gla::UnsupportedFunctionality("Signed addCarry");

                return Intrinsic::gla_addCarry;

            case Instruction::Sub:
                if (isSigned)
                    gla::UnsupportedFunctionality("Signed subBorrow");

                return Intrinsic::gla_subBorrow;

            case Instruction::Mul:
                return isSigned ? Intrinsic::gla_smulExtended : Intrinsic::gla_umulExtended;

            default:
                return Intrinsic::not_intrinsic;

            } // end of switch (opcode)
        }

        Module* module;
        LLVMContext* context;

        BackEnd* backEnd;
        IRBuilder<>* builder;

        const Type* i32Ty;
        const Type* i64Ty;

        GatherInsts(const GatherInsts&); // do not implement
        void operator=(const GatherInsts&); // do not implement
    };
} // end namespace

bool GatherInsts::dce(Function& F)
{
    bool changed = false;

    // (recursively) delete the trivially dead instructions. This is safe to do
    // recursively (iterators will remain valid) because we only delete prior
    // instructions in the basic block, i.e. the use is deleted which may enable
    // the def (which must precede/dominate the use) to be deleted.
    for (Function::iterator bbI = F.begin(), bbE = F.end(); bbI != bbE; ++bbI) {
        for (BasicBlock::iterator instI = bbI->begin(), instE = bbI->end(); instI != instE; /* empty */) {
            BasicBlock::iterator prev = instI;
            ++instI;
            changed |= RecursivelyDeleteTriviallyDeadInstructions(prev);
        }
    }

    return changed;
}

bool GatherInsts::runOnFunction(Function& F)
{
    releaseMemory();

    BackEndPointer* bep = getAnalysisIfAvailable<BackEndPointer>();
    if (! bep)
        return false;

    backEnd = *bep;

    module  = F.getParent();
    context = &F.getContext();
    builder = new IRBuilder<>(*context);

    i32Ty = IntegerType::get(*context, 32);
    i64Ty = IntegerType::get(*context, 64);

    bool changed = false;

    // Visit each instruction, trying to optimize
    for (Function::iterator bbI = F.begin(), bbE = F.end(); bbI != bbE; ++bbI) {
        for (BasicBlock::iterator instI = bbI->begin(), instE = bbI->end(); instI != instE; /* empty */) {
            BasicBlock::iterator prev = instI;
            ++instI;
            changed |= visit(prev);
        }
    }

    changed |= dce(F);

    delete builder;
    builder = 0;

    return changed;
}

bool GatherInsts::matchBinOpExtended(const BinaryOperator* binOp, Value*& operand0, Value*& operand1,
                                   Value*& highBits, Value*& lowBits, bool& isSigned)
{
    // Match:
    //   %lhs = [s|z]ext i32 %operand0, i64
    //   %rhs = [s|z]ext i32 %operand1, i64
    //   %res = binOp i64 %lhs, i64 %rhs
    //   %res_lo = trunc i64 %res, i32
    //   %shift = [l|a]shr i64 %res, 32
    //   %res_hi = trunc i64 %shift, i32
    // modulo vector-ness

    lowBits = 0;
    highBits = 0;
    operand0 = 0;
    operand1 = 0;

    // // Has to be an extendable 64-bit integral binOp.
    if (! isExtendedOp(binOp))
        return false;

    // Operands have to be extended i32 -> i64 and they have to match in their
    // extension (signed vs zero).
    for (unsigned int i = 0; i < binOp->getNumOperands(); ++i) {
        Value* v = binOp->getOperand(i);

        if (const CastInst* ext = dyn_cast<CastInst>(v)) {
            if ((ext->getOpcode() != Instruction::SExt && ext->getOpcode() != Instruction::ZExt)
               || gla::GetBasicType(ext->getSrcTy()) != i32Ty || gla::GetBasicType(ext->getDestTy()) != i64Ty)
                return false;

            (i == 0 ? operand0 : operand1) = ext->getOperand(0);

            isSigned = ext->getOpcode() == Instruction::SExt;

        } else if (const Constant* cArg = dyn_cast<const Constant>(v)) {
            // TODO: handle the case where a constant made its way into the
            // extended binOp.
            gla::UnsupportedFunctionality("extended binOp of a constant");
        } else {
            return false;
        }
    }

    // Operands have to be extended similarly with respect to sign
    const Instruction* op0Inst = dyn_cast<const Instruction>(binOp->getOperand(0));
    const Instruction* op1Inst = dyn_cast<const Instruction>(binOp->getOperand(1));
    if (op0Inst && op1Inst && op0Inst->getOpcode() != op1Inst->getOpcode())
        return false;

    // The binOp's result has to be immediately broken apart into its high
    // and/or low bits
    for (Value::const_use_iterator i = binOp->use_begin(), e = binOp->use_end(); i != e; ++i) {
        const Instruction* use = dyn_cast<const Instruction>(*i);
        if (! use)
            return false;

        // Try low bits
        if (is64To32BitTrunc(*i)) {
            // Has to be the (only) access of the lower 32 bits
            if (lowBits)
                return false;

            lowBits = const_cast<User*>(*i);

            continue;
        }

        // Try high bits

        // Has to be the only access of the high bits
        if (highBits)
            return false;

        const BinaryOperator* shift = dyn_cast<const BinaryOperator>(*i);
        if (! shift || (shift->getOpcode() != Instruction::AShr && shift->getOpcode() != Instruction::LShr))
            return false;

        // Has to be have a constant shift amount, equal across all components
        // if a vector.
        const Constant* shiftBy = dyn_cast<const Constant>(shift->getOperand(1));
        if (! shiftBy)
            return false;

        if (const ConstantVector* shiftByVec = dyn_cast<ConstantVector>(shiftBy)) {
            shiftBy = shiftByVec->getSplatValue();
            if (! shiftBy)
                return false;
        }

        const ConstantInt* shiftByInt = dyn_cast<const ConstantInt>(shiftBy);
        if (! shiftByInt || ! shiftByInt->equalsInt(32))
            return false;

        // The shift has to only be used by a trunc i64 -> i32
        if (! shift->hasOneUse() || ! is64To32BitTrunc(shift->use_back()))
            return false;

        highBits = const_cast<Instruction*>(shift->use_back());
    }

    // We've identified an extended binary op pattern
    return true;
}



bool GatherInsts::visitMinMaxPair(IntrinsicInst* fMin, IntrinsicInst* fMax)
{
    assert(fMin->getIntrinsicID() == Intrinsic::gla_fMin && fMax->getIntrinsicID() == Intrinsic::gla_fMax);
    assert(fMax->hasOneUse() && fMax->use_back() == fMin); //TODO: max(a, min(b,c))

    builder->SetInsertPoint(fMin);

    // Try to get constant operands for min and max, to see if we can make a
    // saturate
    Constant* minC0 = dyn_cast<Constant>(fMin->getArgOperand(0));
    Constant* minC1 = dyn_cast<Constant>(fMin->getArgOperand(1));
    if (minC0 && minC1) {
        gla::UnsupportedFunctionality("fMin should get constant-folded", EATContinue);
    }

    Constant* maxC0 = dyn_cast<Constant>(fMax->getArgOperand(0));
    Constant* maxC1 = dyn_cast<Constant>(fMax->getArgOperand(1));
    if (maxC0 && maxC1) {
        gla::UnsupportedFunctionality("fMax should get constant-folded", EATContinue);
    }

    // See if we can form a saturate instead of a clamp
    if ((minC0 || minC1) && (maxC0 || maxC1)) {
        // The value that may be saturated
        Value* saturateCandidate = 0;

        if (maxC0 && (maxC0->isNullValue() || maxC0->isNegativeZeroValue())) {
            saturateCandidate = fMax->getArgOperand(1);
        } else if (maxC1 && (maxC1->isNullValue() || maxC1->isNegativeZeroValue())) {
            saturateCandidate = fMax->getArgOperand(0);
        } else {
            saturateCandidate = 0;
        }

        if (saturateCandidate) {
            // Check that one of the fMin's argument is 1. If not, then don't do
            // a saturate

            bool fMinHasOne = (minC0 && IsOne(minC0)) || (minC1 && IsOne(minC1));
            if (! fMinHasOne) {
                saturateCandidate = 0;
            }
        }

        if (saturateCandidate) {
            // Make the fSaturate call
            const Type* tys[2] = { fMin->getType(), fMin->getType() };
            Function* f = Intrinsic::getDeclaration(module, Intrinsic::gla_fSaturate, tys, 2);
            Instruction* satInst = builder->CreateCall(f, saturateCandidate);

            fMin->replaceAllUsesWith(satInst);

            return true;
        }
    }

    // Make an fClamp
    const Type* tys[4] = { fMin->getType(), fMin->getType(), fMin->getType(), fMin->getType() };
    Function* f = Intrinsic::getDeclaration(module, Intrinsic::gla_fClamp, tys, 4);
    int nonfMaxOpIdx = fMin->getArgOperand(0) == fMax ? 1 : 0;
    assert(fMin->getArgOperand(nonfMaxOpIdx) != fMax && fMin->getArgOperand(!nonfMaxOpIdx) == fMax);

    Instruction* clampInst = builder->CreateCall3(f, fMax->getArgOperand(0), fMax->getArgOperand(1),
                                                  fMin->getArgOperand(nonfMaxOpIdx));

    fMin->replaceAllUsesWith(clampInst);

    return true;
}

bool GatherInsts::createExtendedBinOp(Intrinsic::ID extId, BinaryOperator* binOp, Value* operand0, Value* operand1,
                                      Value* highBits, Value* lowBits)
{
    assert(isExtendedOp(binOp) && operand0 && operand1 && operand0->getType() == operand1->getType()
           && gla::GetBasicType(operand0->getType()) == i32Ty);

    // Make the mulExtended
    builder->SetInsertPoint(binOp);
    const Type* type = operand0->getType();
    const Type* tys[4] = { type, type, type, type };
    Function* f = Intrinsic::getDeclaration(module, extId, tys, 4);
    Instruction* extIntrinsic = builder->CreateCall2(f, operand0, operand1);

    // Make the extracts and update the code to use the new values.
    if (highBits) {
        Value* extractHigh = builder->CreateExtractValue(extIntrinsic, 0);
        highBits->replaceAllUsesWith(extractHigh);
    }

    if (lowBits) {
        Value* extractLow = builder->CreateExtractValue(extIntrinsic, 1);
        lowBits->replaceAllUsesWith(extractLow);
    }

    return true;
}


bool GatherInsts::visitIntrinsic(IntrinsicInst* intr)
{
    switch (intr->getIntrinsicID()) {
    case Intrinsic::gla_fMax:
        // min(max(a,b),c)

        // TODO: handle the multiple-use case
        if (intr->hasOneUse())
            if (IntrinsicInst* useIntr = dyn_cast<IntrinsicInst>(intr->use_back()))
                if (useIntr->getIntrinsicID() == Intrinsic::gla_fMin)
                    return visitMinMaxPair(useIntr, intr);

        return false;

    case Intrinsic::gla_fMin:
        // max(a, min(b,c))
        // TODO: check for fMin into a saturate/clamp

        return false;

    default:
        return false;
    } // end of switch (intr->getIntrinsicID())
}


bool GatherInsts::visit(Instruction* inst)
{
    if (IntrinsicInst* intr = dyn_cast<IntrinsicInst>(inst)) {
        return visitIntrinsic(intr);
    }

    unsigned opcode = inst->getOpcode();

    switch (opcode) {

    case Instruction::Sub:
    case Instruction::Add:
    case Instruction::Mul: {
        Value* operand0;
        Value* operand1;
        Value* highBits;
        Value* lowBits;
        bool isSigned;

        BinaryOperator* binOp = dyn_cast<BinaryOperator>(inst);
        assert(binOp);

        if (matchBinOpExtended(binOp, operand0, operand1, highBits, lowBits, isSigned))
            return createExtendedBinOp(getExtendedIntrinsicID(opcode, isSigned), binOp, operand0, operand1,
                                       highBits, lowBits);

        break;
    }

    } // end of switch (inst->getOpcode())

    return false;
}

void GatherInsts::getAnalysisUsage(AnalysisUsage& AU) const
{
    return;
}

void GatherInsts::print(raw_ostream&, const Module*) const
{
    return;
}

char GatherInsts::ID = 0;
INITIALIZE_PASS_BEGIN(GatherInsts,
                      "gather-instructions",
                      "Gather instructions for LunarGLASS",
                      false,  // Whether it looks only at CFG
                      false); // Whether it is an analysis pass
INITIALIZE_PASS_END(GatherInsts,
                    "gather-instructions",
                    "Gather instructions for LunarGLASS",
                    false,  // Whether it looks only at CFG
                    false); // Whether it is an analysis pass

FunctionPass* gla_llvm::createGatherInstsPass()
{
    return new GatherInsts();
}

