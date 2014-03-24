//===- DecomposeInsts.cpp - Decompose instructions for LunarGLASS ---===//
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
//
//===----------------------------------------------------------------------===//
//
// Decompose instructions (generally, intrinsics) as requested by the back end.
// For example,
//
//    L = length(v)
//      -->
//    s = dot(v,v)
//    L = sqrt(s)
//
// Will apply to the results, recursively.
//
//===----------------------------------------------------------------------===//

#pragma warning(push, 1)
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Use.h"
#include "llvm/IR/User.h"
#include "llvm/Pass.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#pragma warning(pop)

#include "Passes/PassSupport.h"
#include "Passes/Immutable/BackEndPointer.h"
#include "Passes/Util/InstructionUtil.h"

// LunarGLASS helpers
#include "Exceptions.h"
#include "TopBuilder.h"
#include "Util.h"

using namespace llvm;
using namespace gla_llvm;

namespace  {
    class DecomposeInsts : public FunctionPass {
    public:
        static char ID;

        DecomposeInsts() : FunctionPass(ID)
        {
            initializeDecomposeInstsPass(*PassRegistry::getPassRegistry());
        }

        virtual bool runOnFunction(Function&);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;

    private:
        void decomposeIntrinsics(BasicBlock*);

        DecomposeInsts(const DecomposeInsts&); // do not implement
        void operator=(const DecomposeInsts&); // do not implement

        Module* module;

        BackEnd* backEnd;
        bool changed;
    };

    llvm::Value* OperateWithConstant(IRBuilder<>& builder, Instruction::BinaryOps op, llvm::Value* arg, double constant)
    {
        llvm::Constant* scalar = MakeFloatConstant(builder.getContext(), (float)constant);
        llvm::Constant* llvmConstant = VectorizeConstant(GetComponentCount(arg), scalar);

        return builder.CreateBinOp(op, arg, llvmConstant);
    }

    llvm::Value* MultiplyByConstant(IRBuilder<>& builder, llvm::Value* arg, double constant)
    {
        return OperateWithConstant(builder, llvm::BinaryOperator::FMul, arg, constant);
    }

    llvm::Value* AddWithConstant(IRBuilder<>& builder, llvm::Value* arg, double constant)
    {
        return OperateWithConstant(builder, llvm::BinaryOperator::FAdd, arg, constant);
    }

    llvm::Function* GetDotIntrinsic(Module* module, Type* argTypes[])
    {
        // scalar dest
        llvm::Type* types[] = { GetBasicType(argTypes[0]), argTypes[0], argTypes[1] };

        Intrinsic::ID dotID;
        switch (GetComponentCount(argTypes[0])) {
        case 2:
            dotID = Intrinsic::gla_fDot2;
            break;
        case 3:
            dotID = Intrinsic::gla_fDot3;
            break;
        case 4:
            dotID = Intrinsic::gla_fDot4;
            break;
        default:
            assert(! "Bad dot component count");
            break;
        }

        return Intrinsic::getDeclaration(module, dotID, types);
    }

} // end namespace

void DecomposeInsts::decomposeIntrinsics(BasicBlock* bb)
{
    IRBuilder<> builder(module->getContext());

    for (BasicBlock::iterator instI = bb->begin(), instE = bb->end(); instI != instE; /* empty */) {
        Instruction* inst = instI;

        // Note this increment of instI will skip decompositions of the code
        // inserted to decompose.  E.g., if length -> dot, and dot is also to
        // be decomposed, then the decomposition of dot will be skipped
        // unless instI is reset.
        ++instI;

        IntrinsicInst* intrinsic = dyn_cast<IntrinsicInst>(inst);
        if (! intrinsic)
            continue;

        // Useful preamble for most case
        llvm::Value* arg0 = 0;
        llvm::Value* arg1 = 0;
        llvm::Value* arg2 = 0;
        if (inst->getNumOperands() > 0)
            arg0 = inst->getOperand(0);
        if (inst->getNumOperands() > 1)
            arg1 = inst->getOperand(1);
        if (inst->getNumOperands() > 2)
            arg2 = inst->getOperand(2);
        llvm::Value* newInst = 0;
        Type* instTypes[] = { inst->getType(), inst->getType(), inst->getType(), inst->getType() };
        Type* argTypes[] = { arg0->getType(), arg0->getType(), arg0->getType(), arg0->getType() };
        builder.SetInsertPoint(instI);

        switch (intrinsic->getIntrinsicID()) {
        case Intrinsic::gla_fRadians:
            {
                // always decompose
                // arg0 -> arg0 * pi / 180
                const double pi_over_180 = 0.01745329251994329576923690768489;
                newInst = MultiplyByConstant(builder, arg0, pi_over_180);
                break;
            }
        case Intrinsic::gla_fDegrees:
            {
                // always decompose
                // arg0 -> arg0 * 180 / pi
                const double pi_into_180 = 57.295779513082320876798154814105;
                newInst = MultiplyByConstant(builder, arg0, pi_into_180);
                break;
            }
        case Intrinsic::gla_fMin:
            if (backEnd->decomposeIntrinsic(EDiMin)) {
                //
                // min(a,b) = select (a < b), a, b
                //
                newInst = builder.CreateFCmpOLT(arg0, arg1);
                newInst = builder.CreateSelect(newInst, arg0, arg1);
            }
            break;
        case Intrinsic::gla_fMax:
            if (backEnd->decomposeIntrinsic(EDiMax)) {
                //
                // max(a,b) = select (a > b), a, b
                //
                newInst = builder.CreateFCmpOGT(arg0, arg1);
                newInst = builder.CreateSelect(newInst, arg0, arg1);
            }
            break;
        case Intrinsic::gla_fClamp:
            if (backEnd->decomposeIntrinsic(EDiClamp))
            {
                //
                // Clamp(x, minVal, maxVal) is defined to be min(max(x, minVal), maxVal).
                //
                Function* max = Intrinsic::getDeclaration(module, Intrinsic::gla_fMax, makeArrayRef(argTypes, 3));
                Function* min = Intrinsic::getDeclaration(module, Intrinsic::gla_fMin, makeArrayRef(argTypes, 3));
                newInst = builder.CreateCall2(max, arg0, arg1);
                newInst = builder.CreateCall2(min, newInst, arg2);

                // Make next iteration revisit this decomposition, in case min
                // or max are decomposed.
                instI = inst;
                ++instI;
            }
            break;

        case Intrinsic::gla_fAsin:
            if (backEnd->decomposeIntrinsic(EDiAsin)) {
                UnsupportedFunctionality("decomposition of gla_fAsin");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fAcos:
            if (backEnd->decomposeIntrinsic(EDiAcos))
            {
                // TODO: Do we need to handle domain errors?  (E.g., bad input value)
                //
                // acos(x) ~= sqrt(1-x)*(a + x*(b + x*(c + x*d)))
                // where  a =  1.57079632679
                //        b = -0.213300989
                //        c =  0.077980478
                //        d = -0.0216409
                //
                double a =  1.57079632679;
                double b = -0.213300989;
                double c =  0.077980478;
                double d = -0.0216409;

                // polynomial part, going right to left...
                llvm::Value* poly;
                poly = MultiplyByConstant(builder, arg0, d);
                poly = AddWithConstant(builder, poly, c);
                poly = builder.CreateFMul(arg0, poly);
                poly = AddWithConstant(builder, poly, b);
                poly = builder.CreateFMul(arg0, poly);
                poly = AddWithConstant(builder, poly, a);

                // sqrt part
                Function* sqrt = Intrinsic::getDeclaration(module, Intrinsic::gla_fSqrt, makeArrayRef(argTypes, 2));
                newInst = builder.CreateFNeg(arg0);
                newInst = AddWithConstant(builder, newInst, 1.0);
                newInst = builder.CreateCall(sqrt, newInst);
                newInst = builder.CreateFMul(newInst, poly);
            }
            break;
        case Intrinsic::gla_fAtan:
            if (backEnd->decomposeIntrinsic(EDiAtan)) {
                UnsupportedFunctionality("decomposition of gla_fAtan");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fAtan2:
            if (backEnd->decomposeIntrinsic(EDiAtan2)) {
                UnsupportedFunctionality("decomposition of gla_fAtan2");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fCosh:
            if (backEnd->decomposeIntrinsic(EDiCosh)) {
                UnsupportedFunctionality("decomposition of gla_fCosh");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fSinh:
            if (backEnd->decomposeIntrinsic(EDiSinh)) {
                UnsupportedFunctionality("decomposition of gla_fSinh");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fTanh:
            if (backEnd->decomposeIntrinsic(EDiTanh)) {
                UnsupportedFunctionality("decomposition of gla_fTanh");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fAcosh:
            if (backEnd->decomposeIntrinsic(EDiACosh)) {
                UnsupportedFunctionality("decomposition of gla_fACosh");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fAsinh:
            if (backEnd->decomposeIntrinsic(EDiASinh)) {
                UnsupportedFunctionality("decomposition of gla_fASinh");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fAtanh:
            if (backEnd->decomposeIntrinsic(EDiATanh)) {
                UnsupportedFunctionality("decomposition of gla_fATanh");
                //changed = true;
            }
            break;

        case Intrinsic::gla_fPowi:
            if (backEnd->decomposeIntrinsic(EDiPowi)) {
                UnsupportedFunctionality("decomposition of gla_fPowi");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fExp10:
        case Intrinsic::gla_fExp:
            if ((intrinsic->getIntrinsicID() == Intrinsic::gla_fExp10 && backEnd->decomposeIntrinsic(EDiExp10)) ||
                (intrinsic->getIntrinsicID() == Intrinsic::gla_fExp   && backEnd->decomposeIntrinsic(EDiExp))) {
                //    10^X = 2^(X /(log base 10 of 2))
                // -> 10^X = 2^(X * 3.3219280948873623478703194294894)
                //
                //     e^X = 2^(X /(log base e of 2))
                // ->  e^X = 2^(X * 1.4426950408889634073599246810019)

                //const double inv_log10_e = 2.3025850929940456840179914546844;  // 10 -> e, in case it comes up
                const double inv_log10_2 = 3.3219280948873623478703194294894;  // 10 -> 2
                const double inv_loge_2  = 1.4426950408889634073599246810019;  //  e -> 2

                double multiplier;
                if (intrinsic->getIntrinsicID() == Intrinsic::gla_fExp10)
                    multiplier = inv_log10_2;
                else
                    multiplier = inv_loge_2;

                newInst = MultiplyByConstant(builder, arg0, multiplier);
                Function* exp = Intrinsic::getDeclaration(module, Intrinsic::gla_fExp2, makeArrayRef(argTypes, 2));
                newInst = builder.CreateCall(exp, newInst);
            }
            break;
        case Intrinsic::gla_fLog10:
        case Intrinsic::gla_fLog:
            if ((intrinsic->getIntrinsicID() == Intrinsic::gla_fLog10 && backEnd->decomposeIntrinsic(EDiLog10)) ||
                (intrinsic->getIntrinsicID() == Intrinsic::gla_fLog   && backEnd->decomposeIntrinsic(EDiLog))) {
                //    log base 10 of X = (log base 10 of 2) * (log base 2 of X)
                // -> log base 10 of X = 0.30102999566398119521373889472449 * (log base 2 of X)
                //
                //    log base e  of X = (log base e of 2) * (log base 2 of X)
                // -> log base e  of X = 0.69314718055994530941723212145818 * (log base 2 of X)

                //const double log10_e = 0.43429448190325182765112891891661;  // 10 -> e, in case it comes up
                const double log10_2 = 0.30102999566398119521373889472449;  // 10 -> 2
                const double loge_2  = 0.69314718055994530941723212145818;  //  e -> 2

                double multiplier;
                if (intrinsic->getIntrinsicID() == Intrinsic::gla_fLog10)
                    multiplier = log10_2;
                else
                    multiplier = loge_2;

                Function* log = Intrinsic::getDeclaration(module, Intrinsic::gla_fLog2, makeArrayRef(argTypes, 2));
                newInst = builder.CreateCall(log, arg0);
                newInst = MultiplyByConstant(builder, newInst, multiplier);
            }
            break;

        case Intrinsic::gla_fInverseSqrt:
            if (backEnd->decomposeIntrinsic(EDiInverseSqrt)) {
                Function* sqrt = Intrinsic::getDeclaration(module, Intrinsic::gla_fSqrt, makeArrayRef(argTypes, 2));
                newInst = builder.CreateCall(sqrt, arg0);
                newInst = builder.CreateFDiv(MakeFloatConstant(module->getContext(), 1.0), newInst);
            }
            break;
        case Intrinsic::gla_fFraction:
            if (backEnd->decomposeIntrinsic(EDiFraction)) {
                UnsupportedFunctionality("decomposition of gla_fFraction");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fSign:
            if (backEnd->decomposeIntrinsic(EDiSign)) {
                UnsupportedFunctionality("decomposition of gla_fSign");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fModF:
            if (backEnd->decomposeIntrinsic(EDiModF)) {
                UnsupportedFunctionality("decomposition of gla_fModF");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fMix:
            if (backEnd->decomposeIntrinsic(EDiMix)) {
                //
                // genType mix (x, y, a) = x * (1 - a) + y * a
                //
                llvm::Value* t;
                t = builder.CreateFNeg(arg2);
                t = AddWithConstant(builder, t, 1.0);
                t = builder.CreateFMul(arg0, t);
                newInst = builder.CreateFMul(arg1, arg2);
                newInst = builder.CreateFAdd(t, newInst);
            }
            break;
        case Intrinsic::gla_fStep:
            if (backEnd->decomposeIntrinsic(EDiStep))
            {
                //
                // step(edge, x) is defined to be 0.0 if x < edge, otherwise 1.0.
                //
                llvm::FCmpInst::Predicate predicate = llvm::FCmpInst::FCMP_OLT;
                llvm::Value* condition = builder.CreateFCmp(predicate, arg1, arg0);
                newInst = builder.CreateSelect(condition, VectorizeConstant(GetComponentCount(arg1), MakeFloatConstant(module->getContext(), 0.0)),
                                                          VectorizeConstant(GetComponentCount(arg1), MakeFloatConstant(module->getContext(), 1.0)));
            }
            break;
        case Intrinsic::gla_fSmoothStep:
            if (backEnd->decomposeIntrinsic(EDiSmoothStep)) {
                UnsupportedFunctionality("decomposition of gla_fSmoothStep");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fIsNan:
            if (backEnd->decomposeIntrinsic(EDiIsNan)) {
                UnsupportedFunctionality("decomposition of gla_fIsNan");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fFma:
            if (backEnd->decomposeIntrinsic(EDiFma)) {
                UnsupportedFunctionality("decomposition of gla_Fma");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fPackUnorm2x16:
            if (backEnd->decomposeIntrinsic(EDiPackUnorm2x16)) {
                UnsupportedFunctionality("decomposition of gla_fPackUnorm2x16");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fPackUnorm4x8:
            if (backEnd->decomposeIntrinsic(EDiPackUnorm4x8)) {
                UnsupportedFunctionality("decomposition of gla_fPackUnorm4x8");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fPackSnorm4x8:
            if (backEnd->decomposeIntrinsic(EDiPackSnorm4x8)) {
                UnsupportedFunctionality("decomposition of gla_fPackSnorm4x8");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fUnpackUnorm2x16:
            if (backEnd->decomposeIntrinsic(EDiUnpackUnorm2x16)) {
                UnsupportedFunctionality("decomposition of gla_fUnpackUnorm2x16");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fUnpackUnorm4x8:
            if (backEnd->decomposeIntrinsic(EDiUnpackUnorm4x8)) {
                UnsupportedFunctionality("decomposition of gla_fUnpackUnorm4x8");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fUnpackSnorm4x8:
            if (backEnd->decomposeIntrinsic(EDiUnpackSnorm4x8)) {
                UnsupportedFunctionality("decomposition of gla_fUnpackSnorm4x8");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fPackDouble2x32:
            if (backEnd->decomposeIntrinsic(EDiPackDouble2x32)) {
                UnsupportedFunctionality("decomposition of gla_fPackDouble2x32");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fUnpackDouble2x32:
            if (backEnd->decomposeIntrinsic(EDiUnpackDouble2x32)) {
                UnsupportedFunctionality("decomposition of gla_fUnpackDouble2x32");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fLength:
            if (backEnd->decomposeIntrinsic(EDiLength)) {
                if (GetComponentCount(arg0) > 1) {
                    Function* dot = GetDotIntrinsic(module, argTypes);
                    newInst = builder.CreateCall2(dot, arg0, arg0);

                    Function* sqrt = Intrinsic::getDeclaration(module, Intrinsic::gla_fSqrt, makeArrayRef(instTypes, 2));
                    newInst = builder.CreateCall(sqrt, newInst);
                } else {
                    Function* abs = Intrinsic::getDeclaration(module, Intrinsic::gla_fAbs, makeArrayRef(instTypes, 2));
                    newInst = builder.CreateCall(abs, arg0);
                }

                // Make next iteration revisit this decomposition, in case dot is
                // decomposed.
                instI = inst;
                ++instI;
            }
            break;
        case Intrinsic::gla_fDistance:
            if (backEnd->decomposeIntrinsic(EDiDistance)) {
                newInst = builder.CreateFSub(arg0, arg1);
                llvm::Type* types[] = { GetBasicType(newInst), newInst->getType() };
                Function* length = Intrinsic::getDeclaration(module, Intrinsic::gla_fLength, types);
                newInst = builder.CreateCall(length, newInst);

                // Make next iteration revisit this decomposition, in case length is
                // decomposed.
                instI = inst;
                ++instI;
            }
            break;
        case Intrinsic::gla_fDot2:
            if (backEnd->decomposeIntrinsic(EDiDot)) {
                newInst = builder.CreateFMul(arg0, arg1);
                llvm::Value* element0 = builder.CreateExtractElement(newInst, MakeUnsignedConstant(module->getContext(), 0));
                llvm::Value* element1 = builder.CreateExtractElement(newInst, MakeUnsignedConstant(module->getContext(), 1));
                newInst = builder.CreateFAdd(element0, element1);
            }
            break;
        case Intrinsic::gla_fDot3:
            if (backEnd->decomposeIntrinsic(EDiDot)) {
                newInst = builder.CreateFMul(arg0, arg1);
                arg0 = newInst;
                llvm::Value* element0 = builder.CreateExtractElement(arg0, MakeUnsignedConstant(module->getContext(), 0));
                llvm::Value* element1 = builder.CreateExtractElement(arg0, MakeUnsignedConstant(module->getContext(), 1));
                newInst = builder.CreateFAdd(element0, element1);
                llvm::Value* element = builder.CreateExtractElement(arg0, MakeUnsignedConstant(module->getContext(), 2));
                newInst = builder.CreateFAdd(newInst, element);
            }
            break;
        case Intrinsic::gla_fDot4:
            if (backEnd->decomposeIntrinsic(EDiDot)) {
                newInst = builder.CreateFMul(arg0, arg1);
                arg0 = newInst;
                llvm::Value* element0 = builder.CreateExtractElement(arg0, MakeUnsignedConstant(module->getContext(), 0));
                llvm::Value* element1 = builder.CreateExtractElement(arg0, MakeUnsignedConstant(module->getContext(), 1));
                newInst = builder.CreateFAdd(element0, element1);
                for (int el = 2; el < 4; ++el) {
                    llvm::Value* element = builder.CreateExtractElement(arg0, MakeUnsignedConstant(module->getContext(), el));
                    newInst = builder.CreateFAdd(newInst, element);
                }
            }
            break;
        case Intrinsic::gla_fCross:
            if (backEnd->decomposeIntrinsic(EDiCross)) {
                // (a1, a2, a3) X (b1, b2, b3) -> (a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - a2*b1)

                llvm::Value* a1 = builder.CreateExtractElement(arg0, MakeUnsignedConstant(module->getContext(), 0));
                llvm::Value* a2 = builder.CreateExtractElement(arg0, MakeUnsignedConstant(module->getContext(), 1));
                llvm::Value* a3 = builder.CreateExtractElement(arg0, MakeUnsignedConstant(module->getContext(), 2));

                llvm::Value* b1 = builder.CreateExtractElement(arg1, MakeUnsignedConstant(module->getContext(), 0));
                llvm::Value* b2 = builder.CreateExtractElement(arg1, MakeUnsignedConstant(module->getContext(), 1));
                llvm::Value* b3 = builder.CreateExtractElement(arg1, MakeUnsignedConstant(module->getContext(), 2));

                llvm::Value* empty = llvm::UndefValue::get(arg0->getType());

                bool scalarized = false;

                if (scalarized) {
                    // do it all with scalars

                    // a2*b3 - a3*b2
                    llvm::Value* p1 = builder.CreateFMul(a2, b3);
                    llvm::Value* p2 = builder.CreateFMul(a3, b2);
                    llvm::Value* element = builder.CreateFSub(p1, p2);
                    newInst = builder.CreateInsertElement(empty, element, MakeUnsignedConstant(module->getContext(), 0));

                    // a3*b1 - a1*b3
                    p1 = builder.CreateFMul(a3, b1);
                    p2 = builder.CreateFMul(a1, b3);
                    element = builder.CreateFSub(p1, p2);
                    newInst = builder.CreateInsertElement(newInst, element, MakeUnsignedConstant(module->getContext(), 1));

                    // a1*b2 - a2*b1
                    p1 = builder.CreateFMul(a1, b2);
                    p2 = builder.CreateFMul(a2, b1);
                    element = builder.CreateFSub(p1, p2);
                    newInst = builder.CreateInsertElement(newInst, element, MakeUnsignedConstant(module->getContext(), 2));
                } else {
                    // do it all with vectors

                    // (a2, a3, a1)
                    llvm::Value* aPerm;
                    aPerm = builder.CreateInsertElement(empty, a2, MakeUnsignedConstant(module->getContext(), 0));
                    aPerm = builder.CreateInsertElement(aPerm, a3, MakeUnsignedConstant(module->getContext(), 1));
                    aPerm = builder.CreateInsertElement(aPerm, a1, MakeUnsignedConstant(module->getContext(), 2));

                    // (b3, b1, b2)
                    llvm::Value* bPerm;
                    bPerm = builder.CreateInsertElement(empty, b3, MakeUnsignedConstant(module->getContext(), 0));
                    bPerm = builder.CreateInsertElement(bPerm, b1, MakeUnsignedConstant(module->getContext(), 1));
                    bPerm = builder.CreateInsertElement(bPerm, b2, MakeUnsignedConstant(module->getContext(), 2));

                    // first term computation
                    llvm::Value* firstTerm = builder.CreateFMul(aPerm, bPerm);

                    // (a3, a1, a2)
                    aPerm = builder.CreateInsertElement(empty, a3, MakeUnsignedConstant(module->getContext(), 0));
                    aPerm = builder.CreateInsertElement(aPerm, a1, MakeUnsignedConstant(module->getContext(), 1));
                    aPerm = builder.CreateInsertElement(aPerm, a2, MakeUnsignedConstant(module->getContext(), 2));

                    // (b2, b3, b1)
                    bPerm = builder.CreateInsertElement(empty, b2, MakeUnsignedConstant(module->getContext(), 0));
                    bPerm = builder.CreateInsertElement(bPerm, b3, MakeUnsignedConstant(module->getContext(), 1));
                    bPerm = builder.CreateInsertElement(bPerm, b1, MakeUnsignedConstant(module->getContext(), 2));

                    // second term computation
                    newInst = builder.CreateFMul(aPerm, bPerm);

                    // Finish it off
                    newInst = builder.CreateFSub(firstTerm, newInst);
                }
            }
            break;
        case Intrinsic::gla_fNormalize:
            if (backEnd->decomposeIntrinsic(EDiNormalize)) {
                if (GetComponentCount(arg0) > 1) {
                    Function* dot = GetDotIntrinsic(module, argTypes);
                    newInst = builder.CreateCall2(dot, arg0, arg0);

                    llvm::Type* type[] = { newInst->getType(), newInst->getType() };
                    Function* inverseSqrt = Intrinsic::getDeclaration(module, Intrinsic::gla_fInverseSqrt, type);
                    newInst = builder.CreateCall(inverseSqrt, newInst);

                    // smear it
                    llvm::Value* smeared = llvm::UndefValue::get(arg0->getType());
                    for (int c = 0; c < GetComponentCount(arg0); ++c)
                        smeared = builder.CreateInsertElement(smeared, newInst, MakeIntConstant(module->getContext(), c));

                    newInst = builder.CreateFMul(arg0, smeared);
                } else {
                    newInst = MakeFloatConstant(module->getContext(), 1.0);
                }

                // Make next iteration revisit this decomposition, in case dot or inverse-sqrt
                // are decomposed.
                instI = inst;
                ++instI;
            }
            break;
        case Intrinsic::gla_fNormalize3D:
            if (backEnd->decomposeIntrinsic(EDiNormalize3D)) {

                // Note:  This does a 3D normalize on a vec3 or vec4.  The width of arg0 does
                // not determine that width of the dot-product input, the "3" in the "3D" does.

                llvm::Type* types[] = { GetBasicType(argTypes[0]), argTypes[0], argTypes[1] };
                Function* dot = Intrinsic::getDeclaration(module, Intrinsic::gla_fDot3, types);
                newInst = builder.CreateCall2(dot, arg0, arg0);

                llvm::Type* type[] = { newInst->getType(), newInst->getType() };
                Function* inverseSqrt = Intrinsic::getDeclaration(module, Intrinsic::gla_fInverseSqrt, type);
                newInst = builder.CreateCall(inverseSqrt, newInst);

                // smear it
                llvm::Value* smeared = llvm::UndefValue::get(arg0->getType());
                for (int c = 0; c < GetComponentCount(arg0); ++c)
                    smeared = builder.CreateInsertElement(smeared, newInst, MakeIntConstant(module->getContext(), c));

                // If we're 4-wide, copy over the original w component
                if (GetComponentCount(arg0) == 4)
                    smeared = builder.CreateInsertElement(smeared, arg0, MakeIntConstant(module->getContext(), 4));

                newInst = builder.CreateFMul(arg0, smeared);

                // Make next iteration revisit this decomposition, in case dot or inverse-sqrt
                // are decomposed.
                instI = inst;
                ++instI;
            }
            break;
        case Intrinsic::gla_fLit:
            if (backEnd->decomposeIntrinsic(EDiLit)) {
                UnsupportedFunctionality("decomposition of gla_fLit");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fFaceForward:
            if (backEnd->decomposeIntrinsic(EDiFaceForward)) {
                //
                // faceForward(N, I, Nref) is defined to be N if dot(Nref, I) < 0, otherwise return –N.
                //
                UnsupportedFunctionality("decomposition of gla_fFaceForward");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fReflect:
            if (backEnd->decomposeIntrinsic(EDiReflect))
            {
                //
                // reflect(I, N) is defined to be I – 2 * dot(N, I) * N,
                // where N may be assumed to be normalized.
                //
                // Note if the number of components is 1, then N == 1 and
                // this turns into I - 2*I, or -I.
                //
                if (GetComponentCount(arg0) > 1) {
                    Function* dot = GetDotIntrinsic(module, argTypes);
                    newInst = builder.CreateCall2(dot, arg0, arg1);
                    newInst = MultiplyByConstant(builder, newInst, 2.0);

                    // smear this back up to a vector again
                    llvm::Value* smeared = llvm::UndefValue::get(arg0->getType());
                    for (int c = 0; c < GetComponentCount(arg0); ++c)
                        smeared = builder.CreateInsertElement(smeared, newInst, MakeIntConstant(module->getContext(), c));

                    newInst = builder.CreateFMul(smeared, arg1);
                    newInst = builder.CreateFSub(arg0, newInst);
                } else {
                    newInst = builder.CreateFNeg(arg0);
                }

                // Make next iteration revisit this decomposition, in case dot
                // is decomposed
                instI = inst;
                ++instI;
            }
            break;
        case Intrinsic::gla_fRefract:
            if (backEnd->decomposeIntrinsic(EDiRefract)) {
                UnsupportedFunctionality("decomposition of gla_fRefract");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fFilterWidth:
            if (backEnd->decomposeIntrinsic(EDiFilterWidth)) {
                UnsupportedFunctionality("decomposition of gla_fFilterWidth");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fFixedTransform:
            if (backEnd->decomposeIntrinsic(EDiFixedTransform)) {
                UnsupportedFunctionality("decomposition of gla_fFixedTransform");
                //changed = true;
            }
            break;

        case Intrinsic::gla_any:
            if (backEnd->decomposeIntrinsic(EDiAny)) {
                if (GetComponentCount(arg0) == 1)
                    UnsupportedFunctionality("any() on a scalar");

                newInst = builder.CreateExtractElement(arg0, MakeUnsignedConstant(module->getContext(), 0));
                for (int c = 1; c < GetComponentCount(arg0); ++c) {
                    llvm::Value* comp = builder.CreateExtractElement(arg0, MakeUnsignedConstant(module->getContext(), c));
                    newInst = builder.CreateOr(newInst, comp);
                }
            }
            break;
        case Intrinsic::gla_all:
            if (backEnd->decomposeIntrinsic(EDiAll)) {
                if (GetComponentCount(arg0) == 1)
                    UnsupportedFunctionality("all() on a scalar");

                newInst = builder.CreateExtractElement(arg0, MakeUnsignedConstant(module->getContext(), 0));
                for (int c = 1; c < GetComponentCount(arg0); ++c) {
                    llvm::Value* comp = builder.CreateExtractElement(arg0, MakeUnsignedConstant(module->getContext(), c));
                    newInst = builder.CreateAnd(newInst, comp);
                }
            }
            break;
        case Intrinsic::gla_not:
            if (backEnd->decomposeIntrinsic(EDiNot)) {
                if (GetComponentCount(arg0) == 1)
                    UnsupportedFunctionality("not() on a scalar");

                newInst = builder.CreateNot(arg0);
            }
            break;
        case Intrinsic::gla_fTextureSample:
        case Intrinsic::gla_fTextureSampleLodRefZ:
        case Intrinsic::gla_fTextureSampleLodRefZOffset:
        case Intrinsic::gla_fTextureSampleLodRefZOffsetGrad:
            if (backEnd->decomposeIntrinsic(EDiTextureProjection)) {

                // if projection flag is set, divide all coordinates (and refZ) by projection
                int texFlags = GetConstantInt(intrinsic->getArgOperand(GetTextureOpIndex(ETOFlag)));
                if (texFlags & ETFProjected) {

                    // insert before intrinsic since we are not replacing it
                    builder.SetInsertPoint(inst);

                    // turn off projected flag to reflect decomposition
                    texFlags &= ~ETFProjected;

                    llvm::Value* coords = intrinsic->getArgOperand(GetTextureOpIndex(ETOCoord));

                    // determine how many channels are live after decomposition
                    int newCoordWidth = 0;
                    switch (GetConstantInt(intrinsic->getArgOperand(gla::ETOSamplerType))) {
                    case gla::ESamplerBuffer:
                    case gla::ESampler1D:      newCoordWidth = 1;  break;
                    case gla::ESampler2D:
                    case gla::ESampler2DRect:
                    case gla::ESampler2DMS:    newCoordWidth = 2;  break;
                    case gla::ESampler3D:      newCoordWidth = 3;  break;
                    case gla::ESamplerCube:
                        gla::UnsupportedFunctionality("projection with cube sampler");
                        break;
                    default:
                        assert(0 && "Unknown sampler type");
                        break;
                    }

                    if (texFlags & gla::ETFArrayed)
                        gla::UnsupportedFunctionality("projection with arrayed sampler");

                    // projection resides in last component
                    llvm::Value* projIdx = MakeUnsignedConstant(module->getContext(), GetComponentCount(coords) - 1);
                    llvm::Value* divisor = builder.CreateExtractElement(coords, projIdx);

                    llvm::Type* newCoordType;
                    if (newCoordWidth > 1)
                        newCoordType = llvm::VectorType::get(GetBasicType(coords), newCoordWidth);
                    else
                        newCoordType = GetBasicType(coords);

                    // create space to hold results
                    llvm::Value* newCoords   = llvm::UndefValue::get(newCoordType);
                    llvm::Value* smearedProj = llvm::UndefValue::get(newCoordType);

                    if (newCoordWidth > 1) {
                        for (int i = 0; i < newCoordWidth; ++i) {
                            llvm::Value* idx = MakeUnsignedConstant(module->getContext(), i);

                            // smear projection
                            smearedProj = builder.CreateInsertElement(smearedProj, divisor, idx);

                            // shrink coordinates to remove projection component
                            llvm::Value* oldCoord = builder.CreateExtractElement(coords, idx);
                            newCoords = builder.CreateInsertElement(newCoords, oldCoord, idx);
                        }
                    } else {
                        smearedProj = divisor;
                        newCoords = builder.CreateExtractElement(coords, MakeUnsignedConstant(module->getContext(), 0));
                    }

                    // divide coordinates
                    newCoords = builder.CreateFDiv(newCoords, smearedProj);

                    //
                    // Remaining code declares new intrinsic and modifies call arguments
                    //

                    // build up argTypes for flexible parameters, including result
                    llvm::SmallVector<llvm::Type*, 5> types;

                    // result type
                    types.push_back(intrinsic->getType());

                    // use new coords to reflect shrink
                    types.push_back(newCoords->getType());

                    // add offset
                    switch (intrinsic->getIntrinsicID()) {
                    case Intrinsic::gla_fTextureSampleLodRefZOffset:
                    case Intrinsic::gla_fTextureSampleLodRefZOffsetGrad:
                        types.push_back(intrinsic->getArgOperand(ETOOffset)->getType());
                    default:
                        break;
                    }

                    // add gradients
                    switch (intrinsic->getIntrinsicID()) {
                    case Intrinsic::gla_fTextureSampleLodRefZOffsetGrad:
                        types.push_back(intrinsic->getArgOperand(ETODPdx)->getType());
                        types.push_back(intrinsic->getArgOperand(ETODPdy)->getType());
                    default:
                        break;
                    }

                    // declare the new intrinsic
                    Function* texture = Intrinsic::getDeclaration(module, intrinsic->getIntrinsicID(), types);

                    // modify arguments to match new intrinsic
                    intrinsic->setCalledFunction(texture);
                    intrinsic->setArgOperand(ETOFlag, MakeUnsignedConstant(module->getContext(), texFlags));
                    intrinsic->setArgOperand(ETOCoord, newCoords);

                    switch (intrinsic->getIntrinsicID()) {
                    case Intrinsic::gla_fTextureSampleLodRefZ:
                    case Intrinsic::gla_fTextureSampleLodRefZOffset:
                    case Intrinsic::gla_fTextureSampleLodRefZOffsetGrad:
                        intrinsic->setArgOperand(ETORefZ, builder.CreateFDiv(intrinsic->getArgOperand(ETORefZ), divisor));                        
                    default:
                        break;
                    }

                    // mark our change, but don't replace the intrinsic
                    changed = true;
                }
            }
            break;

        default:
            // The cases above needs to be comprehensive in terms of checking
            // for what intrinsics to decompose.  If not there the assumption is
            // it never needs to be decomposed.
            break;
        }

        if (newInst) {
            inst->replaceAllUsesWith(newInst);
            inst->dropAllReferences();
            inst->eraseFromParent();
            changed = true;
        }
    }
}

bool DecomposeInsts::runOnFunction(Function& F)
{
    BackEndPointer* bep = getAnalysisIfAvailable<BackEndPointer>();
    if (! bep) {
        return false;
    }
    backEnd = *bep;

    module = F.getParent();

    // Start off having not changed anything, our methods will set this to be
    // true if they perform any changes
    changed = false;

    for (Function::iterator bbI = F.begin(), bbE = F.end(); bbI != bbE; ++bbI) {
        decomposeIntrinsics(bbI);
    }

    return changed;
}

void DecomposeInsts::getAnalysisUsage(AnalysisUsage& AU) const
{
    return;
}

void DecomposeInsts::print(raw_ostream&, const Module*) const
{
    return;
}


char DecomposeInsts::ID = 0;
INITIALIZE_PASS_BEGIN(DecomposeInsts,
                      "decompose-insts",
                      "Decompose instructions for LunarGLASS",
                      false,  // Whether it looks only at CFG
                      false); // Whether it is an analysis pass
INITIALIZE_PASS_END(DecomposeInsts,
                    "decompose-insts",
                    "Decompose instructions for LunarGLASS",
                    false,  // Whether it looks only at CFG
                    false); // Whether it is an analysis pass


FunctionPass* gla_llvm::createDecomposeInstsPass()
{
    return new DecomposeInsts();
}
