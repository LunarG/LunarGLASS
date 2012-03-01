//===- DecomposeInsts.cpp - Decompose instructions for LunarGLASS ---===//
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

#include "llvm/GlobalVariable.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Use.h"
#include "llvm/User.h"

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
        if (inst->getNumOperands() >= 1)
            arg0 = inst->getOperand(0);
        if (inst->getNumOperands() >= 2)
            arg1 = inst->getOperand(1);
        llvm::Value* newInst = 0;
        const Type* instTypes[] = { inst->getType(), inst->getType(), inst->getType(), inst->getType() };
        const Type* argTypes[] = { arg0->getType(), arg0->getType(), arg0->getType(), arg0->getType() };
        builder.SetInsertPoint(instI);

        switch (intrinsic->getIntrinsicID()) {
        case Intrinsic::gla_fMin:
            if (backEnd->decomposeIntrinsic(EDiMin)) {
                UnsupportedFunctionality("decomposition of gla_fMin");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fMax:
            if (backEnd->decomposeIntrinsic(EDiMax)) {
                UnsupportedFunctionality("decomposition of gla_fMax");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fClamp:
            if (backEnd->decomposeIntrinsic(EDiClamp)) {
                UnsupportedFunctionality("decomposition of gla_fClamp");
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
        case Intrinsic::gla_fExp2:
        case Intrinsic::gla_fExp:
            if (intrinsic->getIntrinsicID() == Intrinsic::gla_fExp10 && backEnd->decomposeIntrinsic(EDiExp10)) 
            {
                //    10^X = e^(X /(log base 10 of e))
                // -> 10^X = e^(X * 2.3025850929940456840179914546844)
                //
                //    10^X = 2^(X /(log base 10 of 2))
                // -> 10^X = 2^(X * 3.3219280948873623478703194294894)
                
                const double inv_log10_e = 2.3025850929940456840179914546844;  // 10 -> e
                const double inv_log10_2 = 3.3219280948873623478703194294894;  // 10 -> 2
                const double inv_loge_2  = 1.4426950408889634073599246810019;  //  e -> 2

                // the following can be easily changed to re-express in terms of base e or base 2.
                const bool goToBase_e = false;  // false will mean go to base 2
                double multiplier;
                Function* exp;
                if (goToBase_e) {
                    multiplier = inv_log10_e;
                    exp = Intrinsic::getDeclaration(module, Intrinsic::gla_fExp, argTypes, 2);
                } else {
                    multiplier = inv_log10_2;
                    exp = Intrinsic::getDeclaration(module, Intrinsic::gla_fExp2, argTypes, 2);
                }

                llvm::Constant* scalar = MakeFloatConstant(module->getContext(), (float)multiplier);
                if (GetComponentCount(arg0) == 1) {
                    newInst = builder.CreateFMul(arg0, scalar);
                } else {
                    llvm::SmallVector<llvm::Constant*, 4> vector(GetComponentCount(arg0), scalar);
                    llvm::Value* llvmVector = llvm::ConstantVector::get(vector);

                    newInst = builder.CreateFMul(arg0, llvmVector);
                }

                newInst = builder.CreateCall(exp, newInst);                
            }
            break;
        case Intrinsic::gla_fLog10:
        case Intrinsic::gla_fLog2:
        case Intrinsic::gla_fLog:
            if (intrinsic->getIntrinsicID() == Intrinsic::gla_fLog10 && backEnd->decomposeIntrinsic(EDiLog10)) 
            {
                //    log base 10 of X = (log base 10 of e) * (log base e of X)
                // -> log base 10 of X = 0.43429448190325182765112891891661 * (log base e of X)
                //
                //    log base 10 of X = (log base 10 of 2) * (log base 2 of X)
                // -> log base 10 of X = 0.30102999566398119521373889472449 * (log base 2 of X)
                
                const double log10_e = 0.43429448190325182765112891891661;  // 10 -> e
                const double log10_2 = 0.30102999566398119521373889472449;  // 10 -> 2
                const double loge_2  = 0.69314718055994530941723212145818;  //  e -> 2

                // the following can be easily changed to re-express in terms of base e or base 2.
                const bool goToBase_e = false;  // false will mean go to base 2
                double multiplier;
                Function* log;
                if (goToBase_e) {
                    multiplier = log10_e;
                    log = Intrinsic::getDeclaration(module, Intrinsic::gla_fLog, argTypes, 2);
                } else {
                    multiplier = log10_2;
                    log = Intrinsic::getDeclaration(module, Intrinsic::gla_fLog2, argTypes, 2);
                }

                newInst = builder.CreateCall(log, arg0);
                llvm::Constant* scalar = MakeFloatConstant(module->getContext(), (float)multiplier);
                if (GetComponentCount(arg0) == 1) {
                    newInst = builder.CreateFMul(newInst, scalar);
                } else {
                    llvm::SmallVector<llvm::Constant*, 4> vector(GetComponentCount(arg0), scalar);
                    llvm::Value* llvmVector = llvm::ConstantVector::get(vector);

                    newInst = builder.CreateFMul(newInst, llvmVector);
                }
            }
            break;

        case Intrinsic::gla_fInverseSqrt:
            if (backEnd->decomposeIntrinsic(EDiInverseSqrt)) {
                Function* sqrt = Intrinsic::getDeclaration(module, Intrinsic::gla_fSqrt, argTypes, 2);
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
        case Intrinsic::gla_fModF:
            if (backEnd->decomposeIntrinsic(EDiModF)) {
                UnsupportedFunctionality("decomposition of gla_fModF");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fMix:
            if (backEnd->decomposeIntrinsic(EDiMix)) {
                UnsupportedFunctionality("decomposition of gla_fMix");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fStep:
            if (backEnd->decomposeIntrinsic(EDiStep)) {
                UnsupportedFunctionality("decomposition of gla_fStep");
                //changed = true;
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
                    Intrinsic::ID dotID;
                    switch (GetComponentCount(arg0)) {
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
                        assert(! "Bad component count");
                    }

                    Function* dot = Intrinsic::getDeclaration(module, dotID, argTypes, 2);
                    newInst = builder.CreateCall2(dot, arg0, arg0);

                    Function* sqrt = Intrinsic::getDeclaration(module, Intrinsic::gla_fSqrt, instTypes, 2);
                    newInst = builder.CreateCall(sqrt, newInst);
                } else {
                    Function* abs = Intrinsic::getDeclaration(module, Intrinsic::gla_fAbs, instTypes, 2);
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
                const llvm::Type* type = newInst->getType();
                Function* length = Intrinsic::getDeclaration(module, Intrinsic::gla_fLength, &type, 1);
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
                UnsupportedFunctionality("decomposition of gla_fCross");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fNormalize:
            if (backEnd->decomposeIntrinsic(EDiNormalize)) {
                if (GetComponentCount(arg0) > 1) {
                    Intrinsic::ID dotID;
                    switch (GetComponentCount(arg0)) {
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
                        assert(! "Bad component count");
                    }

                    Function* dot = Intrinsic::getDeclaration(module, dotID, argTypes, 2);
                    newInst = builder.CreateCall2(dot, arg0, arg0);

                    const llvm::Type* type[2] = { newInst->getType(), newInst->getType() };
                    Function* inverseSqrt = Intrinsic::getDeclaration(module, Intrinsic::gla_fInverseSqrt, type, 2);
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
                UnsupportedFunctionality("decomposition of gla_fNormalize3D");

                // Note:  This does a 3D normalize on a vec3 or vec4.  The width of arg0 does
                // not determine that width of the dot-product input, the "3" in the "3D" does.

                Function* dot = Intrinsic::getDeclaration(module, Intrinsic::gla_fDot3, argTypes, 2);
                newInst = builder.CreateCall2(dot, arg0, arg0);

                const llvm::Type* type[2] = { newInst->getType(), newInst->getType() };
                Function* inverseSqrt = Intrinsic::getDeclaration(module, Intrinsic::gla_fInverseSqrt, type, 2);
                newInst = builder.CreateCall(inverseSqrt, newInst);

                // smear it
                llvm::Value* smeared = llvm::UndefValue::get(arg0->getType());
                for (int c = 0; c < GetComponentCount(arg0); ++c)
                    smeared = builder.CreateInsertElement(smeared, newInst, MakeIntConstant(module->getContext(), c));

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
                UnsupportedFunctionality("decomposition of gla_fFaceForward");
                //changed = true;
            }
            break;
        case Intrinsic::gla_fReflect:
            if (backEnd->decomposeIntrinsic(EDiReflect)) {
                UnsupportedFunctionality("decomposition of gla_fReflect");
                //changed = true;
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
        default:
            // The cases above needs to be comprehensive in terms of checking
            // for what intrinsics to decompose.  If not there the assumption is
            // it never needs to be decomposed.
            ;
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
