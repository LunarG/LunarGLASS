//===- Backend.h - Public interface to LunarGLASS Backends ----------------===//
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
// Public interface to LunarGLASS Backends.
//
// Don't include Mesa headers here.  LunarGLASS is not Mesa-dependent.
//
// Don't include LLVM headers here.  Users of LunarGLASS don't need to
// pull in LLVM headers.
//
//===----------------------------------------------------------------------===//

#pragma once
#ifndef Backend_H
#define Backend_H

// LLVM includes
#include "llvm/Module.h"

// Forward decls
namespace llvm {
    class PHINode;
    class Value;
    class CmpInst;
} // end namespace llvm

namespace gla {

    // support for back-end queries
    enum EFlowControlMode {
        EFcmStructuredOpCodes,
        EFcmExplicitMasking,
        EFcmDynamic
    };

    enum EDecomposableIntrinsic {
        EDiInverseSqrt,
        EDiFraction,
        EDiMod,
        EDiModF,
        EDiMin,
        EDiMax,
        EDiClamp,
        EDiMix,
        EDiSelect,
        EDiStep,
        EDiSmoothStep,
        EDiIsNan,
        EDiFma,
        EDiPackUnorm2x16,
        EDiPackUnorm4x8,
        EDiPackSnorm4x8,
        EDiUnpackUnorm2x16,
        EDiUnpackUnorm4x8,
        EDiUnpackSnorm4x8,
        EDiPackDouble2x32,
        EDiUnpackDouble2x32,
        EDiLength,
        EDiDistance,
        EDiDot,
        EDiCross,
        EDiNormalize,
        EDiNormalize3D,
        EDiFTransform,
        EDiFaceForward,
        EDiReflect,
        EDiRefract,
        EDiFWidth,
        EDiCount
    };

    class BackEndTranslator {
    public:

        BackEndTranslator() { }
        virtual ~BackEndTranslator() { }
        virtual void addGlobal(const llvm::GlobalVariable*) { }
        virtual void startFunctionDeclaration(const llvm::Type*, const std::string&) = 0;
        virtual void addArgument(const llvm::Value*, bool last) = 0;
        virtual void endFunctionDeclaration() = 0;
        virtual void startFunctionBody() = 0;
        virtual void endFunctionBody() = 0;
        virtual void add(const llvm::Instruction*, bool lastBlock) = 0;

        //
        // The following set of functions is motivated by need to convert to
        // structured flow control and eliminate phi functions.
        //

        // This must get called soon enough that it is before the split that
        // makes the phi.  It is then referred to in addPhiCopy().
        virtual void declarePhiCopy(const llvm::Value* dst) { }

        // Called for Phi elimination.
        virtual void addPhiCopy(const llvm::Value* dst, const llvm::Value* src) = 0;

        // Called to build structured flow control.
        virtual void addIf(const llvm::Value* cond, bool invert=false) = 0;
        virtual void addElse() = 0;
        virtual void addEndif() = 0;


        // Specialized loop constructs

        // TODO: add more loop constructs here

        // TODO: add backend queries for each of these forms

        // TODO: figure out how to name things, as this for now simpleInductive
        // really means "simpleStaticInductive". Alternatively, you could remove
        // all the static constraints.

        // Add a simple conditional loop. A simple conditional loop has a simple
        // conditional expression, which consists of a comparison operation and
        // two arguments that must either be constants, extracts (from a vector),
        // or loads (for uniforms). Receives the comparison instruction, the
        // first operand, and the second operand. If the result of the
        // comparison should be inverted, invert will be passed as true.
        virtual void beginSimpleConditionalLoop(const llvm::CmpInst* cmp, const llvm::Value* op1, const llvm::Value* op2, bool invert=false) = 0;

        // Add a simple inductive loop. A simple inductive loop is a loop with
        // an inductive variable starting at 0, and incrementing by 1 through
        // the loop until some statically known final value. A simple inductive
        // loop also has a statically known trip count (how many times it will
        // be executed). It is given the phi node corresponding to the induction
        // variable, and how many times the body will be executed.
        virtual void beginSimpleInductiveLoop(const llvm::PHINode* phi, unsigned count) = 0;

        // Generic loop constructs

        // TODO: change name to be generic loop
        // Add a generic looping construct (e.g. while (true))
        virtual void beginLoop() = 0;

        // Add an end for any of these loops (e.g. '}')
        virtual void endLoop() = 0;

        // Exit the loop (e.g. break). Recieves the condition if it's a
        // conditional exit. If it's conditional, and the condition should be
        // inverted for the exit, then invert will be true.
        virtual void addLoopExit(const llvm::Value* condition=NULL, bool invert=false) = 0;

        // Add a loop backedge (e.g. continue). Receives the condition if it's a
        // conditional loop-back. If it's conditional, and the condition should
        // be inverted for the loop-back, then invert will be true.
        virtual void addLoopBack(const llvm::Value* condition=NULL, bool invert=false) = 0;

        virtual void print() = 0;
    };

    // Abstract class of back-end queries.  Back-end inherits from this to provide
    // correct answers to queries.  Use getBackEndQueries to get a real one.  The
    // methods here can be used by the derived class as defaults or as initial
    // values that some of are then overridden.
    class BackEnd {
    public:

        BackEnd()
        {
            for (int d = 0; d < EDiCount; ++d)
                decompose[d] = false;
        }

        virtual ~BackEnd() {};

        // despite being pure virtual, there is a base implementation available
        virtual void getRegisterForm(int& outerSoA, int& innerAoS) = 0;

        virtual void getControlFlowMode(EFlowControlMode& flowControlMode,
                                        bool& breakOp, bool& continueOp,
                                        bool& earlyReturnOp, bool& discardOp)
        {
            flowControlMode = EFcmStructuredOpCodes;
            breakOp = true;
            continueOp = true;
            earlyReturnOp = true;
            discardOp = true;
        }

        virtual bool decomposeIntrinsic(int intrinsic)
        {
            return decompose[intrinsic];
        }

        virtual bool preferRegistersOverMemory()
        {
            return true;
        }

        virtual bool getRemovePhiFunctions()
        {
            return true;
        }

        // Does the backend want to be notified before the control splits what the
        // targets of phi functions will be when eliminating phi functions?
        virtual bool getDeclarePhiCopies()
        {
            return false;
        }

    protected:
        bool decompose[EDiCount];

    };
};

#endif /* Backend_H */
