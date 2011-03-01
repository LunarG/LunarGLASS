//===- LunarGLASSBackend.h - Public interface to LunarGLASS Backends ------===//
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
// Public interface to LunarGLASS Backends.
//
// Don't include Mesa headers here.  LunarGLASS is not Mesa-dependent.
//
// Don't include LLVM headers here.  Users of LunarGLASS don't need to
// pull in LLVM headers.
//
//===----------------------------------------------------------------------===//

#pragma once
#ifndef LunarGLASSBackend_H
#define LunarGLASSBackend_H

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

#endif /* LunarGLASSBackend_H */
