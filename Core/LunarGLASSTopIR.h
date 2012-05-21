//===- LunarGLASSTopIR.h - Public interface to LunarGLASS Top IR ----------===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (C) 2010-2011 LunarG, Inc.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice (including the next
// paragraph) shall be included in all copies or substantial portions of the
// Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//
//===----------------------------------------------------------------------===//
//
// Author: John Kessenich, LunarG
//
// Public interface to LunarGLASS Top IR.
//
//===----------------------------------------------------------------------===//

#pragma once
#ifndef LunarGLASSTopIR_H
#define LunarGLASSTopIR_H

#include "llvm/Support/IRBuilder.h"

namespace gla {

    enum ESamplerType {
        ESamplerBuffer,
        ESampler1D,
        ESampler2D,
        ESampler3D,
        ESamplerCube,
        ESampler2DRect,
        ESampler2DMS,
    };

    enum ETextureFlags {
        ETFProjected    = 0x0001,
        ETFBias         = 0x0002,
        ETFLod          = 0x0004,
        ETFShadow       = 0x0008,
        ETFArrayed      = 0x0010,
        ETFFetch        = 0x0020,
        ETFGather       = 0x0040,
        ETFBiasLodArg   = 0x0080,
        ETFOffsetArg    = 0x0100,
        ETFSampleArg    = 0x0200,
        ETFComponentArg = 0x0400,
        ETFRefZArg      = 0x0800,
        ETFProjectedArg = 0x1000,
    };

    // Texture op, for mapping operands
    enum ETextureOperand {
        ETOSamplerType = 0,  // These numbers match the LunarGLASS IR spec
        ETOSamplerLoc  = 1,
        ETOFlag        = 2,
        ETOCoord       = 3,
        ETOBiasLod     = 4,
        ETORefZ        = 5,
        ETOOffset      = 6,
        ETODPdx        = 7,
        ETODPdy        = 8,
    };

    inline int GetTextureOpIndex(ETextureOperand operand, bool SoA = false, int numComps = 0, int comp = 0)
    {
        if (!SoA)
            return operand;

        if (operand < ETOCoord)
            return operand;

        if (operand == ETOCoord)
            return ETOCoord + comp;

        if (operand == ETOBiasLod)
            return ETOCoord + numComps;

        if (operand == ETORefZ)
            return ETOCoord + numComps + 1;

        return ETOCoord + numComps + 1 + (operand - ETOOffset) * numComps + comp;
    }

    const unsigned int GlobalAddressSpace = 0;
    const unsigned int ResourceAddressSpace = 1;
    const unsigned int ConstantAddressSpaceBase = 2;  // use multiple constant spaces through...
    // ConstantAddressSpaceBase + space, where 'space' is 0, 1, 2, ...

    enum EInterpolationMethod {
        EIMNone,  // also for flat
        EIMSmooth,
        EIMNoperspective,
        EIMLast
    };

    enum EInterpolationLocation {
        EILFragment,
        EILSample,
        EILCentroid,
        EILLast
    };

    struct EInterpolationMode {
        #if (EIMLast > 3) || (EILLast > 3)
            #error InterpolationMode fields too small
        #endif
        unsigned int EIMMethod   : 2;
        unsigned int EIMLocation : 2;
    };

    // This is the Top IR definition of shader types
    inline const llvm::Type* GetVoidType  (llvm::LLVMContext& context)   { return llvm::Type::getVoidTy  (context); }
    inline const llvm::Type* GetIntType   (llvm::LLVMContext& context)   { return llvm::Type::getInt32Ty (context); }
    inline const llvm::Type* GetUintType  (llvm::LLVMContext& context)   { return llvm::Type::getInt32Ty (context); }
    inline const llvm::Type* GetBoolType  (llvm::LLVMContext& context)   { return llvm::Type::getInt1Ty  (context); }
    inline const llvm::Type* GetFloatType (llvm::LLVMContext& context)   { return llvm::Type::getFloatTy (context); }
    inline const llvm::Type* GetDoubleType(llvm::LLVMContext& context)   { return llvm::Type::getDoubleTy(context); }

    // Encode where components come from.
    // E.g. 'c2' is the index (0..3) of where component 2 comes from
    inline int MakeSwizzleMask(int c0, int c1, int c2, int c3) 
    {
        return (c0 << 0) | (c1 << 2) | (c2 << 4) | (c3 << 6);
    }

    // Decode where component c comes from.
    inline int GetSwizzle(int glaSwizzle, int c)
    {
        return (glaSwizzle >> c*2) & 0x3;
    }
};

#endif /* LunarGLASSTopIR_H */
