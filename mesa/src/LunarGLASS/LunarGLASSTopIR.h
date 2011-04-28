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
// Author: Cody Northrop, LunarG
//
// Public interface to LunarGLASS.
//
// Don't include Mesa headers here.  LunarGLASS is not Mesa-dependent.
//
// Don't include LLVM headers here.  Users of LunarGLASS don't need to
// pull in LLVM headers.
//
//===----------------------------------------------------------------------===//

#pragma once
#ifndef LunarGLASSTopIR_H
#define LunarGLASSTopIR_H

namespace llvm {
    class Module;
    class Value;
};

namespace gla {

    // Abstract class of external manager of translations within LunarGLASS.
    // Use getManager() to get a concrete manager.
    class Manager {
    public:
        Manager() {}
        virtual ~Manager() {};

        virtual void setModule(llvm::Module*) = 0;
        virtual void translateTopToBottom() = 0;
        virtual void translateBottomToTarget() = 0;
    };

    Manager* getManager();

    enum ESamplerType {
        ESamplerBuffer,
        ESampler1D,
        ESampler2D,
        ESampler3D,
        ESamplerCube,
        ESampler2DRect,
        ESampler1DShadow,
        ESampler2DShadow,
        ESamplerCubeShadow,
        ESampler2DRectShadow,
        ESampler1DArray,
        ESampler2DArray,
        ESamplerCubeArray,
        ESampler1DArrayShadow,
        ESampler2DArrayShadow,
        ESamplerCubeArrayShadow,
        ESampler2DMS,
        ESampler2DMSArray,
    };

    struct ETextureFlags {
        unsigned EProjected : 1;
        unsigned EBias      : 1;
        unsigned ELod       : 1;
        unsigned ECompare   : 1;
        unsigned EOffset    : 1;
        unsigned ESample    : 1;
        unsigned EComp      : 1;
        unsigned ERefZ      : 1;
    };

    // Texture op constants, for mapping operands
    const int SamplerLocAOS = 1;
    const int FlagLocAOS    = 2;
    const int CoordLocAOS   = 3;
    const int BiasLocAOS    = 4;
    const int DdxLocAOS     = 6;
    const int DdyLocAOS     = 7;

    const unsigned int GlobalAddressSpace = 0;
    const unsigned int UniformAddressSpace = 1;

    enum EInterpolationMode {
        EIMNone,  // also for flat
        EIMSmooth,
        EIMNoperspective
    };

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
