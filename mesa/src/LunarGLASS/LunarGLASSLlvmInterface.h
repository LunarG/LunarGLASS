//===- LunarGLASSLlvmInterface.h - Help build/query LLVM for LunarGLASS -=====//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (c) 2011, LunarG, Inc.
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
//===----------------------------------------------------------------------===//

#pragma once
#ifndef LunarGLASSLlvmInterface_H
#define LunarGLASSLlvmInterface_H

// LLVM includes
#include "llvm/IntrinsicInst.h"

namespace gla {

    //
    // the Top Interface to help build (along with LLVM) the top IR
    //

    class Top {
    public:
        llvm::Value* buildMatrixTimesVector(llvm::Value* lmatrix, llvm::Value* rvector);
        llvm::Value* buildVectorTimesMatrix(llvm::Value* lvector, llvm::Value* rmatrix);
        llvm::Value* buildMatrixTimesMatrix(llvm::Value* lmatrix, llvm::Value* rmatrix);
        llvm::Value* buildOuterProduct     (llvm::Value* lvector, llvm::Value* rvector);
        llvm::Value* buildMatrixTranspose  (llvm::Value* matrix);
        llvm::Value* buildMatrixInverse    (llvm::Value* matrix);
    };

    //
    // some utility query functions
    //

    class Util {
    public:

        // get integer value or assert trying
        static int Util::getConstantInt(const llvm::Value*);

        // get floating point value or assert trying
        static float Util::GetConstantFloat(const llvm::Value*);

        static int Util::isGradientTexInst(const llvm::IntrinsicInst* instruction) {
            return (instruction->getIntrinsicID() == llvm::Intrinsic::gla_fTextureSampleLodOffsetGrad);
        }

        static int Util::getComponentCount(const llvm::Type*);
        static int Util::getComponentCount(const llvm::Value*);
        static bool isConsecutiveSwizzle(int glaSwizzle, int width);

        // Whether the argument is undefined or defined (an undef in llvm)
        static bool isUndef(const llvm::Value* val) {
            return llvm::isa<llvm::UndefValue>(val);
        }
        static bool Util::isDefined(const llvm::Value* val) {
            return !isUndef(val);
        }

        // true if a scalar Boolean or vector of Boolean
        static bool isGlaBoolean(const llvm::Type*);

        static bool isGlaScalar(const llvm::Type* type) {
            return llvm::Type::VectorTyID != type->getTypeID();
        }

        // true if all bits in the argument are set
        static bool hasAllSet(const llvm::Value*);

        // is the name something like "%42"?
        static bool Util::isTempName(const std::string& name) {
            return name.length() < 2 || (name[1] >= '0' && name[1] <= '9');
        }
    };
};

#endif // LunarGLASSLlvmInterface_H
