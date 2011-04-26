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
#include "llvm/Support/IRBuilder.h"
#include "llvm/IntrinsicInst.h"

// Forward decls
namespace llvm {
    class BasicBlock;
    class Loop;
    class DominanceFrontier;
} // end namespace llvm

namespace gla {

    //
    // Some utility query/make functions.
    //

    class Util {
    public:
        // extract integer value or assert trying
        static int getConstantInt(const llvm::Value*);

        // create integer constant
        static llvm::Value* makeUnsignedIntConstant(llvm::LLVMContext& context, int i) { return llvm::ConstantInt::get(context, llvm::APInt(32, i, false)); }
        static llvm::Value* makeIntConstant(llvm::LLVMContext& context, int i) { return llvm::ConstantInt::get(context, llvm::APInt(32, i, true)); }

        // get floating point value or assert trying
        static float GetConstantFloat(const llvm::Value*);

        static int isGradientTexInst(const llvm::IntrinsicInst* instruction)
        {
            return (instruction->getIntrinsicID() == llvm::Intrinsic::gla_fTextureSampleLodOffsetGrad);
        }

        static int getComponentCount(const llvm::Type*);
        static int getComponentCount(const llvm::Value*);
        static bool isConsecutiveSwizzle(int glaSwizzle, int width);

        // Whether the argument is undefined or defined (an undef in llvm)
        static bool isUndef(const llvm::Value* val) { return llvm::isa<llvm::UndefValue>(val); }
        static bool isDefined(const llvm::Value* val) { return !isUndef(val); }

        // true if a scalar Boolean or vector of Boolean
        static bool isGlaBoolean(const llvm::Type*);

        static bool isGlaScalar(const llvm::Type* type) { return llvm::Type::VectorTyID != type->getTypeID(); }
        static bool isGlaScalar(const llvm::Value* value) { return isGlaScalar(value->getType()); }

        static bool isVector(const llvm::Type* type) { return type->getTypeID() == llvm::Type::VectorTyID; }
        static bool isVector(const llvm::Value* value) { return isVector(value->getType()); }

        // true if all bits in the argument are set
        static bool hasAllSet(const llvm::Value*);

        // is the name something like "%42"?
        static bool isTempName(const std::string& name)
        {
            return name.length() < 2 || (name[1] >= '0' && name[1] <= '9');
        }

        // true if provided basic block is one of the (possibly many) latches in
        // the provided loop
        static bool isLatch(const llvm::BasicBlock* bb, llvm::Loop* loop);

        // Return the number of latches in a loop
        static int getNumLatches(llvm::Loop* loop);

        // Return the single merge point of the given conditional basic block. Returns
        // null if there is no merge point, or if there are more than 1 merge
        // points. Note that the presense of backedges or exitedges in the then and else
        // branchs' subgraphs may cause there to be multiple potential merge points.
        static llvm::BasicBlock* getSingleMergePoint(const llvm::BasicBlock* condBB, llvm::DominanceFrontier& domFront);

        static llvm::Type::TypeID getBasicType(llvm::Value*);
        static llvm::Type::TypeID getBasicType(const llvm::Type*);

    };  // end Util class

};  // end gla namespace

#endif // LunarGLASSLlvmInterface_H
