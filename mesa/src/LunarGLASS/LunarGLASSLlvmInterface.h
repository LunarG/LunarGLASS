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

    // Helpers to make constants
    inline llvm::Constant* MakeBoolConstant(llvm::LLVMContext& context, int i) { return llvm::ConstantInt::get(context, llvm::APInt(1, i, false)); }
    inline llvm::Constant* MakeBoolConstant(llvm::LLVMContext& context, bool True) { return llvm::ConstantInt::get(context, llvm::APInt(True ? 1 : 0, false)); }
    inline llvm::Constant* MakeUnsignedConstant(llvm::LLVMContext& context, int i) { return llvm::ConstantInt::get(context, llvm::APInt(32, i, false)); }
    inline llvm::Constant* MakeIntConstant(llvm::LLVMContext& context, int i) { return llvm::ConstantInt::get(context, llvm::APInt(32, i, true)); }
    inline llvm::Constant* MakeFloatConstant(llvm::LLVMContext& context, float f) { return llvm::ConstantFP::get(context, llvm::APFloat(f)); };
        
    // extract integer value or assert trying
    int GetConstantInt(const llvm::Value*);

    // Get floating point value or assert trying
    float GetConstantFloat(const llvm::Value*);

    int GetComponentCount(const llvm::Type*);
    int GetComponentCount(const llvm::Value*);

    // Whether the argument is undefined or defined (an undef in llvm)
    inline bool IsUndef(const llvm::Value* val) { return llvm::isa<llvm::UndefValue>(val); }
    inline bool IsDefined(const llvm::Value* val) { return !IsUndef(val); }

    // true if a scalar Boolean or vector of Boolean
    bool IsBoolean(const llvm::Type*);

    inline bool IsScalar(const llvm::Type* type) { return llvm::Type::VectorTyID != type->getTypeID(); }
    inline bool IsScalar(const llvm::Value* value) { return IsScalar(value->getType()); }

    inline bool IsVector(const llvm::Type* type) { return type->getTypeID() == llvm::Type::VectorTyID; }
    inline bool IsVector(const llvm::Value* value) { return IsVector(value->getType()); }

    // true if all bits in the argument are set
    bool HasAllSet(const llvm::Value*);

    // is the name something like "%42"?
    inline bool IsTempName(const std::string& name)
    {
        return name.length() < 2 || (name[1] >= '0' && name[1] <= '9');
    }

    llvm::Type::TypeID GetBasicType(llvm::Value*);
    llvm::Type::TypeID GetBasicType(const llvm::Type*);

};  // end gla namespace

#endif // LunarGLASSLlvmInterface_H
