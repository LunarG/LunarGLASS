//===- Util.h - Help build/query LLVM for LunarGLASS --------------------=====//
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
// Author: Cody Northrop, LunarG
//
//===----------------------------------------------------------------------===//

#pragma once
#ifndef Util_H
#define Util_H

#include "LunarGLASSTopIR.h"

// LLVM includes
#pragma warning(push, 1)
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/IntrinsicInst.h"
#pragma warning(pop)

// Forward decls
namespace llvm {
    class BasicBlock;
    class Loop;
} // end namespace llvm

namespace gla {

    //
    // Some utility query/make functions.
    //

    inline llvm::Constant* VectorizeConstant(int numComponents, llvm::Constant* constant)
    {
        if (numComponents == 1) {
            return constant;
        } else {
            llvm::SmallVector<llvm::Constant*, 4> vector(numComponents, constant);

            return llvm::ConstantVector::get(vector);
        }
    }

    // extract integer value or assert trying
    int GetConstantInt(const llvm::Value*);

    // Get floating point value or assert trying
    float GetConstantFloat(const llvm::Value*);
    double GetConstantDouble(const llvm::Value*);

    // Whether the argument is undefined or defined (an undef in llvm)
    inline bool IsUndef(const llvm::Value* val) { return llvm::isa<llvm::UndefValue>(val); }
    inline bool IsDefined(const llvm::Value* val) { return !IsUndef(val); }
    bool AreAllDefined(const llvm::Value* val);
    bool AreAllUndefined(const llvm::Value* val);

    inline bool AreScalar(llvm::ArrayRef<llvm::Value*> vals)
    {
        for (llvm::ArrayRef<llvm::Value*>::iterator i = vals.begin(), e = vals.end(); i != e; ++i) {
            if (! IsScalar(*i))
                return false;
        }

        return true;
    }

    // Is the given op a component-wise operation?
    bool IsPerComponentOp(const llvm::IntrinsicInst* intr);
    bool IsPerComponentOp(const llvm::Instruction* inst);
    bool IsPerComponentOp(const llvm::Value* value);

    // true if all bits in the argument are set
    bool HasAllSet(const llvm::Value*);

    // is the name something like "%42"?
    inline bool IsTempName(llvm::StringRef name)
    {
        return name.size() < 2 || (name[1] >= '0' && name[1] <= '9');
    }

    void RemoveInlineNotation(std::string& name);

    bool ConvertValuesToUnsigned(unsigned*, int &, llvm::ArrayRef<llvm::Value*>);

};  // end gla namespace

#endif // Util_H
