//===- LunarGLASSLlvmInterface.cpp - Help build/query LLVM for LunarGLASS -===//
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
// Author: Michael Ilseman, LunarG
//
//===----------------------------------------------------------------------===//

#include "Exceptions.h"
#include "LunarGLASSLlvmInterface.h"
#include "LunarGLASSTopIR.h"

// LLVM includes
#include "llvm/BasicBlock.h"
#include "llvm/GlobalVariable.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/CFG.h"

namespace gla {

//
// Util definitions
//

int GetConstantInt(const llvm::Value* value)
{
    const llvm::Constant* constant = llvm::dyn_cast<llvm::Constant>(value);
    assert(constant);
    const llvm::ConstantInt *constantInt = llvm::dyn_cast<llvm::ConstantInt>(constant);
    assert(constantInt);
    return constantInt->getValue().getSExtValue();
}

float GetConstantFloat(const llvm::Value* value)
{
    const llvm::Constant* constant = llvm::dyn_cast<llvm::Constant>(value);
    assert(constant);
    const llvm::ConstantFP *constantFP = llvm::dyn_cast<llvm::ConstantFP>(constant);
    assert(constantFP);
    return constantFP->getValueAPF().convertToFloat();
}

int GetComponentCount(const llvm::Type* type)
{
    if (type->getTypeID() == llvm::Type::VectorTyID)
        return llvm::dyn_cast<llvm::VectorType>(type)->getNumElements();
    else
        return 1;
}

int GetComponentCount(const llvm::Value* value)
{
    const llvm::Type* type = value->getType();

    return GetComponentCount(type);
}

bool IsBoolean(const llvm::Type* type)
{
    if (llvm::Type::VectorTyID == type->getTypeID()) {
        if (type->getContainedType(0) == type->getInt1Ty(type->getContext()))
            return true;
    } else {
        if (type == type->getInt1Ty(type->getContext()))
            return true;
    }

    return false;
}

bool HasAllSet(const llvm::Value* value)
{
    if (! llvm::isa<llvm::Constant>(value))
        return false;

    if (IsScalar(value->getType())) {
        return GetConstantInt(value) == -1;
    } else {
        const llvm::ConstantVector* vector = llvm::dyn_cast<llvm::ConstantVector>(value);
        assert(vector);

        for (int op = 0; op < vector->getNumOperands(); ++op) {
            if (GetConstantInt(vector->getOperand(op)) != -1)
                return false;
        }

        return true;
    }
}

llvm::Type::TypeID GetBasicType(llvm::Value* value)
{
    return GetBasicType(value->getType());
}

llvm::Type::TypeID GetBasicType(const llvm::Type* type)
{
    switch(type->getTypeID()) {
    case llvm::Type::VectorTyID:
    case llvm::Type::ArrayTyID:
        return GetBasicType(type->getContainedType(0));
    }

    assert(gla::IsScalar(type));
    return type->getTypeID();
}

}; // end gla namespace
