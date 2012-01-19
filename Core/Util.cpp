//===- Util.cpp - Help build/query LLVM for LunarGLASS --------------------===//
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
#include "Util.h"
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
    const llvm::ConstantInt *constantInt = llvm::dyn_cast<llvm::ConstantInt>(value);
    assert(constantInt);

    return constantInt->getValue().getSExtValue();
}

float GetConstantFloat(const llvm::Value* value)
{
    const llvm::ConstantFP *constantFP = llvm::dyn_cast<llvm::ConstantFP>(value);
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
    return GetComponentCount(value->getType());
}

// Returns false if base value is undef, or if any member is undef
bool AreAllDefined(const llvm::Value* value)
{
    if (IsUndef(value))
        return false;

    if (const llvm::User* user = llvm::dyn_cast<llvm::User>(value)) {
        switch(user->getType()->getTypeID()) {
        case llvm::Type::VectorTyID:
        case llvm::Type::ArrayTyID:
        case llvm::Type::StructTyID:
            if (user->getNumOperands() > 0) {
                for (llvm::User::const_op_iterator i = user->op_begin(), e = user->op_end(); i != e; ++i) {
                    if (IsUndef(*i))
                        return false;
                }
            }
        }
    }

    return true;
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

        for (unsigned int op = 0; op < vector->getNumOperands(); ++op) {
            if (GetConstantInt(vector->getOperand(op)) != -1)
                return false;
        }

        return true;
    }
}

void AppendArrayIndexToName(std::string &arrayName, int index)
{
    arrayName.append("[");
    llvm::raw_string_ostream out(arrayName);
    out << index;
    arrayName = out.str();
    arrayName.append("]");
}

const llvm::Type* GetBasicType(const llvm::Value* value)
{
    return GetBasicType(value->getType());
}

const llvm::Type* GetBasicType(const llvm::Type* type)
{
    switch(type->getTypeID()) {
    case llvm::Type::VectorTyID:
    case llvm::Type::ArrayTyID:
        return GetBasicType(type->getContainedType(0));
    }

    assert(gla::IsScalar(type));
    return type;
}

llvm::Type::TypeID GetBasicTypeID(const llvm::Value* value)
{
    return GetBasicTypeID(value->getType());
}

llvm::Type::TypeID GetBasicTypeID(const llvm::Type* type)
{
    return GetBasicType(type)->getTypeID();
}

// Some interfaces to LLVM builder require unsigned indices instead of a vector.
// i.e. llvm::IRBuilder::CreateExtractValue()
// This method will do the conversion and inform the caller if not every element was
// a constant integer.
bool ConvertValuesToUnsigned(unsigned* indices, int &count, std::vector<llvm::Value*> chain)
{
    std::vector<llvm::Value*>::iterator start = chain.begin();

    for (count = 0; start != chain.end(); ++start, ++count) {
        if (llvm::Constant* constant = llvm::dyn_cast<llvm::Constant>(*start)) {
            if (llvm::ConstantInt *constantInt = llvm::dyn_cast<llvm::ConstantInt>(constant))
                indices[count] = constantInt->getValue().getSExtValue();
            else
                return false;
        } else {
            return false;
        }
    }

    return true;
}

}; // end gla namespace
