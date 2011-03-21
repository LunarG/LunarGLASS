//===- LunarGLASSBottomIR.cpp - Shared public functions for Bottom IR -----===//
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
// Author: Cody Northrop, LunarG
//
//===----------------------------------------------------------------------===//

#include "LunarGLASSBottomIR.h"

// LLVM includes
#include "Exceptions.h"
#include "llvm/IntrinsicInst.h"

namespace gla {

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

    int IsGradientTexInst(const llvm::IntrinsicInst* llvmInstruction)
    {
        return ( llvmInstruction->getIntrinsicID() ==
                 llvm::Intrinsic::gla_fTextureSampleLodOffsetGrad );
    }

    int GetComponentCount(const llvm::Type* type)
    {
        const llvm::VectorType *vectorType = llvm::dyn_cast<llvm::VectorType>(type);

        if (vectorType)
            return vectorType->getNumElements();
        else
            return 1;
    }

    int GetComponentCount(const llvm::Value* value)
    {
        const llvm::Type* type = value->getType();

        return GetComponentCount(type);
    }

    bool IsConsecutiveSwizzle(int glaSwizzle, int width)
    {
        for (int i = 0; i < width; ++i) {
            if (((glaSwizzle >> i*2) & 0x3) != i)
                return false;
        }

        return true;
    }

    bool IsUndef(llvm::Value* val)
    {
        return llvm::isa<llvm::UndefValue>(val);
    }

    bool IsDefined(llvm::Value* val)
    {
        return !IsUndef(val);
    }

};
