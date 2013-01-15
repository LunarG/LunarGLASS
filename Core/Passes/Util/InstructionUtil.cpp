//===- InstructionUtil.cpp - Utility functions for instructions -----------===//
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
// Author: Michael Ilseman, LunarG
//
// Provides utility functions for Instructions
//
//===----------------------------------------------------------------------===//

#include "Passes/Util/InstructionUtil.h"

#include "llvm/Instructions.h"

#include "LunarGLASSTopIR.h"
#include "Util.h"

#include <cmath>

namespace gla_llvm {
using namespace gla;
using namespace llvm;

bool RepresentsSwizzle(const Instruction* inst)
{
    if (isa<ShuffleVectorInst>(inst))
        return IsUndef(inst->getOperand(0)) || IsUndef(inst->getOperand(1));

    // Otherwise, see if we're a swizzling MultiInsert
    if (! IsMultiInsert(inst))
        return false;

    // MultiInserts all from the same source into undef are swizzles
    if (IsDefined(inst->getOperand(0)))
        return false;

    // Make sure they are from the same source
    return GetMultiInsertUniqueSource(inst) != 0;
}

bool IsNegation(const Instruction* inst)
{
    if (! (inst->getOpcode() == Instruction::FSub || inst->getOpcode() == Instruction::Sub))
        return false;

    const Constant* left;

    // If we're a constant vector, test our splat value, otherwise test
    // ourselves. This is because -0 for a floating point vector isn't
    // necessarily aggregate zero.
    if (const ConstantDataVector* leftVec = dyn_cast<const ConstantDataVector>(inst->getOperand(0)))
        left = leftVec->getSplatValue();
    else if (const ConstantVector* leftVec = dyn_cast<const ConstantVector>(inst->getOperand(0)))
        left = leftVec->getSplatValue();
    else
        left = dyn_cast<const Constant>(inst->getOperand(0));

    if (! left)
        return false;

    return left->isNegativeZeroValue();
}

bool IsTextureInstruction(const IntrinsicInst* intr)
{
    switch (intr->getIntrinsicID()) {
    case Intrinsic::gla_textureSample:
    case Intrinsic::gla_fTextureSample:
    case Intrinsic::gla_rTextureSample1:
    case Intrinsic::gla_fRTextureSample1:
    case Intrinsic::gla_rTextureSample2:
    case Intrinsic::gla_fRTextureSample2:
    case Intrinsic::gla_rTextureSample3:
    case Intrinsic::gla_fRTextureSample3:
    case Intrinsic::gla_rTextureSample4:
    case Intrinsic::gla_fRTextureSample4:
    case Intrinsic::gla_textureSampleLodRefZ:
    case Intrinsic::gla_fTextureSampleLodRefZ:
    case Intrinsic::gla_rTextureSampleLodRefZ1:
    case Intrinsic::gla_fRTextureSampleLodRefZ1:
    case Intrinsic::gla_rTextureSampleLodRefZ2:
    case Intrinsic::gla_fRTextureSampleLodRefZ2:
    case Intrinsic::gla_rTextureSampleLodRefZ3:
    case Intrinsic::gla_fRTextureSampleLodRefZ3:
    case Intrinsic::gla_rTextureSampleLodRefZ4:
    case Intrinsic::gla_fRTextureSampleLodRefZ4:
    case Intrinsic::gla_textureSampleLodRefZOffset:
    case Intrinsic::gla_fTextureSampleLodRefZOffset:
    case Intrinsic::gla_rTextureSampleLodRefZOffset1:
    case Intrinsic::gla_fRTextureSampleLodRefZOffset1:
    case Intrinsic::gla_rTextureSampleLodRefZOffset2:
    case Intrinsic::gla_fRTextureSampleLodRefZOffset2:
    case Intrinsic::gla_rTextureSampleLodRefZOffset3:
    case Intrinsic::gla_fRTextureSampleLodRefZOffset3:
    case Intrinsic::gla_rTextureSampleLodRefZOffset4:
    case Intrinsic::gla_fRTextureSampleLodRefZOffset4:
    case Intrinsic::gla_textureSampleLodRefZOffsetGrad:
    case Intrinsic::gla_fTextureSampleLodRefZOffsetGrad:
    case Intrinsic::gla_rTextureSampleLodRefZOffsetGrad1:
    case Intrinsic::gla_fRTextureSampleLodRefZOffsetGrad1:
    case Intrinsic::gla_rTextureSampleLodRefZOffsetGrad2:
    case Intrinsic::gla_fRTextureSampleLodRefZOffsetGrad2:
    case Intrinsic::gla_rTextureSampleLodRefZOffsetGrad3:
    case Intrinsic::gla_fRTextureSampleLodRefZOffsetGrad3:
    case Intrinsic::gla_rTextureSampleLodRefZOffsetGrad4:
    case Intrinsic::gla_fRTextureSampleLodRefZOffsetGrad4:
    case Intrinsic::gla_texelFetchOffset:
    case Intrinsic::gla_fTexelFetchOffset:
    case Intrinsic::gla_texelGather:
    case Intrinsic::gla_fTexelGather:
    case Intrinsic::gla_texelGatherOffset:
    case Intrinsic::gla_fTexelGatherOffset:
    case Intrinsic::gla_texelGatherOffsets:
    case Intrinsic::gla_fTexelGatherOffsets:
        return true;

    } // end of switch (intr->getIntrinsicID())

    return false;
}


void GetMultiInsertSelects(const Instruction* miInst, SmallVectorImpl<Constant*>& channelSelects)
{
    assert(IsMultiInsert(miInst));

    int wmask = GetConstantInt(miInst->getOperand(1));

    for (int i = 0; i < GetComponentCount(miInst); ++i) {
        // If the writemask isn't specified for this one, put in an undef
        if (! MultiInsertWritesComponent(wmask, i)) {
            channelSelects.push_back(UndefValue::get(GetIntType(miInst->getContext())));
            continue;
        }

        int selectBitIndex = 3 + i*2;

        Constant* elt = dyn_cast<Constant>(miInst->getOperand(selectBitIndex));
        assert(elt);

        channelSelects.push_back(elt);
    }
}

Value* GetMultiInsertUniqueSource(const Instruction* miInst)
{
    assert(IsMultiInsert(miInst));

    Value* source = 0;
    unsigned wmask = GetConstantInt(miInst->getOperand(1));

    // Make sure they are from the same source
    for (int i = 0; i < 4; ++i) {
        if (! MultiInsertWritesComponent(wmask, i))
            continue;

        int operandIndex = (i+1) * 2;

        if (source == miInst->getOperand(operandIndex))
            continue;

        // If we've set source, but we're not the same, there's no unique source
        if (source)
            return 0;

        source = miInst->getOperand(operandIndex);
    }

    return source;
}
} // end namespace gla_llvm


