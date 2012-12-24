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
    
    // this might still be a constant expression, rather than a numeric constant,
    // e.g., expression with undef's in it, so it was not folded
    if (! constantInt) {
        value->dump();
        gla::UnsupportedFunctionality("non-simple constant");
    }

    return constantInt->getValue().getSExtValue();
}

float GetConstantFloat(const llvm::Value* value)
{
    const llvm::ConstantFP *constantFP = llvm::dyn_cast<llvm::ConstantFP>(value);
    
    // this might still be a constant expression, rather than a numeric constant,
    // e.g., expression with undef's in it, so it was not folded
    if (! constantFP) {
        value->dump();
        gla::UnsupportedFunctionality("non-simple constant");
    }

    return constantFP->getValueAPF().convertToFloat();
}

double GetConstantDouble(const llvm::Value* value)
{
    const llvm::ConstantFP *constantFP = llvm::dyn_cast<llvm::ConstantFP>(value);
        
    // this might still be a constant expression, rather than a numeric constant,
    // e.g., expression with undef's in it, so it was not folded
    if (! constantFP) {
        value->dump();
        gla::UnsupportedFunctionality("non-simple constant");
    }

    return constantFP->getValueAPF().convertToDouble();
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
                    if (! AreAllDefined(*i))
                        return false;
                }
            }
        }
    }

    return true;
}

// Returns true if base value is undef, or if all members are undef
bool AreAllUndefined(const llvm::Value* value)
{
    if (IsUndef(value))
        return true;

    // Assume a fully undef aggregate satisfies "IsUndef" at the highest level
    return false;

    // If a fully undef aggregate ever needs to be walked to to verify that,
    // use the following code.
    //if (const llvm::User* user = llvm::dyn_cast<llvm::User>(value)) {
    //    switch(user->getType()->getTypeID()) {
    //    case llvm::Type::VectorTyID:
    //    case llvm::Type::ArrayTyID:
    //    case llvm::Type::StructTyID:
    //        if (user->getNumOperands() > 0) {
    //            for (llvm::User::const_op_iterator i = user->op_begin(), e = user->op_end(); i != e; ++i) {
    //                if (! AreAllUndefined(*i))
    //                    return false;
    //            }
    //        }
    //    }
    //}

    //return true;
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

void AppendArraySizeToName(std::string& arrayName, int size)
{
    arrayName.append("_");
    llvm::raw_string_ostream out(arrayName);
    out << size;
    arrayName = out.str();
}

void GetArraySizeFromName(const std::string& arrayName, std::string& basename, int& size)
{
    size = 0;
    if (arrayName.back() == ']') {
        int sizePos = arrayName.find_last_of('[');
        if (sizePos != std::string::npos) {
            basename = arrayName.substr(0, sizePos);
            --sizePos;
            int power = 1;
            while (sizePos > 0) {
                int num = arrayName[sizePos] - '0';
                if (num < 0 || num > 9)
                    break;
                size += num * power;
                power *= 10;
                --sizePos;
            }
        }
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

bool IsPerComponentOp(const llvm::IntrinsicInst* intr)
{
    switch (intr->getIntrinsicID()) {
    // Pipeline ops
    case llvm::Intrinsic::gla_readData :
    case llvm::Intrinsic::gla_fReadData :
    case llvm::Intrinsic::gla_fWriteInterpolant :
    case llvm::Intrinsic::gla_fReadInterpolant :
    case llvm::Intrinsic::gla_fReadInterpolantOffset :
    case llvm::Intrinsic::gla_getInterpolant :
    case llvm::Intrinsic::gla_writeData :
    case llvm::Intrinsic::gla_fWriteData :

    // Packing
    case llvm::Intrinsic::gla_fPackUnorm2x16:
    case llvm::Intrinsic::gla_fPackUnorm4x8:
    case llvm::Intrinsic::gla_fPackSnorm4x8:
    case llvm::Intrinsic::gla_fUnpackUnorm2x16:
    case llvm::Intrinsic::gla_fUnpackUnorm4x8:
    case llvm::Intrinsic::gla_fUnpackSnorm4x8:
    case llvm::Intrinsic::gla_fPackDouble2x32:
    case llvm::Intrinsic::gla_fUnpackDouble2x32:

    // Texture Sampling
    case llvm::Intrinsic::gla_textureSample:
    case llvm::Intrinsic::gla_fTextureSample:
    case llvm::Intrinsic::gla_rTextureSample1:
    case llvm::Intrinsic::gla_fRTextureSample1:
    case llvm::Intrinsic::gla_rTextureSample2:
    case llvm::Intrinsic::gla_fRTextureSample2:
    case llvm::Intrinsic::gla_rTextureSample3:
    case llvm::Intrinsic::gla_fRTextureSample3:
    case llvm::Intrinsic::gla_rTextureSample4:
    case llvm::Intrinsic::gla_fRTextureSample4:
    case llvm::Intrinsic::gla_textureSampleLodRefZ:
    case llvm::Intrinsic::gla_fTextureSampleLodRefZ:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZ1:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZ1:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZ2:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZ2:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZ3:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZ3:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZ4:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZ4:
    case llvm::Intrinsic::gla_textureSampleLodRefZOffset:
    case llvm::Intrinsic::gla_fTextureSampleLodRefZOffset:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZOffset1:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZOffset1:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZOffset2:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZOffset2:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZOffset3:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZOffset3:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZOffset4:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZOffset4:
    case llvm::Intrinsic::gla_textureSampleLodRefZOffsetGrad:
    case llvm::Intrinsic::gla_fTextureSampleLodRefZOffsetGrad:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZOffsetGrad1:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZOffsetGrad1:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZOffsetGrad2:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZOffsetGrad2:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZOffsetGrad3:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZOffsetGrad3:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZOffsetGrad4:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZOffsetGrad4:
    case llvm::Intrinsic::gla_texelFetchOffset:
    case llvm::Intrinsic::gla_fTexelFetchOffset:
    case llvm::Intrinsic::gla_texelGather:
    case llvm::Intrinsic::gla_fTexelGather:
    case llvm::Intrinsic::gla_texelGatherOffset:
    case llvm::Intrinsic::gla_fTexelGatherOffset:
    case llvm::Intrinsic::gla_texelGatherOffsets:
    case llvm::Intrinsic::gla_fTexelGatherOffsets:

    // Texture Query
    case llvm::Intrinsic::gla_queryTextureSize:
    case llvm::Intrinsic::gla_fQueryTextureLod:

    // Geometry
    case llvm::Intrinsic::gla_fLength:
    case llvm::Intrinsic::gla_fDistance:
    case llvm::Intrinsic::gla_fDot2:
    case llvm::Intrinsic::gla_fDot3:
    case llvm::Intrinsic::gla_fDot4:
    case llvm::Intrinsic::gla_fCross:
    case llvm::Intrinsic::gla_fNormalize:
    case llvm::Intrinsic::gla_fNormalize3D:
    case llvm::Intrinsic::gla_fLit:
    case llvm::Intrinsic::gla_fFaceForward:
    case llvm::Intrinsic::gla_fReflect:
    case llvm::Intrinsic::gla_fRefract:

    // Derivatives/transform
    case llvm::Intrinsic::gla_fDFdx:
    case llvm::Intrinsic::gla_fDFdy:
    case llvm::Intrinsic::gla_fFilterWidth:
    case llvm::Intrinsic::gla_fFixedTransform:

    // Vector ops
    case llvm::Intrinsic::gla_any:
    case llvm::Intrinsic::gla_all:

        return false;
    } // end of switch (intr->getIntrinsicID())

    return true;
}


bool IsPerComponentOp(const llvm::Instruction* inst)
{
    if (const llvm::IntrinsicInst* intr = llvm::dyn_cast<const llvm::IntrinsicInst>(inst))
        return IsPerComponentOp(intr);

    if (inst->isTerminator())
        return false;

    switch (inst->getOpcode()) {

    // Cast ops are only per-component if they cast back to the same vector
    // width
    case llvm::Instruction::Trunc:
    case llvm::Instruction::ZExt:
    case llvm::Instruction::SExt:
    case llvm::Instruction::FPToUI:
    case llvm::Instruction::FPToSI:
    case llvm::Instruction::UIToFP:
    case llvm::Instruction::SIToFP:
    case llvm::Instruction::FPTrunc:
    case llvm::Instruction::FPExt:
    case llvm::Instruction::PtrToInt:
    case llvm::Instruction::IntToPtr:
    case llvm::Instruction::BitCast:
        return GetComponentCount(inst->getOperand(0)) == GetComponentCount(inst);

    // Vector ops
    case llvm::Instruction::InsertElement:
    case llvm::Instruction::ExtractElement:
    case llvm::Instruction::ShuffleVector:

    // Ways of accessing/loading/storing vectors
    case llvm::Instruction::ExtractValue:
    case llvm::Instruction::InsertValue:

    // Memory ops
    case llvm::Instruction::Alloca:
    case llvm::Instruction::Load:
    case llvm::Instruction::Store:
    case llvm::Instruction::GetElementPtr:

    // Phis are a little special. We consider them not to be per-component
    // because the mechanism of choice is a single value (what path we took to
    // get here), and doesn't choose per-component (as select would). The caller
    // should know to handle phis specially
    case llvm::Instruction::PHI:

    // Call insts, conservatively are no per-component
    case llvm::Instruction::Call:

    // Misc
    // case llvm::Instruction::LandingPad:  --- 3.0
    case llvm::Instruction::VAArg:

        return false;


    } // end of switch (inst->getOpcode())

    return true;
}


bool IsPerComponentOp(const llvm::Value* value)
{
    const llvm::Instruction* inst = llvm::dyn_cast<const llvm::Instruction>(value);
    return inst && IsPerComponentOp(inst);
}



}; // end gla namespace
