//===- LunarGLASSTopIR.h - Public interface to LunarGLASS Top IR ----------===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (c) 2010-2013 LunarG, Inc.
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
//
// Public interface to LunarGLASS Top IR.
//
//===----------------------------------------------------------------------===//

#pragma once
#ifndef LunarGLASSTopIR_H
#define LunarGLASSTopIR_H

#include "llvm/IRBuilder.h"

namespace gla {

    enum ESamplerType {
        ESamplerBuffer,
        ESampler1D,
        ESampler2D,
        ESampler3D,
        ESamplerCube,
        ESampler2DRect,
        ESampler2DMS,
    };

    enum ETextureFlags {
        ETFProjected    = 0x0001,
        ETFBias         = 0x0002,
        ETFLod          = 0x0004,
        ETFShadow       = 0x0008,
        ETFArrayed      = 0x0010,
        ETFFetch        = 0x0020,
        ETFGather       = 0x0040,
        ETFBiasLodArg   = 0x0080,
        ETFOffsetArg    = 0x0100,
        ETFSampleArg    = 0x0200,
        ETFComponentArg = 0x0400,
        ETFRefZArg      = 0x0800,
        ETFProjectedArg = 0x1000,
    };

    // Texture op, for mapping operands
    enum ETextureOperand {
        ETOSamplerType = 0,  // These numbers match the LunarGLASS IR spec
        ETOSamplerLoc  = 1,
        ETOFlag        = 2,
        ETOCoord       = 3,
        ETOBiasLod     = 4,
        ETORefZ        = 5,
        ETOOffset      = 6,
        ETODPdx        = 7,
        ETODPdy        = 8,
    };

    inline int GetTextureOpIndex(ETextureOperand operand, bool SoA = false, int numComps = 0, int comp = 0)
    {
        if (!SoA)
            return operand;

        if (operand < ETOCoord)
            return operand;

        if (operand == ETOCoord)
            return ETOCoord + comp;

        if (operand == ETOBiasLod)
            return ETOCoord + numComps;

        if (operand == ETORefZ)
            return ETOCoord + numComps + 1;

        return ETOCoord + numComps + 1 + (operand - ETOOffset) * numComps + comp;
    }

    const unsigned int GlobalAddressSpace = 0;
    const unsigned int ResourceAddressSpace = 1;
    const unsigned int ConstantAddressSpaceBase = 2;  // use multiple constant spaces through...
    // ConstantAddressSpaceBase + space, where 'space' is 0, 1, 2, ...

    enum EInterpolationMethod {
        EIMNone,  // also for flat
        EIMSmooth,
        EIMNoperspective,
        EIMLast
    };

    enum EInterpolationLocation {
        EILFragment,
        EILSample,
        EILCentroid,
        EILLast
    };

    typedef unsigned int EInterpolationMode;

    inline EInterpolationMode MakeInterpolationMode(EInterpolationMethod method, EInterpolationLocation location)
    {
        return method | (location << 8);
    }

    inline void CrackInterpolationMode(EInterpolationMode mode, EInterpolationMethod& method, EInterpolationLocation& location)
    {
        location = static_cast<EInterpolationLocation>((mode >> 8) & 0xFF);
        method = static_cast<EInterpolationMethod>(mode & 0xFF);
    }

    // This is the Top IR definition of shader types
    inline llvm::Type* GetVoidType  (llvm::LLVMContext& context)   { return llvm::Type::getVoidTy  (context); }
    inline llvm::Type* GetIntType   (llvm::LLVMContext& context)   { return llvm::Type::getInt32Ty (context); }
    inline llvm::Type* GetUintType  (llvm::LLVMContext& context)   { return llvm::Type::getInt32Ty (context); }
    inline llvm::Type* GetBoolType  (llvm::LLVMContext& context)   { return llvm::Type::getInt1Ty  (context); }
    inline llvm::Type* GetFloatType (llvm::LLVMContext& context)   { return llvm::Type::getFloatTy (context); }
    inline llvm::Type* GetDoubleType(llvm::LLVMContext& context)   { return llvm::Type::getDoubleTy(context); }

    inline llvm::Type* GetBasicType(llvm::Type* type)
    {
        switch(type->getTypeID()) {
        case llvm::Type::VectorTyID:
        case llvm::Type::ArrayTyID:
            return GetBasicType(type->getContainedType(0));
        }

        return type;
    }

    inline llvm::Type* GetBasicType(llvm::Value* value)
    {
        return GetBasicType(value->getType());
    }

    inline llvm::Type::TypeID GetBasicTypeID(llvm::Type* type)
    {
        return GetBasicType(type)->getTypeID();
    }

    inline llvm::Type::TypeID GetBasicTypeID(const llvm::Value* value)
    {
        return GetBasicTypeID(value->getType());
    }

    inline llvm::Type* GetVectorOrScalarType(llvm::Type* type, int numComponents)
    {
        type = type->isVectorTy() ? type->getContainedType(0) : type;

        assert(numComponents > 0);

        if (numComponents <= 1)
            return type;
     
        return llvm::VectorType::get(type, numComponents);
    }


    // Helpers to make constants
    inline llvm::Constant* MakeBoolConstant(llvm::LLVMContext& context, int i)      { return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context),  i, false); }
    inline llvm::Constant* MakeBoolConstant(llvm::LLVMContext& context, bool True)  { return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context),  True ? 1 : 0, false); }
    inline llvm::Constant* MakeUnsignedConstant(llvm::LLVMContext& context, int i)  { return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), i, false); }
    inline llvm::Constant* MakeIntConstant(llvm::LLVMContext& context, int i)       { return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), i, true); }
    inline llvm::Constant* MakeFloatConstant(llvm::LLVMContext& context, float f)   { return llvm::ConstantFP::get(llvm::Type::getFloatTy(context),  f); };
    inline llvm::Constant* MakeDoubleConstant(llvm::LLVMContext& context, double f) { return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context), f); };

    //
    // A value in Top IR is exactly one of the following:
    //  - scalar
    //  - vector
    //  - aggregate
    //
    // Scalar means a single component; not an array, not a vector, not a struct.
    //
    // A vector is a set of scalars, arranged as an llvm vector.
    //
    // Aggregate means only
    //  - array
    //  - struct
    //
    // If a matrix has been converted to llvm as an array of columns, then it will
    // also be an aggregate.
    //
    inline bool IsVector(const llvm::Type* type)      { return type->isVectorTy(); }
    inline bool IsAggregate(const llvm::Type* type)   { return type->isAggregateType(); }
    inline bool IsScalar(const llvm::Type* type)      { return ! IsAggregate(type) && ! IsVector(type); }
    
    inline llvm::VectorType* GetColumnType (const llvm::Type* type)  { return llvm::dyn_cast<llvm::VectorType>(type->getContainedType(0)); }
    inline llvm::Type* GetMatrixElementType(const llvm::Type* type)  { return GetColumnType(type)->getContainedType(0); }

    inline int GetNumColumns(const llvm::Type* type)    { return llvm::dyn_cast<llvm::ArrayType>(type)->getNumElements(); }
    inline int GetNumColumns(const llvm::Value* value)  { return GetNumColumns(value->getType()); }

    inline int GetNumRows(const llvm::Type* type)       { return GetColumnType(type)->getNumElements(); }
    inline int GetNumRows(const llvm::Value* value)     { return GetNumRows(value->getType()); }

    inline bool IsVector(const llvm::Value* value)    { return IsVector(value->getType()); }
    inline bool IsAggregate(const llvm::Value* value) { return IsAggregate(value->getType()); }
    inline bool IsScalar(const llvm::Value* value)    { return IsScalar(value->getType()); }

    // true if argument is a scalar Boolean or vector of Boolean
    inline bool IsBoolean(const llvm::Type* type)
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

    inline int GetComponentCount(llvm::Type* type)
    {
        if (llvm::VectorType* vTy = llvm::dyn_cast<llvm::VectorType>(type))
            return vTy->getNumElements();
        else
            return 1;
    }

    inline int GetComponentCount(const llvm::Value* value)
    {
        return GetComponentCount(value->getType());
    }

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
