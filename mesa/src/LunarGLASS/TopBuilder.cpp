//===- TopBuilder.cpp - Generic Top IR builders -=============================//
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

#include "Exceptions.h"
#include "LunarGLASSLlvmInterface.h"
#include "LunarGLASSTopIR.h"
#include "TopBuilder.h"

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

gla::Builder::~Builder()
{
    for (std::vector<Matrix*>::iterator i = matrixList.begin(); i != matrixList.end(); ++i)
        delete *i;
}


llvm::Constant* Builder::getConstant(std::vector<llvm::Constant*>& constants)
{
    if (constants.size() == 1)
        return constants[0];

    return llvm::ConstantVector::get(constants);
}

gla::Builder::SuperValue Builder::createVariable(EStorageQualifier storageQualifier, int storageInstance,
                                                 const llvm::Type* type, bool isMatrix, llvm::Constant* initializer, const std::string* annotation,
                                                 const std::string& name)
{
    std::string annotatedName;
    if (annotation != 0) {
        annotatedName = *annotation;
        annotatedName.append(" ");
        annotatedName.append(name);
    } else
        annotatedName = name;

    // Set some common default values, which the switch will override
    unsigned int addressSpace = gla::GlobalAddressSpace;
    llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalVariable::InternalLinkage;
    bool global = false;
    bool readOnly = false;

    switch (storageQualifier) {
    case ESQUniform:
        addressSpace = gla::UniformAddressSpace;
        linkage = llvm::GlobalVariable::ExternalLinkage;
        global = true;
        readOnly = true;
        break;

    case ESQInput:
        assert(! "input is handled through intrinsics only");
        break;

    case ESQOutput:
        // This isn't for the actual pipeline output, but for the variable
        // holding the value up until when the epilogue writes out to the pipe.
        // Internal linkage helps with global optimizations,
        // so does having an initializer.
        global = true;
        if (initializer == 0)
            initializer = llvm::Constant::getNullValue(type);

    case ESQGlobal:
        global = true;
        if (initializer == 0)
            initializer = llvm::Constant::getNullValue(type);
        break;

    case ESQLocal:
        break;

    default:
        assert(! "unhandled storage qualifier");
    }

    llvm::Value* value;
    if (global) {
        llvm::GlobalVariable* globalValue = new llvm::GlobalVariable(type, readOnly, linkage, initializer, annotatedName, false /* ThreadLocal */, addressSpace);
        module->getGlobalList().push_back(globalValue);
        value = globalValue;

        if (storageQualifier == ESQOutput) {
            // Track the value that must be copied out to the pipeline at
            // the end of the shader.
            copyOuts.push_back(value);
        }

    } else {
        // LLVM's promote memory to registers only works when
        // alloca is in the entry block.
        llvm::BasicBlock* entryBlock = &builder.GetInsertBlock()->getParent()->getEntryBlock();
        llvm::IRBuilder<> entryBuilder(entryBlock, entryBlock->begin());
        value = entryBuilder.CreateAlloca(type, 0, annotatedName);
    }

    if (isMatrix)
        return newMatrix(value);

    return value;
}

void Builder::copyOutPipeline(llvm::IRBuilder<>& builder)
{
    llvm::Intrinsic::ID intrinsicID;

    std::list<llvm::Value*>::iterator outIter;

    // Call writeData intrinsic on our outs
    for ( outIter = copyOuts.begin(); outIter != copyOuts.end(); outIter++ ) {
        llvm::Value* loadVal = builder.CreateLoad(*outIter);

        switch(Util::getBasicType(loadVal)) {
        case llvm::Type::IntegerTyID:   intrinsicID = llvm::Intrinsic::gla_writeData;   break;
        case llvm::Type::FloatTyID:     intrinsicID = llvm::Intrinsic::gla_fWriteData;  break;
        }

        llvm::Function *intrinsicName = getIntrinsic(intrinsicID, loadVal->getType());

        builder.CreateCall2(intrinsicName, Util::makeIntConstant(loadVal->getContext(), 0), loadVal);
    }
}

llvm::Value* Builder::readPipeline(const llvm::Type* type, std::string& name, int slot, EInterpolationMode mode, float offsetX, float offsetY)
{
    llvm::Constant *slotConstant = Util::makeUnsignedConstant(builder.getContext(), slot);

    // This correction is necessary for some front ends, which might allow
    // "interpolated" integers or Booleans.
    if (Util::getBasicType(type) != llvm::Type::FloatTyID)
        mode = EIMNone;

    llvm::Function *intrinsic;
    if (mode != EIMNone) {
        llvm::Constant *modeConstant = Util::makeUnsignedConstant(builder.getContext(), mode);

        if (offsetX != 0.0 || offsetY != 0.0) {
            std::vector<llvm::Constant*> offsets;
            offsets.push_back(Util::makeFloatConstant(builder.getContext(), offsetX));
            offsets.push_back(Util::makeFloatConstant(builder.getContext(), offsetY));
            llvm::Constant* offset = getConstant(offsets);
            intrinsic = getIntrinsic(llvm::Intrinsic::gla_fReadInterpolantOffset, type, offset->getType());
            return builder.CreateCall3(intrinsic, slotConstant, modeConstant, offset, name);
        } else {
            intrinsic = getIntrinsic(llvm::Intrinsic::gla_fReadInterpolant, type);
            return builder.CreateCall2(intrinsic, slotConstant, modeConstant, name);
        }
    } else {
        switch (Util::getBasicType(type)) {
        case llvm::Type::IntegerTyID:   intrinsic = getIntrinsic(llvm::Intrinsic::gla_readData, type); break;
        case llvm::Type::FloatTyID:     intrinsic = getIntrinsic(llvm::Intrinsic::gla_fReadData, type); break;
        }

        return builder.CreateCall(intrinsic, slotConstant, name);
    }
}

//
// Builder::Matrix definitions
//

Builder::Matrix* Builder::newMatrix(llvm::Value* value)
{
    gla::Builder::Matrix* matrix = new gla::Builder::Matrix(value);
    matrixList.push_back(matrix);

    return matrix;
}

Builder::Matrix::Matrix(llvm::Value* m) : matrix(m)
{
    const llvm::PointerType* pointerType = llvm::dyn_cast<const llvm::PointerType>(matrix->getType());
    const llvm::ArrayType* matrixType;
    if (pointerType)
        matrixType = llvm::dyn_cast<const llvm::ArrayType>(pointerType->getContainedType(0));
    else
        matrixType = llvm::dyn_cast<const llvm::ArrayType>(matrix->getType());
    assert(matrixType);
    numColumns = matrixType->getNumElements();

    const llvm::VectorType* columnType = llvm::dyn_cast<const llvm::VectorType>(matrixType->getElementType());
    assert(columnType);
    numRows = columnType->getNumElements();
}

Builder::Matrix::Matrix(int c, int r, Matrix* oldMatrix) : numColumns(c), numRows(r)
{
    UnsupportedFunctionality("construction of matrix from matrix");
}

const llvm::Type* Builder::Matrix::getType(llvm::Type* elementType, int numColumns, int numRows)
{
    // This is not a matrix... it's a cache of types for all possible matrix sizes.
    static const int minSize = 2;
    static const int maxSize = 4;
    static const llvm::Type* typeCache[maxSize-minSize+1][maxSize-minSize+1] =
        { {0, 0, 0},
          {0, 0, 0},
          {0, 0, 0} };

    assert(numColumns >= minSize && numRows >= minSize);
    assert(numColumns <= maxSize && numRows <= maxSize);

    static const llvm::Type** type = &typeCache[numColumns-minSize][numRows-minSize];
    if (*type == 0) {
        // a matrix is an array of vectors
        llvm::VectorType* columnType = llvm::VectorType::get(elementType, numRows);
        *type = llvm::ArrayType::get(columnType, numColumns);
    }

    return *type;
}

Builder::SuperValue Builder::createMatrixOp(llvm::Instruction::BinaryOps llvmOpcode, Builder::SuperValue left, Builder::SuperValue right)
{
    Builder::SuperValue ret;

    assert(left.isMatrix() || right.isMatrix());

    // component-wise matrix operations on same-shape matrices
    if (left.isMatrix() && right.isMatrix()) {
        assert(left.getMatrix()->getNumColumns() == right.getMatrix()->getNumColumns());
        assert(left.getMatrix()->getNumRows() == right.getMatrix()->getNumRows());

        // spit out the component-wise operations

        UnsupportedFunctionality("component-wise matrix operation");

        return ret;
    }

    // matrix <op> smeared scalar
    if (left.isMatrix()) {
        assert(Util::isGlaScalar(right.getValue()));

        return createSmearedMatrixOp(llvmOpcode, left.getMatrix(), right.getValue(), false);
    }

    // smeared scalar <op> matrix
    if (right.isMatrix()) {
        assert(Util::isGlaScalar(left.getValue()));

        return createSmearedMatrixOp(llvmOpcode, right.getMatrix(), left.getValue(), true);
    }

    assert(! "nonsensical matrix operation");

    return ret;
}

Builder::SuperValue Builder::createMatrixMultiply(Builder::SuperValue left, Builder::SuperValue right)
{
    Builder::SuperValue ret;

    // outer product
    if (left.isValue() && right.isValue()) {
        assert(Util::getComponentCount(left) == Util::getComponentCount(right));
        UnsupportedFunctionality("outer product");

        return ret;
    }

    assert(left.isMatrix() || right.isMatrix());

    // matrix times matrix
    if (left.isMatrix() && right.isMatrix()) {
        assert(left.getMatrix()->getNumRows()    == right.getMatrix()->getNumColumns());
        assert(left.getMatrix()->getNumColumns() == right.getMatrix()->getNumRows());

        return createMatrixTimesMatrix(left.getMatrix(), right.getMatrix());
    }

    // matrix times vector
    if (left.isMatrix() && Util::isVector(right.getValue()->getType())) {
        assert(left.getMatrix()->getNumColumns() == Util::getComponentCount(right.getValue()));

        return createMatrixTimesVector(left.getMatrix(), right.getValue());
    }

    // vector times matrix
    if (Util::isVector(left.getValue()->getType()) && right.isMatrix()) {
        assert(right.getMatrix()->getNumRows() == Util::getComponentCount(left.getValue()));

        return createVectorTimesMatrix(left.getValue(), right.getMatrix());
    }

    // matrix times scalar
    if (left.isMatrix() && Util::isGlaScalar(right.getValue()))
        return createSmearedMatrixOp(llvm::Instruction::FMul, left.getMatrix(), right.getValue(), true);

    // scalar times matrix
    if (Util::isGlaScalar(left.getValue()) && right.isMatrix())
        return createSmearedMatrixOp(llvm::Instruction::FMul, right.getMatrix(), left.getValue(), false);

    assert(! "nonsensical matrix multiply");

    return ret;
}

Builder::SuperValue Builder::createMatrixCompare(SuperValue left, SuperValue right, bool allEqual)
{
    assert(left.isMatrix() && right.isMatrix());
    assert(left.getMatrix()->getNumColumns() == right.getMatrix()->getNumColumns());
    assert(left.getMatrix()->getNumRows() == right.getMatrix()->getNumRows());

    llvm::Value* value1 =  left.getMatrix()->getMatrixValue();
    llvm::Value* value2 = right.getMatrix()->getMatrixValue();

    // Get a boolean to accumulate the results in
    llvm::Value* result = builder.CreateAlloca(llvm::IntegerType::get(builder.getContext(), 1), 0, "__Matrix-Compare");
    result = builder.CreateLoad(result, "__Matrix-Compare");
    llvm::Function* any;
    llvm::Function* all;

    for (int c = 0; c < left.getMatrix()->getNumColumns(); ++c) {
        // Get intermediate comparison values
        llvm::Value* column1 = builder.CreateExtractValue(value1, c, "__column");
        llvm::Value* column2 = builder.CreateExtractValue(value2, c, "__column");

        // compute intermediate comparison
        llvm::Value* interm;
        if (allEqual) {
            interm = builder.CreateFCmpOEQ(column1, column2);

            // first time, get our intrinsics to finish off the compares
            if (c == 0)
                all = Builder::getIntrinsic(llvm::Intrinsic::gla_all, interm->getType());

            interm = builder.CreateCall(all, interm);
        } else {
            interm = builder.CreateFCmpONE(column1, column2);

            // first time, get our intrinsics to finish off the compares
            if (c == 0)
                any = Builder::getIntrinsic(llvm::Intrinsic::gla_any, interm->getType());

            interm = builder.CreateCall(any, interm);
        }

        // Accumulate intermediate comparison
        if (c == 0) {
            result = interm;
        } else {
            if (allEqual)
                result = builder.CreateAnd(result, interm);
            else
                result = builder.CreateOr(result, interm);
        }
    }

    return result;
}

Builder::Matrix* Builder::createMatrixTranspose(Matrix* matrix)
{
    UnsupportedFunctionality("matrix transpose");

    return 0;
}

Builder::Matrix* Builder::createMatrixInverse(Matrix* matrix)
{
    UnsupportedFunctionality("matrix inverse");

    return 0;
}

Builder::Matrix* Builder::createMatrixDeterminant(Matrix* matrix)
{
    UnsupportedFunctionality("matrix determinant");

    return 0;
}

llvm::Value* Builder::createMatrixTimesVector(Matrix* matrix, llvm::Value* rvector)
{
    assert(matrix->getNumColumns() == Util::getComponentCount(rvector));

    UnsupportedFunctionality("matrix times vector");

    return 0;
}

llvm::Value* Builder::createVectorTimesMatrix(llvm::Value* lvector, Matrix* matrix)
{
    // Get the dot product intrinsic for these operands
    llvm::Function *dot = Builder::getIntrinsic(llvm::Intrinsic::gla_fDot, lvector->getType(), lvector->getType());

    // Allocate a vector to build the result in
    llvm::Value* result = builder.CreateAlloca(lvector->getType());
    result = builder.CreateLoad(result);

    // Compute the dot products for the result
    for (int c = 0; c < matrix->getNumColumns(); ++c) {
        llvm::Value* column = builder.CreateExtractValue(matrix->getMatrixValue(), c, "__column");
        llvm::Value* comp = builder.CreateCall2(dot, lvector, column, "__dot");
        result = builder.CreateInsertElement(result, comp, Util::makeUnsignedConstant(result->getContext(), c));
    }

    return result;
}

Builder::Matrix* Builder::createSmearedMatrixOp(llvm::Instruction::BinaryOps op, Matrix* matrix, llvm::Value* scalar, bool reverseOrder)
{
    // ?? better to smear the scalar to a column-like vector, and apply that vector multiple times
    // Allocate a matrix to build the result in
    llvm::Value* result = builder.CreateAlloca(matrix->getMatrixValue()->getType());
    result = builder.CreateLoad(result);

    // Compute per column vector
    for (int c = 0; c < matrix->getNumColumns(); ++c) {
        llvm::Value* column = builder.CreateExtractValue(matrix->getMatrixValue(), c, "__column");

        for (int r = 0; r < matrix->getNumRows(); ++r) {
            llvm::Value* element = builder.CreateExtractElement(column, Util::makeUnsignedConstant(result->getContext(), r), "__row");
            if (reverseOrder)
                element = builder.CreateBinOp(op, scalar, element);
            else
                element = builder.CreateBinOp(op, element, scalar);
            column = builder.CreateInsertElement(column, element, Util::makeUnsignedConstant(result->getContext(), r));
        }

        result = builder.CreateInsertValue(result, column, c);
    }

    //?? what about deleting all these matrices?
    return newMatrix(result);
}

Builder::Matrix* Builder::createMatrixTimesMatrix(Matrix* lmatrix, Matrix* rmatrix)
{
    assert(lmatrix->getNumColumns() == rmatrix->getNumRows() &&
           rmatrix->getNumColumns() == lmatrix->getNumRows());

    return 0;
}

Builder::Matrix* Builder::createOuterProduct(llvm::Value* lvector, llvm::Value* rvector)
{
    UnsupportedFunctionality("outer product");

    return 0;
}

// Get intrinsic declarations
// ?? LLVM issue: each time we lookup, LLVM makes a whole copy of all intrinsic name addresses
//    see Intrinsic::getName() in function.cpp
llvm::Function* Builder::getIntrinsic(llvm::Intrinsic::ID ID, const llvm::Type* type1)
{
    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID, &type1, 1);
}

llvm::Function* Builder::getIntrinsic(llvm::Intrinsic::ID ID, const llvm::Type* type1, const llvm::Type* type2)
{
    const llvm::Type* intrinsicTypes[] = {
        type1,
        type2 };

    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID, intrinsicTypes, 2);
}

llvm::Function* Builder::getIntrinsic(llvm::Intrinsic::ID ID, const llvm::Type* type1, const llvm::Type* type2, const llvm::Type* type3)
{
    const llvm::Type* intrinsicTypes[] = {
        type1,
        type2,
        type3 };

    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID, intrinsicTypes, 3);
}

llvm::Function* Builder::getIntrinsic(llvm::Intrinsic::ID ID, const llvm::Type* type1, const llvm::Type* type2, const llvm::Type* type3, const llvm::Type* type4)
{
    const llvm::Type* intrinsicTypes[] = {
        type1,
        type2,
        type3,
        type4 };

    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID, intrinsicTypes, 4);
}

void Builder::promoteScalar(SuperValue& left, SuperValue& right)
{
    int direction = Util::getComponentCount(right) - Util::getComponentCount(left);

    if (direction > 0)
        left = gla::Builder::smearScalar(left, right->getType());
    else if (direction < 0)
        right = gla::Builder::smearScalar(right, left->getType());

    return;
}

llvm::Value* Builder::smearScalar(llvm::Value* scalar, const llvm::Type* vectorType)
{
    assert(gla::Util::isGlaScalar(scalar->getType()));

    // Use a swizzle to expand the scalar to a vector
    llvm::Intrinsic::ID intrinsicID;
    switch(scalar->getType()->getTypeID()) {
    case llvm::Type::IntegerTyID:   intrinsicID = llvm::Intrinsic::gla_swizzle;     break;
    case llvm::Type::FloatTyID:     intrinsicID = llvm::Intrinsic::gla_fSwizzle;    break;
    default:
        gla::UnsupportedFunctionality("smear type");
    }

    llvm::Function *intrinsicName = getIntrinsic(intrinsicID, vectorType, scalar->getType());

    return builder.CreateCall2(intrinsicName, scalar, gla::Util::makeIntConstant(builder.getContext(), 0));
}

}; // end gla namespace
