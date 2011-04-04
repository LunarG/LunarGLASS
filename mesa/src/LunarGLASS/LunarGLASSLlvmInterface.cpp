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
//
//===----------------------------------------------------------------------===//

#include "Exceptions.h"
#include "LunarGLASSLlvmInterface.h"

namespace gla {

//
// Builder::Matrix definitions
//

Builder::Matrix::Matrix(int c, int r, llvm::Value* vectors[]) : numColumns(c), numRows(r)
{
    for (int column = 0; column < numColumns; ++column)
        columns[column] = vectors[column];
}

Builder::Matrix::Matrix(int c, int r, Matrix* oldMatrix) : numColumns(c), numRows(r)
{
    UnsupportedFunctionality("construction of matrix from matrix");
}

Builder::SuperValue Builder::createMatrixOp(llvm::IRBuilder<>& builder, unsigned llvmOpcode, Builder::SuperValue left, Builder::SuperValue right)
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

        return createSmearedMatrixOp(builder, llvmOpcode, left.getMatrix(), right.getValue());
    }

    // smeared scalar <op> matrix
    if (right.isMatrix()) {
        assert(Util::isGlaScalar(left.getValue()));

        return createSmearedMatrixOp(builder, llvmOpcode, left.getValue(), right.getMatrix());
    }

    assert(! "nonsensical matrix operation");

    return ret;
}

Builder::SuperValue Builder::createMatrixMultiply(llvm::IRBuilder<>& builder, Builder::SuperValue left, Builder::SuperValue right)
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

        createMatrixTimesMatrix(builder, left.getMatrix(), right.getMatrix());

        return ret;
    }

    // matrix times vector
    if (left.isMatrix() && Util::isVector(right.getValue()->getType())) {
        assert(left.getMatrix()->getNumColumns() == Util::getComponentCount(right.getValue()));

        createMatrixTimesVector(builder, left.getMatrix(), right.getValue());

        return ret;
    }

    // vector times matrix
    if (Util::isVector(left.getValue()->getType()) && right.isMatrix()) {
        assert(right.getMatrix()->getNumRows() == Util::getComponentCount(left.getValue()));

        createVectorTimesMatrix(builder, left.getValue(), right.getMatrix());

        return ret;
    }

    // matrix times scalar
    if (left.isMatrix() && Util::isGlaScalar(right.getValue()))
        return createSmearedMatrixOp(builder, llvm::Instruction::FMul, left.getMatrix(), right.getValue());

    // scalar times matrix
    if (Util::isGlaScalar(left.getValue()) && right.isMatrix())
        return createSmearedMatrixOp(builder, llvm::Instruction::FMul, left.getValue(), right.getMatrix());

    assert(! "nonsensical matrix multiply");

    return ret;
}

Builder::Matrix* Builder::createMatrixTranspose(llvm::IRBuilder<>&, Matrix* matrix)
{
    UnsupportedFunctionality("matrix transpose");

    return 0;
}

Builder::Matrix* Builder::createMatrixInverse(llvm::IRBuilder<>&, Matrix* matrix)
{
    UnsupportedFunctionality("matrix inverse");

    return 0;
}

Builder::Matrix* Builder::createMatrixDeterminant(llvm::IRBuilder<>&, Matrix* matrix)
{
    UnsupportedFunctionality("matrix determinant");

    return 0;
}

llvm::Value* Builder::createMatrixTimesVector(llvm::IRBuilder<>&, Matrix* matrix, llvm::Value* rvector)
{
    assert(matrix->getNumColumns() == Util::getComponentCount(rvector));

    return 0;
}

llvm::Value* Builder::createVectorTimesMatrix(llvm::IRBuilder<>&, llvm::Value* lvector, Matrix* matrix)
{
    assert(matrix->getNumRows() == Util::getComponentCount(lvector));

    return 0;
}

llvm::Value* Builder::createSmearedMatrixOp(llvm::IRBuilder<>&, unsigned llvmOpcode, Matrix*, llvm::Value*)
{
    switch (llvmOpcode) {
    case llvm::Instruction::FAdd: break;
    case llvm::Instruction::FSub: break;
    case llvm::Instruction::FMul: break;
    case llvm::Instruction::FDiv: break;
    default:
        UnsupportedFunctionality("matrix operation", llvmOpcode);
    }

    return 0;
}

llvm::Value* Builder::createSmearedMatrixOp  (llvm::IRBuilder<>&, unsigned llvmOpcode, llvm::Value*, Matrix*)
{
    switch (llvmOpcode) {
    case llvm::Instruction::FAdd: break;
    case llvm::Instruction::FSub: break;
    case llvm::Instruction::FMul: break;
    case llvm::Instruction::FDiv: break;
    default:
        UnsupportedFunctionality("matrix operation", llvmOpcode);
    }

    return 0;
}

Builder::Matrix* Builder::createMatrixTimesMatrix(llvm::IRBuilder<>&, Matrix* lmatrix, Matrix* rmatrix)
{
    assert(lmatrix->getNumColumns() == rmatrix->getNumRows() &&
           rmatrix->getNumColumns() == lmatrix->getNumRows());

    return 0;
}

Builder::Matrix* Builder::createOuterProduct(llvm::IRBuilder<>&, llvm::Value* lvector, llvm::Value* rvector)
{
    UnsupportedFunctionality("outer product");

    return 0;
}

//
// Util definitions
//

int Util::getConstantInt(const llvm::Value* value)
{
    const llvm::Constant* constant = llvm::dyn_cast<llvm::Constant>(value);
    assert(constant);
    const llvm::ConstantInt *constantInt = llvm::dyn_cast<llvm::ConstantInt>(constant);
    assert(constantInt);
    return constantInt->getValue().getSExtValue();
}

float Util::GetConstantFloat(const llvm::Value* value)
{
    const llvm::Constant* constant = llvm::dyn_cast<llvm::Constant>(value);
    assert(constant);
    const llvm::ConstantFP *constantFP = llvm::dyn_cast<llvm::ConstantFP>(constant);
    assert(constantFP);
    return constantFP->getValueAPF().convertToFloat();
}

int Util::getComponentCount(const llvm::Type* type)
{
    const llvm::VectorType *vectorType = llvm::dyn_cast<llvm::VectorType>(type);

    if (vectorType)
        return vectorType->getNumElements();
    else
        return 1;
}

int Util::getComponentCount(const llvm::Value* value)
{
    const llvm::Type* type = value->getType();

    return Util::getComponentCount(type);
}

bool Util::isConsecutiveSwizzle(int glaSwizzle, int width)
{
    for (int i = 0; i < width; ++i) {
        if (((glaSwizzle >> i*2) & 0x3) != i)
            return false;
    }

    return true;
}

bool Util::isGlaBoolean(const llvm::Type* type)
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

bool Util::hasAllSet(const llvm::Value* value)
{
    if (! llvm::isa<llvm::Constant>(value))
        return false;

    if (isGlaScalar(value->getType())) {
        return Util::getConstantInt(value) == -1;
    } else {
        const llvm::ConstantVector* vector = llvm::dyn_cast<llvm::ConstantVector>(value);
        assert(vector);

        for (int op = 0; op < vector->getNumOperands(); ++op) {
            if (Util::getConstantInt(vector->getOperand(op)) != -1)
                return false;
        }

        return true;
    }
}

}; // end gla namespace
