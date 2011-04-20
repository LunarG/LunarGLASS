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

// LLVM includes
#include "llvm/BasicBlock.h"
#include "llvm/Instructions.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/CFG.h"

namespace gla {

//
// Builder::Matrix definitions
//

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

Builder::SuperValue Builder::createMatrixOp(llvm::IRBuilder<>& builder, llvm::Instruction::BinaryOps llvmOpcode, Builder::SuperValue left, Builder::SuperValue right)
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

        return createSmearedMatrixOp(builder, llvmOpcode, left.getMatrix(), right.getValue(), false);
    }

    // smeared scalar <op> matrix
    if (right.isMatrix()) {
        assert(Util::isGlaScalar(left.getValue()));

        return createSmearedMatrixOp(builder, llvmOpcode, right.getMatrix(), left.getValue(), true);
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

        return createMatrixTimesMatrix(builder, left.getMatrix(), right.getMatrix());
    }

    // matrix times vector
    if (left.isMatrix() && Util::isVector(right.getValue()->getType())) {
        assert(left.getMatrix()->getNumColumns() == Util::getComponentCount(right.getValue()));

        return createMatrixTimesVector(builder, left.getMatrix(), right.getValue());
    }

    // vector times matrix
    if (Util::isVector(left.getValue()->getType()) && right.isMatrix()) {
        assert(right.getMatrix()->getNumRows() == Util::getComponentCount(left.getValue()));

        return createVectorTimesMatrix(builder, left.getValue(), right.getMatrix());
    }

    // matrix times scalar
    if (left.isMatrix() && Util::isGlaScalar(right.getValue()))
        return createSmearedMatrixOp(builder, llvm::Instruction::FMul, left.getMatrix(), right.getValue(), true);

    // scalar times matrix
    if (Util::isGlaScalar(left.getValue()) && right.isMatrix())
        return createSmearedMatrixOp(builder, llvm::Instruction::FMul, right.getMatrix(), left.getValue(), false);

    assert(! "nonsensical matrix multiply");

    return ret;
}

Builder::SuperValue Builder::createMatrixCompare(llvm::IRBuilder<>& builder, SuperValue left, SuperValue right, bool allEqual)
{
    assert(left.isMatrix() && right.isMatrix());
    assert(left.getMatrix()->getNumColumns() == right.getMatrix()->getNumColumns());
    assert(left.getMatrix()->getNumRows() == right.getMatrix()->getNumRows());

    llvm::Value* value1 =  left.getMatrix()->getMatrixValue();
    llvm::Value* value2 = right.getMatrix()->getMatrixValue();

    llvm::Module* module = builder.GetInsertBlock()->getParent()->getParent();

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
                all = Util::getIntrinsic(module, llvm::Intrinsic::gla_all, interm->getType());

            interm = builder.CreateCall(all, interm);
        } else {
            interm = builder.CreateFCmpONE(column1, column2);

            // first time, get our intrinsics to finish off the compares
            if (c == 0)
                any = Util::getIntrinsic(module, llvm::Intrinsic::gla_any, interm->getType());

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

llvm::Value* Builder::createMatrixTimesVector(llvm::IRBuilder<>& builder, Matrix* matrix, llvm::Value* rvector)
{
    assert(matrix->getNumColumns() == Util::getComponentCount(rvector));

    UnsupportedFunctionality("matrix times vector");

    return 0;
}

llvm::Value* Builder::createVectorTimesMatrix(llvm::IRBuilder<>& builder, llvm::Value* lvector, Matrix* matrix)
{
    // Get the dot product intrinsic for these operands
    llvm::Function *dot = Util::getIntrinsic(builder.GetInsertBlock()->getParent()->getParent(),
                                             llvm::Intrinsic::gla_fDot, lvector->getType(), lvector->getType());

    // Allocate a vector to build the result in
    llvm::Value* result = builder.CreateAlloca(lvector->getType());
    result = builder.CreateLoad(result);

    // Compute the dot products for the result
    for (int c = 0; c < matrix->getNumColumns(); ++c) {
        llvm::Value* column = builder.CreateExtractValue(matrix->getMatrixValue(), c, "__column");
        llvm::Value* comp = builder.CreateCall2(dot, lvector, column, "__dot");
        result = builder.CreateInsertElement(result, comp, Util::makeUnsignedIntConstant(result->getContext(), c));
    }

    return result;
}

Builder::Matrix* Builder::createSmearedMatrixOp(llvm::IRBuilder<>& builder, llvm::Instruction::BinaryOps op, Matrix* matrix, llvm::Value* scalar, bool reverseOrder)
{
    // ?? better to smear the scalar to a column-like vector, and apply that vector multiple times
    // Allocate a matrix to build the result in
    llvm::Value* result = builder.CreateAlloca(matrix->getMatrixValue()->getType());
    result = builder.CreateLoad(result);

    // Compute per column vector
    for (int c = 0; c < matrix->getNumColumns(); ++c) {
        llvm::Value* column = builder.CreateExtractValue(matrix->getMatrixValue(), c, "__column");

        for (int r = 0; r < matrix->getNumRows(); ++r) {
            llvm::Value* element = builder.CreateExtractElement(column, Util::makeUnsignedIntConstant(result->getContext(), r), "__row");
            if (reverseOrder)
                element = builder.CreateBinOp(op, scalar, element);
            else
                element = builder.CreateBinOp(op, element, scalar);
            column = builder.CreateInsertElement(column, element, Util::makeUnsignedIntConstant(result->getContext(), r));
        }

        result = builder.CreateInsertValue(result, column, c);
    }

    //?? what about deleting all these matrices?
    return new Matrix(result);
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

// true if provided basic block is one of the (possibly many) latches in the provided loop
bool Util::isLatch(const llvm::BasicBlock* bb, llvm::Loop* loop)
{
    if (!loop)
        return false;

    llvm::BasicBlock* header = loop->getHeader();
    for (llvm::succ_const_iterator sI = succ_begin(bb), sE = succ_end(bb); sI != sE; ++sI) {
        if (*sI == header)
            return true;
    }

    return false;
}

// Return the number of latches in a loop
int Util::getNumLatches(llvm::Loop* loop)
{
    if (!loop)
        return 0;

    int count = 0;
    for (llvm::Loop::block_iterator bbI = loop->block_begin(), bbE = loop->block_end(); bbI != bbE; ++bbI) {
        if (isLatch(*bbI, loop)) {
            count++;
        }
    }

    return count;
}

// Return the single merge point of the given conditional basic block. Returns
// null if there is no merge point, or if there are more than 1 merge
// points. Note that the presense of backedges or exitedges in the then and else
// branchs' subgraphs may cause there to be multiple potential merge points.
llvm::BasicBlock* Util::getSingleMergePoint(const llvm::BasicBlock* condBB, llvm::DominanceFrontier& domFront)
{
    const llvm::BranchInst* branchInst = llvm::dyn_cast<llvm::BranchInst>(condBB->getTerminator());
    assert(branchInst && branchInst->getNumSuccessors() == 2 && "writeMergePoints called with improper terminator");

    llvm::BasicBlock* left  = branchInst->getSuccessor(0);
    llvm::BasicBlock* right = branchInst->getSuccessor(1);

    llvm::DominanceFrontier::DomSetType leftDomFront  = (*domFront.find(left)).second;
    llvm::DominanceFrontier::DomSetType rightDomFront = (*domFront.find(right)).second;

    bool isLeft  = rightDomFront.count(left);
    bool isRight = leftDomFront.count(right);
    assert(!(isLeft && isRight) && "Noncanonical control flow: cross edges");

    if (isLeft)
        return left;
    if (isRight)
        return right;

    std::vector<llvm::BasicBlock*> merges;
    merges.resize(leftDomFront.size() + rightDomFront.size());

    std::vector<llvm::BasicBlock*>::iterator it = std::set_intersection(leftDomFront.begin(), leftDomFront.end(), rightDomFront.begin(), rightDomFront.end(), merges.begin());

    // If we got no merge points, or if we got multiple merge points, return null
    if (it == merges.begin() || it != ++merges.begin())
        return NULL;

    return merges[0];
}

llvm::Function* Util::getIntrinsic(llvm::Module* module, llvm::Intrinsic::ID ID, const llvm::Type* type1)
{
    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID, &type1, 1);
}

llvm::Function* Util::getIntrinsic(llvm::Module* module, llvm::Intrinsic::ID ID, const llvm::Type* type1, const llvm::Type* type2)
{
    const llvm::Type* intrinsicTypes[] = {
        type1,
        type2 };

    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID, intrinsicTypes, 2);
}

llvm::Function* Util::getIntrinsic(llvm::Module* module, llvm::Intrinsic::ID ID, const llvm::Type* type1, const llvm::Type* type2, const llvm::Type* type3)
{
    const llvm::Type* intrinsicTypes[] = {
        type1,
        type2,
        type3 };

    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID, intrinsicTypes, 3);
}

llvm::Function* Util::getIntrinsic(llvm::Module* module, llvm::Intrinsic::ID ID, const llvm::Type* type1, const llvm::Type* type2, const llvm::Type* type3, const llvm::Type* type4)
{
    const llvm::Type* intrinsicTypes[] = {
        type1,
        type2,
        type3,
        type4 };

    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID, intrinsicTypes, 4);
}

}; // end gla namespace
