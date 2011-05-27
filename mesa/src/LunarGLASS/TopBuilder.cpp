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
// Author: Michael Ilseman, LunarG
//
//===----------------------------------------------------------------------===//

#include "Exceptions.h"
#include "Util.h"
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

Builder::Builder(llvm::IRBuilder<>& b, llvm::Module* m) :
    builder(b),
    module(m),
    context(builder.getContext()),
    mainFunction(0),
    stageEpilogue(0),
    stageExit(0)
{
}

Builder::~Builder()
{
    for (std::vector<Matrix*>::iterator i = matrixList.begin(); i != matrixList.end(); ++i)
        delete *i;
}

llvm::BasicBlock* Builder::makeMain()
{
    assert(! mainFunction);

    llvm::BasicBlock* entry;
    std::vector<const llvm::Type*> params;

    stageEpilogue = llvm::BasicBlock::Create(context, "stage-epilogue");
    stageExit    = llvm::BasicBlock::Create(context, "stage-exit");

    mainFunction = makeFunctionEntry(gla::GetVoidType(context), "main", params, entry);

    return entry;
}

void Builder::closeMain()
{
    // Add our instructions to stageEpilogue, and stageExit
    builder.SetInsertPoint(stageEpilogue);
    copyOutPipeline();
    builder.CreateBr(stageExit);

    builder.SetInsertPoint(stageExit);
    builder.CreateRet(0);

    mainFunction->getBasicBlockList().push_back(stageEpilogue);
    mainFunction->getBasicBlockList().push_back(stageExit);
}


void Builder::makeReturn(llvm::Value* retVal, bool isMain)
{
    if (! isMain || retVal)
        gla::UnsupportedFunctionality("return from non-main functions");

    builder.CreateBr(stageEpilogue);
}

void Builder::makeDiscard(bool isMain)
{
    if (! isMain)
        gla::UnsupportedFunctionality("discard from non-main functions");

    builder.CreateBr(stageExit);
}

llvm::Function* Builder::makeFunctionEntry(const llvm::Type* type, const char* name, std::vector<const llvm::Type*> paramTypes, llvm::BasicBlock*& entry)
{
    llvm::FunctionType *functionType = llvm::FunctionType::get(type, paramTypes, false);
    llvm::Function *function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, module);

    // For shaders, we want everything passed in registers
    function->setCallingConv(llvm::CallingConv::Fast);

    entry = llvm::BasicBlock::Create(context, "entry", function);

    return function;
}

llvm::Constant* Builder::getConstant(std::vector<llvm::Constant*>& constants)
{
    if (constants.size() == 1)
        return constants[0];

    return llvm::ConstantVector::get(constants);
}

Builder::SuperValue Builder::createVariable(EStorageQualifier storageQualifier, int storageInstance,
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

Builder::SuperValue Builder::createStore(SuperValue rValue, SuperValue lValue)
{
    llvm::Value* llvmRValue = rValue;
    llvm::Value* llvmLValue = lValue;

    if (lValue.isMatrix()) {
        assert(rValue.isMatrix());
        assert(lValue.getMatrix()->getNumColumns() == rValue.getMatrix()->getNumColumns() &&
               lValue.getMatrix()->getNumRows()    == rValue.getMatrix()->getNumRows());
    }

    // Retroactively change the name of the last-value temp to the name of the
    // l-value, to help debuggability, if it's just an llvm temp name.
    if (llvmRValue->getNameStr().length() < 2 || (llvmRValue->getNameStr()[1] >= '0' && llvmRValue->getNameStr()[1] <= '9'))
        llvmRValue->setName(llvmLValue->getName());

    builder.CreateStore(llvmRValue, llvmLValue);

    return lValue;
}

Builder::SuperValue Builder::createLoad(SuperValue lValue)
{
    if (lValue.isMatrix()) {
        llvm::Value* newValue = builder.CreateLoad(lValue.getMatrix()->getValue(), "__matrix");
        gla::Builder::Matrix* loadedMatrix = new gla::Builder::Matrix(newValue);
        return gla::Builder::SuperValue(loadedMatrix);
    } else
        return builder.CreateLoad(lValue);
}

Builder::SuperValue Builder::createGEP(SuperValue gepValue, std::vector<llvm::Value*> gepIndexChain)
{
    if (gepValue.isMatrix()) {
        llvm::Value* newValue = builder.CreateGEP(gepValue.getMatrix()->getValue(),
                                gepIndexChain.begin(),
                                gepIndexChain.end());

        if (gepIndexChain.size() == 1) {
            gla::Builder::Matrix* gepMatrix = new gla::Builder::Matrix(newValue);
            return gla::Builder::SuperValue(gepMatrix);
        } else {
            return newValue;
        }

    } else
         return builder.CreateGEP(gepValue, gepIndexChain.begin(), gepIndexChain.end());
}

Builder::SuperValue Builder::createInsertValue(SuperValue target, SuperValue source, unsigned* indices, int indexCount)
{
    if (target.isMatrix()) {
        llvm::Value* newValue = builder.CreateInsertValue(target.getMatrix()->getValue(), source, indices, indices + indexCount);
        gla::Builder::Matrix* insertValMatrix = new gla::Builder::Matrix(newValue);
        return gla::Builder::SuperValue(insertValMatrix);
    } else
        return builder.CreateInsertValue(target, source, indices, indices + indexCount);
}

void Builder::copyOutPipeline()
{
    llvm::Intrinsic::ID intrinsicID;

    std::list<llvm::Value*>::iterator outIter;

    // Call writeData intrinsic on our outs
    for ( outIter = copyOuts.begin(); outIter != copyOuts.end(); outIter++ ) {
        llvm::Value* loadVal = builder.CreateLoad(*outIter);

        switch(GetBasicType(loadVal)) {
        case llvm::Type::IntegerTyID:   intrinsicID = llvm::Intrinsic::gla_writeData;   break;
        case llvm::Type::FloatTyID:     intrinsicID = llvm::Intrinsic::gla_fWriteData;  break;
        }

        llvm::Function *intrinsicName = getIntrinsic(intrinsicID, loadVal->getType());

        builder.CreateCall2(intrinsicName, MakeIntConstant(context, 0), loadVal);
    }
}

llvm::Value* Builder::readPipeline(const llvm::Type* type, std::string& name, int slot, EInterpolationMode mode, float offsetX, float offsetY)
{
    llvm::Constant *slotConstant = MakeUnsignedConstant(context, slot);

    // This correction is necessary for some front ends, which might allow
    // "interpolated" integers or Booleans.
    if (GetBasicType(type) != llvm::Type::FloatTyID)
        mode = EIMNone;

    llvm::Function *intrinsic;
    if (mode != EIMNone) {
        llvm::Constant *modeConstant = MakeUnsignedConstant(context, mode);

        if (offsetX != 0.0 || offsetY != 0.0) {
            std::vector<llvm::Constant*> offsets;
            offsets.push_back(MakeFloatConstant(context, offsetX));
            offsets.push_back(MakeFloatConstant(context, offsetY));
            llvm::Constant* offset = getConstant(offsets);
            intrinsic = getIntrinsic(llvm::Intrinsic::gla_fReadInterpolantOffset, type, offset->getType());
            return builder.CreateCall3(intrinsic, slotConstant, modeConstant, offset, name);
        } else {
            intrinsic = getIntrinsic(llvm::Intrinsic::gla_fReadInterpolant, type);
            return builder.CreateCall2(intrinsic, slotConstant, modeConstant, name);
        }
    } else {
        switch (GetBasicType(type)) {
        case llvm::Type::IntegerTyID:   intrinsic = getIntrinsic(llvm::Intrinsic::gla_readData, type); break;
        case llvm::Type::FloatTyID:     intrinsic = getIntrinsic(llvm::Intrinsic::gla_fReadData, type); break;
        }

        return builder.CreateCall(intrinsic, slotConstant, name);
    }
}

llvm::Value* Builder::createSwizzle(llvm::Value* source, int swizzleMask, const llvm::Type* finalType)
{
    const int numComponents = gla::GetComponentCount(finalType);

    // If we are dealing with a scalar, just put it in a register and return
    if (numComponents == 1)
        return builder.CreateExtractElement(source, gla::MakeIntConstant(context, gla::GetSwizzle(swizzleMask, 0)));

    // Else we are dealing with a vector

    // We start out with an undef to insert into
    llvm::Value* target = llvm::UndefValue::get(finalType);

    for (int i = 0; i < numComponents; ++i) {

        // If we're constructing a vector from a scalar, then just
        // make inserts. Otherwise make insert/extract pairs
        if (IsScalar(source)) {
            target = builder.CreateInsertElement(target, source, gla::MakeIntConstant(context, i));
        } else {
            // Extract an element to a scalar, then immediately insert to our target
            llvm::Value* extractInst = builder.CreateExtractElement(source, gla::MakeIntConstant(context, gla::GetSwizzle(swizzleMask, i)));
            target = builder.CreateInsertElement(target, extractInst, gla::MakeIntConstant(context, i));
        }
    }

    return target;
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

const llvm::Type* Builder::Matrix::getType(const llvm::Type* elementType, int numColumns, int numRows)
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

    const llvm::Type** type = &typeCache[numColumns-minSize][numRows-minSize];
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

        return createMatrixOp(llvmOpcode, left.getMatrix(), right.getMatrix());
    }

    // matrix <op> smeared scalar
    if (left.isMatrix()) {
        assert(IsScalar(right.getValue()));

        return createSmearedMatrixOp(llvmOpcode, left.getMatrix(), right.getValue(), false);
    }

    // smeared scalar <op> matrix
    if (right.isMatrix()) {
        assert(IsScalar(left.getValue()));

        return createSmearedMatrixOp(llvmOpcode, right.getMatrix(), left.getValue(), true);
    }

    assert(! "nonsensical matrix operation");

    return ret;
}

Builder::SuperValue Builder::createMatrixMultiply(Builder::SuperValue left, Builder::SuperValue right)
{
    Builder::SuperValue ret;

    // outer product
    if (left.isValue() && right.isValue())
        return createOuterProduct(left.getValue(), right.getValue());

    assert(left.isMatrix() || right.isMatrix());

    // matrix times matrix
    if (left.isMatrix() && right.isMatrix()) {
        assert(left.getMatrix()->getNumRows()    == right.getMatrix()->getNumColumns());
        assert(left.getMatrix()->getNumColumns() == right.getMatrix()->getNumRows());

        return createMatrixTimesMatrix(left.getMatrix(), right.getMatrix());
    }

    // matrix times vector
    if (left.isMatrix() && IsVector(right.getValue()->getType())) {
        assert(left.getMatrix()->getNumColumns() == GetComponentCount(right.getValue()));

        return createMatrixTimesVector(left.getMatrix(), right.getValue());
    }

    // vector times matrix
    if (IsVector(left.getValue()->getType()) && right.isMatrix()) {
        assert(right.getMatrix()->getNumRows() == GetComponentCount(left.getValue()));

        return createVectorTimesMatrix(left.getValue(), right.getMatrix());
    }

    // matrix times scalar
    if (left.isMatrix() && IsScalar(right.getValue()))
        return createSmearedMatrixOp(llvm::Instruction::FMul, left.getMatrix(), right.getValue(), true);

    // scalar times matrix
    if (IsScalar(left.getValue()) && right.isMatrix())
        return createSmearedMatrixOp(llvm::Instruction::FMul, right.getMatrix(), left.getValue(), false);

    assert(! "nonsensical matrix multiply");

    return ret;
}

Builder::SuperValue Builder::createMatrixCompare(SuperValue left, SuperValue right, bool allEqual)
{
    assert(left.isMatrix() && right.isMatrix());
    assert(left.getMatrix()->getNumColumns() == right.getMatrix()->getNumColumns());
    assert(left.getMatrix()->getNumRows() == right.getMatrix()->getNumRows());

    llvm::Value* value1 =  left.getMatrix()->getValue();
    llvm::Value* value2 = right.getMatrix()->getValue();

    // Get a boolean to accumulate the results in
    llvm::Value* result = builder.CreateAlloca(llvm::IntegerType::get(context, 1), 0, "__Matrix-Compare");
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
    assert(matrix->getNumColumns() == GetComponentCount(rvector));

    // Allocate a vector to build the result in
    llvm::Value* result = builder.CreateAlloca(llvm::VectorType::get(rvector->getType()->getContainedType(0), matrix->getNumRows()));
    result = builder.CreateLoad(result);

    // Cache the components of the vector; they'll be revisited multiple times
    llvm::Value* components[4];
    for (int comp = 0; comp < GetComponentCount(rvector); ++comp)
        components[comp] = builder.CreateExtractElement(rvector,  MakeUnsignedConstant(context, comp), "__component");

    // Go row by row, manually forming the cross-column "dot product"
    for (int row = 0; row < matrix->getNumRows(); ++row) {
        llvm::Value* dotProduct;
        for (int col = 0; col < matrix->getNumColumns(); ++col) {
            llvm::Value* column = builder.CreateExtractValue(matrix->getValue(), col, "__column");
            llvm::Value* element = builder.CreateExtractElement(column, MakeUnsignedConstant(context, row), "__element");
            llvm::Value* product = builder.CreateFMul(element, components[col], "__product");
            if (col == 0)
                dotProduct = product;
            else
                dotProduct = builder.CreateFAdd(dotProduct, product, "__dotProduct");
        }
        result = builder.CreateInsertElement(result, dotProduct, MakeUnsignedConstant(context, row));
    }

    return result;
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
        llvm::Value* column = builder.CreateExtractValue(matrix->getValue(), c, "__column");
        llvm::Value* comp = builder.CreateCall2(dot, lvector, column, "__dot");
        result = builder.CreateInsertElement(result, comp, MakeUnsignedConstant(context, c));
    }

    return result;
}

Builder::Matrix* Builder::createMatrixOp(llvm::Instruction::BinaryOps op, Matrix* left, Matrix* right)
{
    // Allocate a matrix to hold the result in
    llvm::Value* result = builder.CreateAlloca(left->getValue()->getType());
    result = builder.CreateLoad(result);

    // Compute the component-wise operation per column vector
    for (int c = 0; c < left->getNumColumns(); ++c) {
        llvm::Value*  leftColumn = builder.CreateExtractValue( left->getValue(), c,  "__leftColumn");
        llvm::Value* rightColumn = builder.CreateExtractValue(right->getValue(), c, "__rightColumn");
        llvm::Value* column = builder.CreateBinOp(op, leftColumn, rightColumn, "__column");
        result = builder.CreateInsertValue(result, column, c);
    }

    return newMatrix(result);
}

Builder::Matrix* Builder::createSmearedMatrixOp(llvm::Instruction::BinaryOps op, Matrix* matrix, llvm::Value* scalar, bool reverseOrder)
{
    // ?? better to smear the scalar to a column-like vector, and apply that vector multiple times
    // Allocate a matrix to build the result in
    llvm::Value* result = builder.CreateAlloca(matrix->getValue()->getType());
    result = builder.CreateLoad(result);

    // Compute per column vector
    for (int c = 0; c < matrix->getNumColumns(); ++c) {
        llvm::Value* column = builder.CreateExtractValue(matrix->getValue(), c, "__column");

        for (int r = 0; r < matrix->getNumRows(); ++r) {
            llvm::Value* element = builder.CreateExtractElement(column, MakeUnsignedConstant(context, r), "__row");
            if (reverseOrder)
                element = builder.CreateBinOp(op, scalar, element);
            else
                element = builder.CreateBinOp(op, element, scalar);
            column = builder.CreateInsertElement(column, element, MakeUnsignedConstant(context, r));
        }

        result = builder.CreateInsertValue(result, column, c);
    }

    return newMatrix(result);
}

Builder::Matrix* Builder::createMatrixTimesMatrix(Matrix* left, Matrix* right)
{
    // Allocate a matrix to hold the result in
    int rows = left->getNumRows();
    int columns =  right->getNumColumns();
    llvm::Value* result = builder.CreateAlloca(Matrix::getType(left->getElementType(), columns, rows));
    result = builder.CreateLoad(result, "__resultMatrix");

    // Allocate a column for intermediate results
    llvm::Value* column = builder.CreateAlloca(llvm::VectorType::get(left->getElementType(), rows));
    column = builder.CreateLoad(column, "__tempColumn");

    for (int col = 0; col < columns; ++col) {
        llvm::Value* rightColumn = builder.CreateExtractValue(right->getValue(), col, "__rightColumn");
        for (int row = 0; row < rows; ++row) {
            llvm::Value* dotProduct;

            for (int dotRow = 0; dotRow < right->getNumRows(); ++dotRow) {
                llvm::Value* leftColumn = builder.CreateExtractValue(left->getValue(), dotRow,  "__leftColumn");
                llvm::Value* leftComp = builder.CreateExtractElement(leftColumn, MakeUnsignedConstant(context, row), "__leftComp");
                llvm::Value* rightComp = builder.CreateExtractElement(rightColumn, MakeUnsignedConstant(context, dotRow), "__rightComp");
                llvm::Value* product = builder.CreateFMul(leftComp, rightComp, "__product");
                if (dotRow == 0)
                    dotProduct = product;
                else
                    dotProduct = builder.CreateFAdd(dotProduct, product, "__dotProduct");
            }
            column = builder.CreateInsertElement(column, dotProduct, MakeUnsignedConstant(context, row), "__column");
        }

        result = builder.CreateInsertValue(result, column, col, "__resultMatrix");
    }

    return newMatrix(result);
}

Builder::Matrix* Builder::createOuterProduct(llvm::Value* left, llvm::Value* right)
{
    // Allocate a matrix to hold the result in
    int rows = GetComponentCount(left);
    int columns =  GetComponentCount(right);
    llvm::Value* result = builder.CreateAlloca(Matrix::getType(left->getType()->getContainedType(0), columns, rows));
    result = builder.CreateLoad(result);

    // Allocate a column for intermediate results
    llvm::Value* column = builder.CreateAlloca(left->getType());
    column = builder.CreateLoad(column);

    // Build it up column by column, element by element
    for (int col = 0; col < columns; ++col) {
        llvm::Value* rightComp = builder.CreateExtractElement(right, MakeUnsignedConstant(context, col), "__rightComp");
        for (int row = 0; row < rows; ++row) {
            llvm::Value*  leftComp = builder.CreateExtractElement( left, MakeUnsignedConstant(context, row),  "__leftComp");
            llvm::Value* element = builder.CreateFMul(leftComp, rightComp, "__element");
            column = builder.CreateInsertElement(column, element, MakeUnsignedConstant(context, row), "__column");
        }
        result = builder.CreateInsertValue(result, column, col, "__matrix");
    }

    return newMatrix(result);
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
    int direction = GetComponentCount(right) - GetComponentCount(left);

    if (direction > 0)
        left = gla::Builder::smearScalar(left, right->getType());
    else if (direction < 0)
        right = gla::Builder::smearScalar(right, left->getType());

    return;
}

llvm::Value* Builder::smearScalar(llvm::Value* scalar, const llvm::Type* vectorType)
{
    assert(gla::IsScalar(scalar->getType()));

    // Use a swizzle to expand the scalar to a vector
    llvm::Intrinsic::ID intrinsicID;
    switch(scalar->getType()->getTypeID()) {
    case llvm::Type::IntegerTyID:   intrinsicID = llvm::Intrinsic::gla_swizzle;     break;
    case llvm::Type::FloatTyID:     intrinsicID = llvm::Intrinsic::gla_fSwizzle;    break;
    default:
        gla::UnsupportedFunctionality("smear type");
    }

    llvm::Function *intrinsicName = getIntrinsic(intrinsicID, vectorType, scalar->getType());

    return builder.CreateCall2(intrinsicName, scalar, gla::MakeIntConstant(context, 0));
}

// Accept all parameters needed to create LunarGLASS texture intrinsics
// Select the correct intrinsic based on the inputs, and make the call
// TODO:  Expand this beyond current level of GLSL 1.2 functionality
llvm::Value* Builder::createTextureCall(const llvm::Type* resultType, gla::ESamplerType samplerType, gla::ETextureFlags texFlags, const TextureParameters& parameters)
{
    // Based on our texFlags, set the intrinsicID
    static const int maxTextureArgs = 40;
    llvm::Value* texArgs[maxTextureArgs] = {0};

    texArgs[GetTextureOpIndex(ETOSamplerType)] = MakeIntConstant(context, samplerType);
    texArgs[GetTextureOpIndex(ETOSamplerLoc)]  = parameters.ETPSampler;
    texArgs[GetTextureOpIndex(ETOFlag)]     = MakeUnsignedConstant(context, *(int*)&texFlags);
    texArgs[GetTextureOpIndex(ETOCoord)] = parameters.ETPCoords;
    int numArgs = 4;

    llvm::Intrinsic::ID intrinsicID = llvm::Intrinsic::gla_fTextureSample;
    if (texFlags.EBias || texFlags.ELod) {
        intrinsicID = llvm::Intrinsic::gla_fTextureSampleLod;
        texArgs[GetTextureOpIndex(ETOBias)] = parameters.ETPBiasLod;
        numArgs = 5;
    }

    llvm::Function* intrinsic = getIntrinsic(intrinsicID, resultType, parameters.ETPCoords->getType());
    assert(intrinsic);

    return builder.CreateCall(intrinsic, texArgs, texArgs + numArgs);
}

llvm::Value* Builder::createRecip(llvm::Value* operand)
{
    return builder.CreateFDiv(MakeFloatConstant(context, 1.0), operand);
}

llvm::Value* Builder::createCompare(llvm::Value* lhs, llvm::Value* rhs, bool equal, bool isFloat, bool isSigned)
{
    llvm::Value* result = 0;
    llvm::Intrinsic::ID intrinsicID = llvm::Intrinsic::gla_all;

    if (isFloat) {
        if (equal) {
            result = builder.CreateFCmpOEQ(lhs, rhs);
        } else {
            result = builder.CreateFCmpONE(lhs, rhs);
            intrinsicID = llvm::Intrinsic::gla_any;
        }
    } else {
        if (equal) {
            result = builder.CreateICmpEQ(lhs, rhs);
        } else {
            result = builder.CreateICmpNE(lhs, rhs);
            intrinsicID = llvm::Intrinsic::gla_any;
        }
    }

    if (llvm::dyn_cast<llvm::VectorType>(result->getType()))
        return createIntrinsicCall(intrinsicID, result);

    return result;
}

llvm::Value* Builder::createIntrinsicCall(llvm::Intrinsic::ID intrinsicID, SuperValue operand)
{
    llvm::Function* intrinsicName = 0;

    // Handle special return types here.  Things that don't have same result type as parameter
    switch (intrinsicID) {
    case llvm::Intrinsic::gla_fModF:
    case llvm::Intrinsic::gla_fIsNan:
    case llvm::Intrinsic::gla_fIsInf:
    case llvm::Intrinsic::gla_fFloatBitsToInt:
    case llvm::Intrinsic::gla_fIntBitsTofloat:
    case llvm::Intrinsic::gla_fFrexp:
    case llvm::Intrinsic::gla_fLdexp:
    case llvm::Intrinsic::gla_fPackUnorm2x16:
    case llvm::Intrinsic::gla_fPackUnorm4x8 :
    case llvm::Intrinsic::gla_fPackSnorm4x8 :
    case llvm::Intrinsic::gla_fUnpackUnorm2x16:
    case llvm::Intrinsic::gla_fUnpackUnorm4x8:
    case llvm::Intrinsic::gla_fUnpackSnorm4x8:
    case llvm::Intrinsic::gla_fPackDouble2x32:
    case llvm::Intrinsic::gla_fUnpackDouble2x32 :
        // TODO:  Hook these up
        gla::UnsupportedFunctionality("unary intrinsic", intrinsicID);
        break;
    case llvm::Intrinsic::gla_fLength:
    case llvm::Intrinsic::gla_any:
    case llvm::Intrinsic::gla_all:
        // fixed result type
        intrinsicName = getIntrinsic(intrinsicID, operand->getType());
        break;
    default:
        // Unary intrinsics that have operand and dest with same flexible type
        intrinsicName = getIntrinsic(intrinsicID,  operand->getType(), operand->getType());
    }

    assert(intrinsicName);

    return builder.CreateCall(intrinsicName, operand.getValue());
}

llvm::Value* Builder::createIntrinsicCall(llvm::Intrinsic::ID intrinsicID, SuperValue lhs, SuperValue rhs)
{
    llvm::Function* intrinsicName = 0;

    // Handle special return types here.  Things that don't have same result type as parameter
    switch (intrinsicID) {
    case llvm::Intrinsic::gla_fDistance:
        // TODO:  Hook this up
        gla::UnsupportedFunctionality("binary intrinsic", intrinsicID);
        break;
    case llvm::Intrinsic::gla_fDot:
        // fixed result type
        intrinsicName = getIntrinsic(intrinsicID, lhs->getType(), rhs->getType());
        break;
    default:
        // Binary intrinsics that have operand and dest with same flexible type
        intrinsicName = getIntrinsic(intrinsicID,  lhs->getType(), lhs->getType(), rhs->getType());
    }

    assert(intrinsicName);

    return builder.CreateCall2(intrinsicName, lhs, rhs);
}

llvm::Value* Builder::createIntrinsicCall(llvm::Intrinsic::ID intrinsicID, SuperValue operand0, SuperValue operand1, SuperValue operand2)
{

    // Use operand0 type as result type
    llvm::Function* intrinsicName =  getIntrinsic(intrinsicID, operand0->getType(), operand0->getType(), operand1->getType(), operand2->getType());

    assert(intrinsicName);

    return builder.CreateCall3(intrinsicName, operand0, operand1, operand2);
}

Builder::If::If(llvm::Value* condition, bool withElse, Builder* gb) : glaBuilder(gb)
{
    function = glaBuilder->builder.GetInsertBlock()->getParent();

    // make the blocks, but only put the then-block into the function,
    // the else-block and merge-block will be added later, in order, after
    // earlier code is emitted
    thenBB = llvm::BasicBlock::Create(glaBuilder->context, "then", function);
    elseBB = withElse ? llvm::BasicBlock::Create(glaBuilder->context, "else") : 0;
    mergeBB = llvm::BasicBlock::Create(glaBuilder->context, "ifmerge");

    // make the flow control split
    if (withElse)
        glaBuilder->builder.CreateCondBr(condition, thenBB, elseBB);
    else
        glaBuilder->builder.CreateCondBr(condition, thenBB, mergeBB);

    glaBuilder->builder.SetInsertPoint(thenBB);
}

void Builder::If::makeEndThen()
{
    // jump to the merge block
    glaBuilder->builder.CreateBr(mergeBB);

    if (elseBB) {
        // add else block to the function
        function->getBasicBlockList().push_back(elseBB);
        glaBuilder->builder.SetInsertPoint(elseBB);
    }
}

void Builder::If::makeEndIf()
{
    if (elseBB) {
        // jump to the merge block
        glaBuilder->builder.CreateBr(mergeBB);
    }

    // add the merge block to the function
    function->getBasicBlockList().push_back(mergeBB);
    glaBuilder->builder.SetInsertPoint(mergeBB);
}

// Start the beginning of a new loop. For inductive loops, specify the
// inductive variable, what value it starts at, when it finishes, and how
// much it increments by on each iteration. Also specify whether you want
// this Builder to do the increment (true), or if you will do it yourself
// (false).
void Builder::makeNewLoop()
{
    makeNewLoop(NULL, NULL, NULL, NULL, false);
}

void Builder::makeNewLoop(llvm::Value* inductiveVariable, llvm::Constant* from, llvm::Constant* finish,
                          llvm::Constant* increment,  bool builderDoesIncrement)
{
    llvm::Function* function = builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *headerBB = llvm::BasicBlock::Create(context, "loop-header", function);
    llvm::BasicBlock *mergeBB  = llvm::BasicBlock::Create(context, "loop-merge");

    LoopData ld = { };
    ld.exit   = mergeBB;
    ld.header = headerBB;
    ld.counter = inductiveVariable;
    ld.finish = finish;
    ld.increment = increment;
    ld.function = function;
    ld.builderDoesIncrement = builderDoesIncrement;

    // If we were passed a non-null inductive variable, then we're inductive
    if (inductiveVariable) {
        ld.isInductive = true;
        builder.CreateStore(from, inductiveVariable);
    }

    // If we were passed an inductive variable, all other arguments should be defined
    assert(! ld.isInductive || (inductiveVariable && from && finish && increment));

    loops.push(ld);

    // Branch into the loop
    builder.CreateBr(headerBB);

    // Set ourselves inside the loop
    builder.SetInsertPoint(headerBB);
}

// Add a back-edge (e.g "continue") for the innermost loop that you're in
void Builder::makeLoopBackEdge()
{
    LoopData ld = loops.top();

    // If we're not inductive, just branch back.
    if (! ld.isInductive) {
        builder.CreateBr(ld.header);
        return;
    }

    //  Otherwise we have to (possibly) increment the inductive variable, and
    // set up a conditional exit.
    assert(ld.counter && ld.counter->getType()->isPointerTy() && ld.increment && ld.finish);

    llvm::Value* iPrev = builder.CreateLoad(ld.counter);
    llvm::Value* cmp   = NULL;
    llvm::Value* iNext = NULL;

    // iNext is either iPrev if the user did the increment theirselves, or it is
    // the result of the increment if we have to do it ourselves.
    switch (ld.counter->getType()->getContainedType(0)->getTypeID()) {
    case llvm::Type::FloatTyID:
        iNext = ! ld.builderDoesIncrement ? iPrev : builder.CreateFAdd(iPrev, ld.increment);
        cmp   = builder.CreateFCmpOGE(iNext, ld.finish);
        break;
    case llvm::Type::IntegerTyID:
        iNext = ! ld.builderDoesIncrement ? iPrev : builder.CreateAdd(iPrev, ld.increment);
        cmp   = builder.CreateICmpSGE(iNext, ld.finish);
        break;
    default: gla::UnsupportedFunctionality("unknown type in inductive variable");
    }

    // Store the new value for the inductive variable back in
    if (ld.builderDoesIncrement)
        builder.CreateStore(iNext, ld.counter);

    // If iNext exceeds ld.finish, exit the loop, else branch back to
    // the header
    builder.CreateCondBr(cmp, ld.exit, ld.header);
}

// Add an exit (e.g. "break") for the innermost loop that you're in
void Builder::makeLoopExit()
{
    builder.CreateBr(loops.top().exit);
}

// Close the innermost loop that you're in
void Builder::closeLoop()
{
    // Branch back through the loop
    makeLoopBackEdge();

    LoopData ld = loops.top();

    ld.function->getBasicBlockList().push_back(ld.exit);
    builder.SetInsertPoint(ld.exit);

    loops.pop();
}


}; // end gla namespace
