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

Builder::Builder(llvm::IRBuilder<>& b, gla::Manager* m) :
    accessRightToLeft(true),
    builder(b),
    manager(m),
    module(manager->getModule()),
    context(builder.getContext()),
    mainFunction(0),
    stageEpilogue(0),
    stageExit(0)
{
    clearAccessChain();
}

Builder::~Builder()
{
    for (std::vector<Matrix*>::iterator i = matrixList.begin(); i != matrixList.end(); ++i)
        delete *i;
}

void Builder::clearAccessChain()
{
    accessChain.base.clear();
    accessChain.indexChain.clear();
    accessChain.gep = 0;
    accessChain.swizzle.clear();
    accessChain.component = 0;
    accessChain.swizzleResultType = 0;
    accessChain.swizzleTargetWidth = 0;
    accessChain.isRValue = false;
    accessChain.trackOutputIndex = false;
}

void Builder::accessChainPushSwizzleLeft(std::vector<int>& swizzle, const llvm::Type* type, int width)
{
    // if needed, propagate the swizzle for the current access chain
    if (accessChain.swizzle.size()) {
        for (unsigned int i = 0; i < accessChain.swizzle.size(); ++i) {
            accessChain.swizzle[i] = swizzle[accessChain.swizzle[i]];
        }
    } else {
        accessChain.swizzleResultType = type;
        accessChain.swizzle = swizzle;
    }

    // track width the swizzle operates on
    accessChain.swizzleTargetWidth = width;

    // determine if we need to track this swizzle anymore
    simplifyAccessChainSwizzle();
}

void Builder::accessChainPushSwizzleRight(std::vector<int>& swizzle, const llvm::Type* type, int width)
{
    // if needed, propagate the swizzle for the current access chain
    if (accessChain.swizzle.size()) {
        std::vector<int> oldSwizzle = accessChain.swizzle;
        accessChain.swizzle.resize(0);
        for (unsigned int i = 0; i < swizzle.size(); ++i) {
            accessChain.swizzle.push_back(oldSwizzle[swizzle[i]]);
        }
    } else {
        accessChain.swizzle = swizzle;
    }

    // track the final type, which always changes with each push
    accessChain.swizzleResultType = type;

    // track width the swizzle operates on; once known, it does not change
    if (accessChain.swizzleTargetWidth == 0)
        accessChain.swizzleTargetWidth = width;

    // determine if we need to track this swizzle anymore
    simplifyAccessChainSwizzle();
}

// clear out swizzle if it is redundant
void Builder::simplifyAccessChainSwizzle()
{
    // if swizzle has fewer components than our target, it is a writemask
    if (accessChain.swizzleTargetWidth > (int)accessChain.swizzle.size())
        return;

    // if components are out of order, it is a swizzle
    for (unsigned int i = 0; i < accessChain.swizzle.size(); ++i) {
        if (i != accessChain.swizzle[i])
            return;
    }

    // otherwise, there is no need to track this swizzle
    accessChain.swizzle.clear();
    accessChain.swizzleTargetWidth = 0;
}

void Builder::setAccessChainRValue(SuperValue rValue)
{
    // We don't support exposed pointers, so no r-value should be a pointer.
    // If code is calling this with a pointer, it should probably be calling
    // setAccessChainLValue() instead.
    assert(! llvm::isa<llvm::PointerType>(rValue.getValue()->getType()));

    accessChain.isRValue = true;
    accessChain.base = rValue;

    // Because we might later turn an r-value into an l-value, just
    // to use the GEP mechanism for complex r-value dereferences,
    // push a pointer dereference now.
    accessChain.indexChain.push_back(MakeIntConstant(context, 0));
}

void Builder::setAccessChainLValue(SuperValue lValue)
{
    // l-values need to be allocated somewhere, so we expect a pointer.
    assert(llvm::isa<llvm::PointerType>(lValue.getValue()->getType()));

    // Pointers need to push a 0 on the gep chain to dereference them.
    accessChain.indexChain.push_back(MakeIntConstant(context, 0));

    accessChain.base = lValue;
}

void Builder::setAccessChainPipeValue(llvm::Value* val)
{
    // evolve the accessChain
    accessChain.indexChain.clear();

    setAccessChainRValue(val);
}

Builder::SuperValue Builder::collapseAccessChain()
{
    assert(accessChain.isRValue == false);

    if (accessChain.indexChain.size() > 1) {
        if (accessChain.gep == 0) {
            if (accessRightToLeft)
                std::reverse(accessChain.indexChain.begin(), accessChain.indexChain.end());
            accessChain.gep = createGEP(accessChain.base, accessChain.indexChain);

            if (accessChain.trackOutputIndex)
                trackOutputIndex(accessChain.base, accessChain.indexChain.back());
        }

        return accessChain.gep;
    } else {
        if (accessChain.trackOutputIndex)
            trackOutputIndex(accessChain.base, 0);

        return accessChain.base;
    }
}

Builder::SuperValue Builder::collapseInputAccessChain()
{
    if (accessChain.indexChain.size() == 1) {
        // no need to reverse a single index
        return accessChain.indexChain.front();

    } else if (accessChain.indexChain.size() > 1) {
        if (accessRightToLeft)
            std::reverse(accessChain.indexChain.begin(), accessChain.indexChain.end());
        UnsupportedFunctionality("More than one dimension on input");
    }

    // if no indexChain, we have nothing to add to input slot
    return MakeIntConstant(context, 0);
}

void Builder::accessChainStore(SuperValue value)
{
    assert(accessChain.isRValue == false);

    SuperValue base = collapseAccessChain();
    SuperValue source = value;

    // if swizzle exists, it is out-of-order or not full, we must load the target vector,
    // extract and insert elements to perform writeMask and/or swizzle
    if (accessChain.swizzle.size()) {

        llvm::Value* shadowVector = createLoad(base);

        // walk through the swizzle
        for (unsigned int i = 0; i < accessChain.swizzle.size(); ++i) {

            // extract scalar if needed
            llvm::Value* component = value;
            if (IsVector(component))
                component = builder.CreateExtractElement(value, MakeIntConstant(context, i));

            assert(IsScalar(component));

            // insert to our target at swizzled index
            shadowVector = builder.CreateInsertElement(shadowVector, component, MakeIntConstant(context, accessChain.swizzle[i]));
        }

        source = shadowVector;
    }

    if (accessChain.component)
        UnsupportedFunctionality("store to variable vector channel");

    createStore(source, base);
}

Builder::SuperValue Builder::accessChainLoad()
{
    SuperValue base = accessChain.base;
    SuperValue value;

    if (accessChain.isRValue) {
        if (accessChain.indexChain.size() > 1) {

            // create space for our r-value on the stack
            SuperValue lVal;
            lVal = createVariable(ESQLocal, 0, accessChain.base->getType(), accessChain.base.isMatrix(), 0, 0, "indexable");

            // store into it
            createStore(accessChain.base, lVal);

            // move base to the new alloca
            accessChain.base = lVal;
            accessChain.isRValue = false;

            // GEP from local alloca
            value = createLoad(collapseAccessChain());
        } else {
            value = accessChain.base;
        }
    } else {
        value = createLoad(collapseAccessChain());
    }

    if (accessChain.component)
        UnsupportedFunctionality("extract from variable vector component");

    if (accessChain.swizzle.size())
        value = createSwizzle(value, accessChain.swizzle, accessChain.swizzleResultType);

    return value;
}

void Builder::leaveFunction(bool main)
{
    llvm::BasicBlock* BB = builder.GetInsertBlock();
    llvm::Function* F = builder.GetInsertBlock()->getParent();
    assert(BB && F);

    // If our function did not contain a return,
    // return void now
    if (0 == BB->getTerminator()) {

        // Whether we're in an unreachable (non-entry) block
        bool unreachable = &*F->begin() != BB && pred_begin(BB) == pred_end(BB);

        if (main && !unreachable) {
            // If we're leaving main and it is not terminated,
            // generate our pipeline writes
            makeMainReturn(true);
        } else if (unreachable)
            // If we're not the entry block, and we have no predecessors, we're
            // unreachable, so don't bother adding a return instruction in
            // (e.g. we're in a post-return block). Otherwise add a ret void.
            builder.CreateUnreachable();
        else
            makeReturn(true);
    }

    if (main)
        closeMain();
}

llvm::BasicBlock* Builder::makeMain()
{
    assert(! mainFunction);

    llvm::BasicBlock* entry;
    std::vector<const llvm::Type*> params;

    stageEpilogue = llvm::BasicBlock::Create(context, "stage-epilogue");
    stageExit    = llvm::BasicBlock::Create(context, "stage-exit");

    mainFunction = makeFunctionEntry(gla::GetVoidType(context), "main", params, &entry, true /* needs external visibility */);

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


void Builder::makeReturn(bool implicit, llvm::Value* retVal, bool isMain)
{
    if (isMain && retVal)
        gla::UnsupportedFunctionality("return value from main()");

    if (isMain)
        builder.CreateBr(stageEpilogue);
    else if (retVal)
        builder.CreateRet(retVal);
    else
        builder.CreateRetVoid();

    if (! implicit)
        createAndSetNoPredecessorBlock("post-return");
}

void Builder::makeDiscard(bool isMain)
{
    if (! isMain)
        gla::UnsupportedFunctionality("discard from non-main functions");

    createIntrinsicCall(llvm::Intrinsic::gla_discard);
    builder.CreateBr(stageExit);

    createAndSetNoPredecessorBlock("post-discard");
}

void Builder::createAndSetNoPredecessorBlock(std::string name)
{
    builder.SetInsertPoint(llvm::BasicBlock::Create(context, name,
                                                    builder.GetInsertBlock()->getParent()));

}

llvm::Function* Builder::makeFunctionEntry(const llvm::Type* type, const char* name, const std::vector<const llvm::Type*>& paramTypes, llvm::BasicBlock** entry, bool external)
{
    llvm::FunctionType *functionType = llvm::FunctionType::get(type, paramTypes, false);
    llvm::Function *function = llvm::Function::Create(functionType, external ? llvm::Function::ExternalLinkage : llvm::Function::InternalLinkage, name, module);

    // For shaders, we want everything passed in registers
    function->setCallingConv(llvm::CallingConv::Fast);

    if (entry)
        *entry = llvm::BasicBlock::Create(context, "entry", function);

    return function;
}

llvm::Constant* Builder::getConstant(std::vector<llvm::Constant*>& constants, const llvm::Type* type)
{
    assert(type);

    switch (type->getTypeID()) {
    case llvm::Type::IntegerTyID:
    case llvm::Type::FloatTyID:
        return constants[0];
    case llvm::Type::VectorTyID:
        return llvm::ConstantVector::get(constants);
    case llvm::Type::ArrayTyID:
        return llvm::ConstantArray::get(llvm::dyn_cast<llvm::ArrayType>(type), constants);
    case llvm::Type::StructTyID:
        return llvm::ConstantStruct::get(llvm::dyn_cast<llvm::StructType>(type), constants);
    default:
        assert(0 && "Constant type in TopBuilder");
    }

    return 0;
}

Builder::SuperValue Builder::createVariable(EStorageQualifier storageQualifier, int storageInstance,
                                            const llvm::Type* type, bool isMatrix, llvm::Constant* initializer, const std::string* annotation,
                                            const std::string& name)
{
    std::string annotatedName;
    std::string pipelineName;
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
    case ESQResource:
        addressSpace = gla::ResourceAddressSpace;
        linkage = llvm::GlobalVariable::ExternalLinkage;
        global = true;
        readOnly = true;
        break;

    case ESQUniform:
        addressSpace = gla::ConstantAddressSpaceBase + storageInstance;
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
        pipelineName = annotatedName;
        if (annotatedName.substr(0, 3) == "gl_")
            annotatedName.erase(0, 3);
        annotatedName.append("_shadow");
        global = true;
        if (initializer == 0)
            initializer = llvm::Constant::getNullValue(type);
        break;

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
            const llvm::ArrayType* arrayType = llvm::dyn_cast<llvm::ArrayType>(value->getType()->getContainedType(0));
            if (arrayType) {
                for (int index = 0; index < arrayType->getNumElements(); ++index) {
                    char buf[8];
                    itoa(index, buf, 10);
                    PipelineSymbol symbol = {pipelineName + "[" + buf + "]", arrayType->getContainedType(0)};
                    manager->getPipeOutSymbols().push_back(symbol);

                    // wait until specific indices are used (or the whole array)
                    // to know an array element is active
                    copyOutActive.push_back(false);
                }
            } else {
                PipelineSymbol symbol = {pipelineName, value->getType()->getContainedType(0)};
                manager->getPipeOutSymbols().push_back(symbol);
                copyOutActive.push_back(true);
            }
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
    } else {
        if (llvm::isa<llvm::PointerType>(lValue.getValue()->getType()))
            return builder.CreateLoad(lValue);
        else
            return lValue;
    }
}

Builder::SuperValue Builder::createGEP(SuperValue gepValue, llvm::ArrayRef<llvm::Value*> gepIndexChain)
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

void Builder::trackOutputIndex(SuperValue base, const llvm::Value* gepIndex)
{
    int arrayIndex = -1;   // we'll use -1 to mean the whole array

    // If the entire array is being accessed (gepIndex pointer is 0)
    // or a variable index is used, then output the whole array.
    if (gepIndex && llvm::isa<llvm::ConstantInt>(gepIndex))
        arrayIndex = gla::GetConstantInt(gepIndex);

    int slot = 0;
    for (unsigned int out = 0; out < copyOuts.size(); ++out) {
        const llvm::ArrayType* arrayType = llvm::dyn_cast<llvm::ArrayType>(copyOuts[out]->getType()->getContainedType(0));
        if (arrayType) {
            if (copyOuts[out] == base) {
                if (arrayIndex == -1) {
                    for (int index = 0; index < arrayType->getNumElements(); ++index)
                        copyOutActive[slot + index] = true;
                } else
                    copyOutActive[slot + arrayIndex] = true;
            }
            slot += arrayType->getNumElements();
        } else
            slot++;
    }
}

void Builder::copyOutPipeline()
{
    int slot = 0;

    for (unsigned int out = 0; out < copyOuts.size(); ++out) {
        const llvm::ArrayType* arrayType = llvm::dyn_cast<llvm::ArrayType>(copyOuts[out]->getType()->getContainedType(0));
        if (arrayType) {
            std::vector<llvm::Value*> gepChain;
            gepChain.push_back(MakeIntConstant(context, 0));
            for (int index = 0; index < arrayType->getNumElements(); ++index) {
                if (copyOutActive[slot]) {
                    gepChain.push_back(MakeIntConstant(context, index));
                    llvm::Value* loadVal = builder.CreateLoad(createGEP(copyOuts[out], gepChain));
                    writePipeline(loadVal, MakeUnsignedConstant(context, slot));
                    gepChain.pop_back();
                }
                ++slot;
            }
        } else {
            llvm::Value* loadVal = builder.CreateLoad(copyOuts[out]);
            writePipeline(loadVal, MakeUnsignedConstant(context, slot));
            ++slot;
        }
    }
}

void Builder::writePipeline(llvm::Value* outValue, int slot, int mask, EInterpolationMethod method, EInterpolationLocation location)
{
    writePipeline(outValue, MakeUnsignedConstant(context, slot), mask, method, location);
}

void Builder::writePipeline(llvm::Value* outValue, llvm::Value* slot, int mask, EInterpolationMethod method, EInterpolationLocation location)
{
    llvm::Constant *maskConstant = MakeIntConstant(context, mask);

    EInterpolationMode mode = {};
    mode.EIMMethod   = method;
    mode.EIMLocation = location;

    if (! llvm::isa<llvm::IntegerType>(slot->getType()))
        gla::UnsupportedFunctionality("Pipeline write using non-integer index");

    // This correction is necessary for some front ends, which might allow
    // "interpolated" integers or Booleans.
    if (! GetBasicType(outValue)->isFloatTy())
        mode.EIMMethod = EIMNone;

    llvm::Function *intrinsic;
    if (mode.EIMMethod == EIMNone) {
        llvm::Intrinsic::ID intrinsicID;
        switch(GetBasicTypeID(outValue)) {
        case llvm::Type::IntegerTyID:   intrinsicID = llvm::Intrinsic::gla_writeData;   break;
        case llvm::Type::FloatTyID:     intrinsicID = llvm::Intrinsic::gla_fWriteData;  break;
        default:                        assert(! "Unsupported type in writePipeline");
        }

        intrinsic = getIntrinsic(intrinsicID, outValue->getType());
        builder.CreateCall3(intrinsic, slot, maskConstant, outValue);
    } else {
        llvm::Constant *modeConstant = MakeUnsignedConstant(context, reinterpret_cast<int&>(mode));
        intrinsic = getIntrinsic(llvm::Intrinsic::gla_fWriteInterpolant, outValue->getType());
        builder.CreateCall4(intrinsic, slot, maskConstant, modeConstant, outValue);
    }
}

llvm::Value* Builder::readPipeline(const llvm::Type* type, std::string& name, int slot, int mask,
                                   EInterpolationMethod method, EInterpolationLocation location,
                                   llvm::Value* offset, llvm::Value* sampleIdx)
{
    llvm::Constant *slotConstant = MakeUnsignedConstant(context, slot);
    llvm::Constant *maskConstant = MakeIntConstant(context, mask);

    EInterpolationMode mode = {};
    mode.EIMMethod   = method;
    mode.EIMLocation = location;

    // This correction is necessary for some front ends, which might allow
    // "interpolated" integers or Booleans.
    if (! GetBasicType(type)->isFloatTy())
        mode.EIMMethod = EIMNone;

    llvm::Function *intrinsic;
    if (mode.EIMMethod != EIMNone) {
        llvm::Constant *modeConstant = MakeUnsignedConstant(context, reinterpret_cast<int&>(mode));

        if (sampleIdx) {
            assert(0 == offset);
            intrinsic = getIntrinsic(llvm::Intrinsic::gla_fReadInterpolantSample, type);
            return builder.CreateCall4(intrinsic, slotConstant, maskConstant, modeConstant, sampleIdx, name);
        } else if (offset) {
            assert(0 == sampleIdx);
            intrinsic = getIntrinsic(llvm::Intrinsic::gla_fReadInterpolantOffset, type, offset->getType());
            return builder.CreateCall4(intrinsic, slotConstant, maskConstant, modeConstant, offset, name);
        } else {
            intrinsic = getIntrinsic(llvm::Intrinsic::gla_fReadInterpolant, type);
            return builder.CreateCall3(intrinsic, slotConstant, maskConstant, modeConstant, name);
        }
    } else {
        switch (GetBasicTypeID(type)) {
        case llvm::Type::IntegerTyID:   intrinsic = getIntrinsic(llvm::Intrinsic::gla_readData, type); break;
        case llvm::Type::FloatTyID:     intrinsic = getIntrinsic(llvm::Intrinsic::gla_fReadData, type); break;
        }

        return builder.CreateCall2(intrinsic, slotConstant, maskConstant, name);
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

llvm::Value* Builder::createSwizzle(llvm::Value* source, const std::vector<int>& channels, const llvm::Type* finalType)
{
    int swizMask = 0;
    for (unsigned int i = 0; i < channels.size(); ++i) {
        swizMask |= channels[i] << i*2;
    }

    return createSwizzle(source, swizMask, finalType);
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
    if (left.isMatrix() && IsVector(right.getValue())) {
        assert(left.getMatrix()->getNumColumns() == GetComponentCount(right.getValue()));

        return createMatrixTimesVector(left.getMatrix(), right.getValue());
    }

    // vector times matrix
    if (IsVector(left.getValue()) && right.isMatrix()) {
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

    return createCompare(left, right, allEqual);
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
    llvm::Intrinsic::ID dotIntrinsic;
    switch (matrix->getNumRows()) {
    case 2:
        dotIntrinsic = llvm::Intrinsic::gla_fDot2;
        break;
    case 3:
        dotIntrinsic = llvm::Intrinsic::gla_fDot3;
        break;
    case 4:
        dotIntrinsic = llvm::Intrinsic::gla_fDot4;
        break;
    default:
        assert(! "bad matrix size in createVectorTimesMatrix");
    }

    llvm::Function *dot = Builder::getIntrinsic(dotIntrinsic, GetBasicType(lvector), lvector->getType(), lvector->getType());

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
llvm::Function* Builder::getIntrinsic(llvm::Intrinsic::ID ID)
{
    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID);
}

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

llvm::Function* Builder::getIntrinsic(llvm::Intrinsic::ID ID, const llvm::Type* type1, const llvm::Type* type2, const llvm::Type* type3, const llvm::Type* type4, const llvm::Type* type5)
{
    const llvm::Type* intrinsicTypes[] = {
        type1,
        type2,
        type3,
        type4,
        type5};

    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID, intrinsicTypes, 5);
}

void Builder::promoteScalar(SuperValue& left, SuperValue& right)
{
    int direction;
    if (const llvm::PointerType* pointer = llvm::dyn_cast<const llvm::PointerType>(left->getType()))
        direction = GetComponentCount(right) - GetComponentCount(pointer->getContainedType(0));
    else
        direction = GetComponentCount(right) - GetComponentCount(left);

    if (direction > 0)
        left = gla::Builder::smearScalar(left, right->getType());
    else if (direction < 0)
        right = gla::Builder::smearScalar(right, left->getType());

    return;
}

llvm::Value* Builder::smearScalar(llvm::Value* scalar, const llvm::Type* vectorType)
{
    assert(gla::IsScalar(scalar->getType()));
    return createSwizzle(scalar, 0x00, vectorType);
}

// Accept all parameters needed to create LunarGLASS texture intrinsics
// Select the correct intrinsic based on the inputs, and make the call
// TODO:  Expand this beyond current level of GLSL 1.2 functionality
llvm::Value* Builder::createTextureCall(const llvm::Type* resultType, gla::ESamplerType samplerType, int texFlags, const TextureParameters& parameters)
{
    bool floatReturn = gla::GetBasicType(resultType)->isFloatTy();

    // Max args based on LunarGLASS TopIR, no SOA
    static const int maxTextureArgs = 9;
    llvm::Value* texArgs[maxTextureArgs] = {0};

    // Base case: First texture arguments are fixed for most intrinsics
    int numArgs = 4;

    texArgs[GetTextureOpIndex(ETOSamplerType)] = MakeIntConstant(context, samplerType);
    texArgs[GetTextureOpIndex(ETOSamplerLoc)]  = parameters.ETPSampler;
    texArgs[GetTextureOpIndex(ETOFlag)]        = MakeUnsignedConstant(context, *(int*)&texFlags);
    texArgs[GetTextureOpIndex(ETOCoord)]       = parameters.ETPCoords;

    llvm::Intrinsic::ID intrinsicID = llvm::Intrinsic::not_intrinsic;

    // Look at feature flags to determine which intrinsic is needed
    if (texFlags & ETFFetch) {

        intrinsicID = (floatReturn) ? llvm::Intrinsic::gla_fTexelFetchOffset
                                    : llvm::Intrinsic::gla_texelFetchOffset;

    } else if (texFlags & ETFGather) {

        intrinsicID = (floatReturn) ? llvm::Intrinsic::gla_fTexelGather
                                    : llvm::Intrinsic::gla_texelGather;

    } else if (parameters.ETPGradX || parameters.ETPGradY) {

        intrinsicID = (floatReturn) ? llvm::Intrinsic::gla_fTextureSampleLodRefZOffsetGrad
                                    : llvm::Intrinsic::gla_textureSampleLodRefZOffsetGrad;

    } else if (texFlags & ETFOffsetArg) {

        intrinsicID = (floatReturn) ? llvm::Intrinsic::gla_fTextureSampleLodRefZOffset
                                    : llvm::Intrinsic::gla_textureSampleLodRefZOffset;

    } else if (texFlags & ETFBias || texFlags & ETFLod || texFlags & ETFShadow) {

        intrinsicID = (floatReturn) ? llvm::Intrinsic::gla_fTextureSampleLodRefZ
                                    : llvm::Intrinsic::gla_textureSampleLodRefZ;
    } else {

        intrinsicID = (floatReturn) ? llvm::Intrinsic::gla_fTextureSample
                                    : llvm::Intrinsic::gla_textureSample;
    }

    // Set fields based on argument flags
    if (texFlags & ETFBiasLodArg)
        texArgs[GetTextureOpIndex(ETOBiasLod)] = parameters.ETPBiasLod;

    if (texFlags & ETFRefZArg)
        texArgs[GetTextureOpIndex(ETORefZ)] = parameters.ETPShadowRef;

    if (texFlags & ETFOffsetArg)
        texArgs[GetTextureOpIndex(ETOOffset)] = parameters.ETPOffset;

    llvm::Function* intrinsic = 0;

    // Initialize required operands based on intrinsic
    switch (intrinsicID) {
    case llvm::Intrinsic::gla_textureSample:
    case llvm::Intrinsic::gla_fTextureSample:
        // Base case
        break;

    case llvm::Intrinsic::gla_textureSampleLodRefZ:
    case llvm::Intrinsic::gla_fTextureSampleLodRefZ:

        numArgs = 6;

        if (! texArgs[GetTextureOpIndex(ETOBiasLod)])
            texArgs[GetTextureOpIndex(ETOBiasLod)] = llvm::UndefValue::get(GetFloatType(context));

        if (! texArgs[GetTextureOpIndex(ETORefZ)])
            texArgs[GetTextureOpIndex(ETORefZ)]    = llvm::UndefValue::get(GetFloatType(context));

        // Texcoords are the only flexible parameter for this intrinsic, no need to getIntrinsic here

        break;

    case llvm::Intrinsic::gla_textureSampleLodRefZOffset:
    case llvm::Intrinsic::gla_fTextureSampleLodRefZOffset:

        numArgs = 7;

        if (! texArgs[GetTextureOpIndex(ETOBiasLod)])
            texArgs[GetTextureOpIndex(ETOBiasLod)] = llvm::UndefValue::get(GetFloatType(context));

        if (! texArgs[GetTextureOpIndex(ETORefZ)])
            texArgs[GetTextureOpIndex(ETORefZ)]    = llvm::UndefValue::get(GetFloatType(context));

        if (! texArgs[GetTextureOpIndex(ETOOffset)])
            texArgs[GetTextureOpIndex(ETOOffset)]  = llvm::UndefValue::get(GetIntType(context));


        // We know our flexible types when looking at the intrinsicID, so create our intrinsic here
        intrinsic = getIntrinsic(intrinsicID, resultType, texArgs[GetTextureOpIndex(ETOCoord)]->getType(),
                                                          texArgs[GetTextureOpIndex(ETOOffset)]->getType());

        break;

    case llvm::Intrinsic::gla_textureSampleLodRefZOffsetGrad:
    case llvm::Intrinsic::gla_fTextureSampleLodRefZOffsetGrad:

        numArgs = 9;

        if (! texArgs[GetTextureOpIndex(ETOBiasLod)])
            texArgs[GetTextureOpIndex(ETOBiasLod)] = llvm::UndefValue::get(GetFloatType(context));

        if (! texArgs[GetTextureOpIndex(ETORefZ)])
            texArgs[GetTextureOpIndex(ETORefZ)]    = llvm::UndefValue::get(GetFloatType(context));

        if (! texArgs[GetTextureOpIndex(ETOOffset)])
            texArgs[GetTextureOpIndex(ETOOffset)]  = llvm::UndefValue::get(GetIntType(context));

        assert(parameters.ETPGradX);
        assert(parameters.ETPGradY);

        texArgs[GetTextureOpIndex(ETODPdx)] = parameters.ETPGradX;
        texArgs[GetTextureOpIndex(ETODPdy)] = parameters.ETPGradY;

        // We know our flexible types when looking at the intrinsicID, so create our intrinsic here
        intrinsic = getIntrinsic(intrinsicID, resultType, texArgs[GetTextureOpIndex(ETOCoord)]->getType(),
                                                          texArgs[GetTextureOpIndex(ETOOffset)]->getType(),
                                                          texArgs[GetTextureOpIndex(ETODPdx)]->getType(),
                                                          texArgs[GetTextureOpIndex(ETODPdy)]->getType());

        break;

    case llvm::Intrinsic::gla_texelFetchOffset:
    case llvm::Intrinsic::gla_fTexelFetchOffset:

        // LOD and sample can share the BiasLod field
        // RefZ is provided so our operand order matches every other texture op
        numArgs = 7;

        if (! texArgs[GetTextureOpIndex(ETOBiasLod)])
            texArgs[GetTextureOpIndex(ETOBiasLod)] = llvm::UndefValue::get(GetIntType(context));

        if (! texArgs[GetTextureOpIndex(ETORefZ)])
            texArgs[GetTextureOpIndex(ETORefZ)]    = llvm::UndefValue::get(GetFloatType(context));

        if (! texArgs[GetTextureOpIndex(ETOOffset)])
            texArgs[GetTextureOpIndex(ETOOffset)]  = llvm::UndefValue::get(GetIntType(context));

        // We know our flexible types when looking at the intrinsicID, so create our intrinsic here
        intrinsic = getIntrinsic(intrinsicID, resultType, texArgs[GetTextureOpIndex(ETOCoord)]->getType(),
                                                          texArgs[GetTextureOpIndex(ETOBiasLod)]->getType(),
                                                          texArgs[GetTextureOpIndex(ETOOffset)]->getType());

        break;

    case llvm::Intrinsic::gla_texelGather:
    case llvm::Intrinsic::gla_fTexelGather:

        // Component select resides in BiasLod field
        numArgs = 6;

        if (! texArgs[GetTextureOpIndex(ETOBiasLod)])
            texArgs[GetTextureOpIndex(ETOBiasLod)] = llvm::UndefValue::get(GetIntType(context));

        if (! texArgs[GetTextureOpIndex(ETORefZ)])
            texArgs[GetTextureOpIndex(ETORefZ)]    = llvm::UndefValue::get(GetFloatType(context));

        // If offset is defined, change the intrinsic
        if (texArgs[GetTextureOpIndex(ETOOffset)]) {

            intrinsicID = (floatReturn) ? llvm::Intrinsic::gla_fTexelGatherOffset
                                        : llvm::Intrinsic::gla_texelGatherOffset;
            numArgs = 7;

            intrinsic = getIntrinsic(intrinsicID, resultType, texArgs[GetTextureOpIndex(ETOCoord)]->getType(),
                                                              texArgs[GetTextureOpIndex(ETOOffset)]->getType());
        } else {

            texArgs[GetTextureOpIndex(ETOOffset)]  = llvm::UndefValue::get(GetIntType(context));

            intrinsic = getIntrinsic(intrinsicID, resultType, texArgs[GetTextureOpIndex(ETOCoord)]->getType());
        }

        break;

    default:
        gla::UnsupportedFunctionality("Texture intrinsic: ", intrinsicID);
    }

    // If we haven't already set our intrinsic, do so now with coordinates
    if (! intrinsic)
        intrinsic = getIntrinsic(intrinsicID, resultType, texArgs[GetTextureOpIndex(ETOCoord)]->getType());

    assert(intrinsic);

    return builder.CreateCall(intrinsic, texArgs, texArgs + numArgs);
}

llvm::Value* Builder::createTextureQueryCall(llvm::Intrinsic::ID intrinsicID, const llvm::Type* returnType, llvm::Constant* samplerType, llvm::Value* sampler, llvm::Value* src)
{
    llvm::Function* intrinsicName = 0;

    switch (intrinsicID) {
    case llvm::Intrinsic::gla_queryTextureSize:
        intrinsicName = getIntrinsic(intrinsicID, returnType);
        break;
    case llvm::Intrinsic::gla_fQueryTextureLod:
        intrinsicName = getIntrinsic(intrinsicID, returnType, src->getType());
        break;
    default:
        gla::UnsupportedFunctionality("Texture query intrinsic");
    }

    assert(intrinsicName);

    return builder.CreateCall3(intrinsicName, samplerType, sampler, src);
}

llvm::Value* Builder::createSamplePositionCall(const llvm::Type* returnType, llvm::Value* sampleIdx)
{
    // Return type is only flexible type
    llvm::Function* intrinsicName = getIntrinsic(llvm::Intrinsic::gla_fSamplePosition, returnType);

    return builder.CreateCall(intrinsicName, sampleIdx);
}

llvm::Value* Builder::createBitFieldExtractCall(llvm::Value* value, llvm::Value* offset, llvm::Value* bits, bool isSigned)
{
    llvm::Intrinsic::ID intrinsicID = isSigned ? llvm::Intrinsic::gla_sBitFieldExtract
                                               : llvm::Intrinsic::gla_uBitFieldExtract;

    if (IsScalar(offset) == false || IsScalar(bits) == false)
        gla::UnsupportedFunctionality("bitFieldExtract operand types");

    // Dest and value are matching flexible types
    llvm::Function* intrinsicName = getIntrinsic(intrinsicID, value->getType(), value->getType());

    assert(intrinsicName);

    return builder.CreateCall3(intrinsicName, value, offset, bits);
}

llvm::Value* Builder::createBitFieldInsertCall(llvm::Value* base, llvm::Value* insert, llvm::Value* offset, llvm::Value* bits)
{
    llvm::Intrinsic::ID intrinsicID = llvm::Intrinsic::gla_bitFieldInsert;

    if (IsScalar(offset) == false || IsScalar(bits) == false)
        gla::UnsupportedFunctionality("bitFieldInsert operand types");

    // Dest, base, and insert are matching flexible types
    llvm::Function* intrinsicName = getIntrinsic(intrinsicID, base->getType(), base->getType(), base->getType());

    assert(intrinsicName);

    return builder.CreateCall4(intrinsicName, base, insert, offset, bits);
}

llvm::Value* Builder::createRecip(llvm::Value* operand)
{
    const llvm::Type* ty = operand->getType();

    if (GetBasicType(ty)->isFloatTy())
        return builder.CreateFDiv(llvm::ConstantFP::get(ty, 1.0), operand);

    UnsupportedFunctionality("Unknown type to be taking the reciprocal of: ", ty->getTypeID());

    return 0;
}

llvm::Value* Builder::createCompare(llvm::Value* value1, llvm::Value* value2, bool equal)
{
    if (llvm::isa<llvm::PointerType>(value1->getType()))
        value1 = builder.CreateLoad(value1);
    if (llvm::isa<llvm::PointerType>(value2->getType()))
        value2 = builder.CreateLoad(value2);

    llvm::Value* result;

    // Directly compare scalars and vectors.

    if (IsScalar(value1) || IsVector(value1)) {
        if (GetBasicTypeID(value1) == llvm::Type::FloatTyID) {
            if (equal)
                result = builder.CreateFCmpOEQ(value1, value2);
            else
                result = builder.CreateFCmpONE(value1, value2);
        } else {
            if (equal)
                result = builder.CreateICmpEQ(value1, value2);
            else
                result = builder.CreateICmpNE(value1, value2);
        }
    }

    if (IsScalar(value1))
        return result;

    // Reduce vector compares with any() and all().

    if (IsVector(value1)) {
        llvm::Intrinsic::ID intrinsicID;
        if (equal)
            intrinsicID = llvm::Intrinsic::gla_all;
        else
            intrinsicID = llvm::Intrinsic::gla_any;

        return createIntrinsicCall(intrinsicID, result);
    }

    // Recursively handle aggregates, which include matrices, arrays, and structures
    // and accumulate the results.

    // arrays (includes matrices)
    int numElements;
    const llvm::ArrayType* arrayType = llvm::dyn_cast<llvm::ArrayType>(value1->getType());
    if (arrayType)
        numElements = arrayType->getNumElements();
    else {
        // better be structure
        const llvm::StructType* structType = llvm::dyn_cast<llvm::StructType>(value1->getType());
        assert(structType);
        numElements = structType->getNumElements();
    }

    assert(numElements > 0);

    for (int element = 0; element < numElements; ++element) {
        // Get intermediate comparison values
        llvm::Value* element1 = builder.CreateExtractValue(value1, element, "element1");
        llvm::Value* element2 = builder.CreateExtractValue(value2, element, "element2");

        llvm::Value* subResult = createCompare(element1, element2, equal);

        // Accumulate intermediate comparison
        if (element == 0)
            result = subResult;
        else {
            if (equal)
                result = builder.CreateAnd(result, subResult);
            else
                result = builder.CreateOr(result, subResult);
        }
    }

    return result;
}

// deprecated, use createCompare(SuperValue, SuperValue, bool)
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

    if (llvm::isa<llvm::VectorType>(result->getType()))
        return createIntrinsicCall(intrinsicID, result);

    return result;
}

llvm::Value* Builder::createIntrinsicCall(llvm::Intrinsic::ID intrinsicID)
{
    return builder.CreateCall(getIntrinsic(intrinsicID));
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
       // scalar result type
       intrinsicName = getIntrinsic(intrinsicID, GetBasicType(operand->getType()), operand->getType());
       break;
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
    case llvm::Intrinsic::gla_fDot2:
    case llvm::Intrinsic::gla_fDot3:
    case llvm::Intrinsic::gla_fDot4:
        // scalar result type
        intrinsicName = getIntrinsic(intrinsicID, GetBasicType(lhs), lhs->getType(), rhs->getType());
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

llvm::Value* Builder::createConstructor(const std::vector<SuperValue>& sources, llvm::Value* constructee)
{
    unsigned int numTargetComponents = GetComponentCount(constructee);
    unsigned int targetComponent = 0;

    // Special case: when calling a vector constructor with a single scalar
    // argument, smear the scalar
    if (sources.size() == 1 && IsScalar(sources[0]) && numTargetComponents > 1) {
        return smearScalar(sources[0], constructee->getType());
    }

    for (unsigned int i = 0; i < sources.size(); ++i) {
        if (sources[i].isMatrix())
            gla::UnsupportedFunctionality("matrix in constructor");

        unsigned int sourceSize = GetComponentCount(sources[i]);

        unsigned int sourcesToUse = sourceSize;
        if (sourcesToUse + targetComponent > numTargetComponents)
            sourcesToUse = numTargetComponents - targetComponent;

        for (unsigned int s = 0; s < sourcesToUse; ++s) {
            llvm::Value* arg = sources[i];
            if (sourceSize > 1) {
                arg = builder.CreateExtractElement(arg, MakeIntConstant(context, s));
            }
            if (numTargetComponents > 1)
                constructee = builder.CreateInsertElement(constructee, arg, MakeIntConstant(context, targetComponent));
            else
                constructee = arg;
            ++targetComponent;
        }

        if (targetComponent >= numTargetComponents)
            break;
    }

    return constructee;
}

Builder::SuperValue Builder::createMatrixConstructor(const std::vector<SuperValue>& sources, SuperValue constructee)
{
    const Matrix* matrixee = constructee.getMatrix();

    // Will use a two step process
    // 1. make a compile-time 2D array of values
    // 2. copy it into the run-time constructee

    // Step 1.

    // initialize the array to the identity matrix
    llvm::Value* values[4][4];
    llvm::Value*  one = gla::MakeFloatConstant(context, 1.0);
    llvm::Value* zero = gla::MakeFloatConstant(context, 0.0);
    for (int col = 0; col < 4; ++col) {
        for (int row = 0; row < 4; ++row) {
            if (col == row)
                values[col][row] = one;
            else
                values[col][row] = zero;
        }
    }

    // modify components as dictated by the arguments
    if (sources.size() == 1 && IsScalar(sources[0])) {
        // a single scalar; resets the diagonals
        for (int col = 0; col < 4; ++col)
            values[col][col] = sources[0];
    } else if (sources[0].isMatrix()) {
        // a matrix; copy over the parts that exist in both the argument and constructee
        const Matrix* matrix = sources[0].getMatrix();
        int minCols = std::min(matrixee->getNumColumns(), matrix->getNumColumns());
        int minRows = std::min(matrixee->getNumRows(), matrix->getNumRows());
        for (int col = 0; col < minCols; ++col) {
            llvm::Value* column = builder.CreateExtractValue(matrix->getValue(), col, "__column");
            for (int row = 0; row < minRows; ++row)
                values[col][row] = builder.CreateExtractElement(column, MakeUnsignedConstant(context, row), "__element");
        }
    } else {
        // fill in the matrix in column-major order with whatever argument components are available
        int row = 0;
        int col = 0;

        for (int arg = 0; arg < sources.size(); ++arg) {
            llvm::Value* argComp = sources[arg];
            for (int comp = 0; comp < GetComponentCount(sources[arg]); ++comp) {
                if (GetComponentCount(sources[arg]) > 1)
                    argComp = builder.CreateExtractElement(sources[arg], MakeUnsignedConstant(context, comp), "__element");
                values[col][row++] = argComp;
                if (row == matrixee->getNumRows()) {
                    row = 0;
                    col++;
                }
            }
        }
    }

    // Step 2:  Copy into run-time result.
    for (int col = 0; col < matrixee->getNumColumns(); ++col) {
        llvm::Value* column = builder.CreateExtractValue(matrixee->getValue(), col, "__column");
        for (int row = 0; row < matrixee->getNumRows(); ++row) {
            column = builder.CreateInsertElement(column, values[col][row], MakeIntConstant(context, row), "__column");
        }
        constructee = builder.CreateInsertValue(constructee, column, col, "__matrix");
    }

    return newMatrix(constructee);
}

Builder::If::If(llvm::Value* cond, Builder* gb)
    : glaBuilder(gb)
    , condition(cond)
    , elseBB(0)

{
    function = glaBuilder->builder.GetInsertBlock()->getParent();

    // make the blocks, but only put the then-block into the function,
    // the else-block and merge-block will be added later, in order, after
    // earlier code is emitted
    thenBB = llvm::BasicBlock::Create(glaBuilder->context, "then", function);
    mergeBB = llvm::BasicBlock::Create(glaBuilder->context, "ifmerge");

    // Save the current block, so that we can add in the flow control split when
    // makeEndIf is called.
    headerBB = glaBuilder->builder.GetInsertBlock();

    glaBuilder->builder.SetInsertPoint(thenBB);
}

void Builder::If::makeBeginElse()
{
    // Close out the "then" by having it jump to the mergeBB
    glaBuilder->builder.CreateBr(mergeBB);

    // Make the else
    elseBB = llvm::BasicBlock::Create(glaBuilder->context, "else");

    // add else block to the function
    function->getBasicBlockList().push_back(elseBB);
    glaBuilder->builder.SetInsertPoint(elseBB);
}

void Builder::If::makeEndIf()
{
    // jump to the merge block
    glaBuilder->builder.CreateBr(mergeBB);

    // Go back the the headerBB and make the flow control split
    glaBuilder->builder.SetInsertPoint(headerBB);
    if (elseBB)
        glaBuilder->builder.CreateCondBr(condition, thenBB, elseBB);
    else
        glaBuilder->builder.CreateCondBr(condition, thenBB, mergeBB);

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
void Builder::makeLoopBackEdge(bool implicit)
{
    LoopData ld = loops.top();

    // If we're not inductive, just branch back.
    if (! ld.isInductive) {
        builder.CreateBr(ld.header);
        if (! implicit)
            createAndSetNoPredecessorBlock("post-loop-continue");

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

    if (! implicit) {
        createAndSetNoPredecessorBlock("post-loop-continue");
    }
}

// Add an exit (e.g. "break") for the innermost loop that you're in
void Builder::makeLoopExit()
{
    builder.CreateBr(loops.top().exit);

    createAndSetNoPredecessorBlock("post-loop-break");
}

// Close the innermost loop that you're in
void Builder::closeLoop()
{
    // Branch back through the loop
    makeLoopBackEdge(true);

    LoopData ld = loops.top();

    ld.function->getBasicBlockList().push_back(ld.exit);
    builder.SetInsertPoint(ld.exit);

    loops.pop();
}


}; // end gla namespace
