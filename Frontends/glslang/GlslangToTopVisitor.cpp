//===- GlslangToTop.cpp - Translate GLSL IR to LunarGLASS Top IR ---------===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (C) 2012 LunarG, Inc.
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
// Visit the nodes in the glslang intermediate tree representation to
// translate it to LunarGLASS TopIR.
//
//===----------------------------------------------------------------------===//

// Glslang includes
#include "glslang/Include/intermediate.h"
#include "glslang/Public/ShaderLang.h"
#include "glslang/MachineIndependent/SymbolTable.h"

// LunarGLASS includes
#include "LunarGLASSTopIR.h"
#include "LunarGLASSManager.h"
#include "Util.h"
#include "Exceptions.h"
#include "Options.h"
#include "TopBuilder.h"

// LLVM includes
#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Intrinsics.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/IRBuilder.h"
#include <string>
#include <map>
#include <list>
#include <vector>
#include <stack>

// Adapter includes
#include "GlslangToTopVisitor.h"

//
// Use this class to carry along data from node to node in
// the traversal
//
class TGlslangToTopTraverser : public TIntermTraverser {
public:
    TGlslangToTopTraverser(gla::Manager*);
    virtual ~TGlslangToTopTraverser();

    gla::Builder::SuperValue createLLVMVariable(TIntermSymbol* node);
    const llvm::Type* convertGlslangToGlaType(TType& type);
    gla::Builder::SuperValue createBinaryOperation(TOperator op, gla::Builder::SuperValue left, gla::Builder::SuperValue right, bool isFloat, bool isSigned);
    llvm::Value* createPipelineRead(TIntermSymbol*, int slot);
    int getNextInterpIndex(std::string& name);
    llvm::Constant* createLLVMConstant(TType& type, constUnion *consts, int& nextConst);

    llvm::LLVMContext &context;
    llvm::BasicBlock* shaderEntry;
    llvm::IRBuilder<> llvmBuilder;
    llvm::Module* module;

    gla::Builder* glaBuilder;
    int interpIndex;
    bool inMain;

    std::map<int, gla::Builder::SuperValue> namedValues;
    std::map<std::string, int> interpMap;
};

TGlslangToTopTraverser::TGlslangToTopTraverser(gla::Manager* manager)
    : context(llvm::getGlobalContext()), llvmBuilder(context),
      module(manager->getModule()), interpIndex(0), inMain(false), shaderEntry(0)
{
    // do this after the builder knows the module
    glaBuilder = new gla::Builder(llvmBuilder, manager);
    glaBuilder->clearAccessChain();
    glaBuilder->setAccessChainDirectionRightToLeft(false);

    shaderEntry = glaBuilder->makeMain();
    llvmBuilder.SetInsertPoint(shaderEntry);

    postVisit = true;
}

TGlslangToTopTraverser::~TGlslangToTopTraverser()
{
    delete glaBuilder;
}

//
// The rest of the file are the traversal functions.  The last one
// is the one that starts the traversal.
//
// Return true from interior nodes to have the external traversal
// continue on to children.  Return false if children were
// already processed.
//

void TranslateSymbol(TIntermSymbol* node, TIntermTraverser* it)
{
    TGlslangToTopTraverser* oit = static_cast<TGlslangToTopTraverser*>(it);

    TIntermSymbol* symbolNode = node->getAsSymbolNode();

    if (symbolNode == 0) {
        gla::UnsupportedFunctionality("glslang null symbol", gla::EATContinue);
        return;
    }

    bool input = false;
    switch (symbolNode->getQualifier()) {
    case EvqAttribute:
    case EvqVaryingIn:
    case EvqFragCoord:
    case EvqFace:
        input = true;
        break;
    }

    // L-value chains will be computed purely left to right, so now is clear time
    // (since we are on the symbol; the base of the expression, which is left-most)
    oit->glaBuilder->clearAccessChain();

    if (input) {
        gla::Builder::SuperValue rValue = oit->createPipelineRead(symbolNode, 0);
        oit->glaBuilder->setAccessChainRValue(rValue);
    } else {
        std::map<int, gla::Builder::SuperValue>::iterator iter;
        iter = oit->namedValues.find(symbolNode->getId());

        if (oit->namedValues.end() == iter) {
            // it was not found, create it
            oit->namedValues[symbolNode->getId()] = oit->createLLVMVariable(symbolNode);
        }

        // Track the current value
        oit->glaBuilder->setAccessChainLValue(oit->namedValues[symbolNode->getId()]);
    }
}

bool TranslateBinary(bool /* preVisit */, TIntermBinary* node, TIntermTraverser* it)
{
    TGlslangToTopTraverser* oit = static_cast<TGlslangToTopTraverser*>(it);

    gla::Builder::SuperValue result;
    llvm::Instruction::BinaryOps binOp = llvm::Instruction::BinaryOps(0);
    bool needsPromotion = true;

    switch (node->getOp()) {
    case EOpAdd:
    case EOpSub:
    case EOpMul:
    case EOpDiv:
    case EOpMod:
    case EOpRightShift:
    case EOpLeftShift:
    case EOpAnd:
    case EOpInclusiveOr:
    case EOpExclusiveOr:
    case EOpLogicalOr:
    case EOpLogicalXor:
    case EOpLogicalAnd:
        {
            node->getLeft()->traverse(oit);
            gla::Builder::SuperValue left = oit->glaBuilder->accessChainLoad();
            node->getRight()->traverse(oit);
            gla::Builder::SuperValue right = oit->glaBuilder->accessChainLoad();
            gla::Builder::SuperValue rValue = oit->createBinaryOperation(node->getOp(), left, right, true, false);
            oit->glaBuilder->clearAccessChain();
            oit->glaBuilder->setAccessChainRValue(rValue);
            return false;
        }
    case EOpAssign:
        {
            // GLSL says to evaluate the left before the right...
            node->getLeft()->traverse(oit);
            gla::Builder::AccessChain lValue = oit->glaBuilder->getAccessChain();
            oit->glaBuilder->clearAccessChain();  //?? when are clears really needed
            node->getRight()->traverse(oit);
            gla::Builder::SuperValue rValue = oit->glaBuilder->accessChainLoad();
            oit->glaBuilder->setAccessChain(lValue);
            oit->glaBuilder->accessChainStore(rValue);

            // assignments are expressions having an rValue after they are evaluated...
            oit->glaBuilder->clearAccessChain();
            oit->glaBuilder->setAccessChainRValue(rValue);
            return false;
        }

    case EOpAddAssign:
    case EOpSubAssign:
    case EOpMulAssign:
    case EOpVectorTimesMatrixAssign:
    case EOpVectorTimesScalarAssign:
    case EOpMatrixTimesScalarAssign:
    case EOpMatrixTimesMatrixAssign:
    case EOpDivAssign:
    case EOpModAssign:
    case EOpAndAssign:
    case EOpInclusiveOrAssign:
    case EOpExclusiveOrAssign:
    case EOpLeftShiftAssign:
    case EOpRightShiftAssign:
        gla::UnsupportedFunctionality("glslang binary assign", gla::EATContinue);
        return true;

    case EOpIndexDirect:
    case EOpIndexIndirect:
    case EOpIndexDirectStruct:
        {
            // this adapter is building access chains left to right
            // set up the access chain
            node->getLeft()->traverse(oit);
            // save it so that computing the right side doesn't trash it
            gla::Builder::AccessChain partial = oit->glaBuilder->getAccessChain();
            // compute the next component
            node->getRight()->traverse(oit);
            gla::Builder::SuperValue rValue = oit->glaBuilder->accessChainLoad();
            // make the new access chain to date
            oit->glaBuilder->setAccessChain(partial);
            oit->glaBuilder->accessChainPushLeft(rValue);
            return false;
        }

    case EOpVectorSwizzle:
        gla::UnsupportedFunctionality("glslang swizzle", gla::EATContinue);
        return true;

    case EOpEqual:
    case EOpNotEqual:
    case EOpLessThan:
    case EOpGreaterThan:
    case EOpLessThanEqual:
    case EOpGreaterThanEqual:
        gla::UnsupportedFunctionality("glslang binary relation", gla::EATContinue);
        return true;

    case EOpVectorTimesScalar:
        {
            node->getLeft()->traverse(oit);
            gla::Builder::SuperValue left = oit->glaBuilder->accessChainLoad();
            node->getRight()->traverse(oit);
            gla::Builder::SuperValue right = oit->glaBuilder->accessChainLoad();
            oit->glaBuilder->promoteScalar(left, right);
            gla::Builder::SuperValue rValue = oit->createBinaryOperation(EOpMul, left, right, true, false);
            oit->glaBuilder->clearAccessChain();
            oit->glaBuilder->setAccessChainRValue(rValue);
            return false;
        }

    case EOpVectorTimesMatrix:
    case EOpMatrixTimesVector:
    case EOpMatrixTimesScalar:
    case EOpMatrixTimesMatrix:

    default:
        gla::UnsupportedFunctionality("glslang binary matrix", gla::EATContinue);
        return true;
    }

    return true;
}

bool TranslateUnary(bool /* preVisit */, TIntermUnary* node, TIntermTraverser* it)
{
    TGlslangToTopTraverser* oit = static_cast<TGlslangToTopTraverser*>(it);

    switch (node->getOp()) {
    case EOpNegative:
    case EOpVectorLogicalNot:
    case EOpLogicalNot:
    case EOpBitwiseNot:

    case EOpPostIncrement:
    case EOpPostDecrement:
    case EOpPreIncrement:
    case EOpPreDecrement:

    case EOpConvIntToBool:
    case EOpConvFloatToBool:
    case EOpConvBoolToFloat:
    case EOpConvIntToFloat:
    case EOpConvFloatToInt:
    case EOpConvBoolToInt:

    case EOpRadians:
    case EOpDegrees:
    case EOpSin:
    case EOpCos:
    case EOpTan:
    case EOpAsin:
    case EOpAcos:
    case EOpAtan:

    case EOpExp:
    case EOpLog:
    case EOpExp2:
    case EOpLog2:
    case EOpSqrt:
    case EOpInverseSqrt:

    case EOpAbs:
    case EOpSign:
    case EOpFloor:
    case EOpCeil:
    case EOpFract:

    case EOpLength:
    case EOpNormalize:
    case EOpDPdx:
    case EOpDPdy:
    case EOpFwidth:

    case EOpAny:
    case EOpAll:

    default:
        gla::UnsupportedFunctionality("glslang unary", gla::EATContinue);
        return false;
    }

    return true;
}

bool TranslateAggregate(bool preVisit, TIntermAggregate* node, TIntermTraverser* it)
{
    TGlslangToTopTraverser* oit = static_cast<TGlslangToTopTraverser*>(it);

    if (node->getOp() == EOpNull) {
        gla::UnsupportedFunctionality("glslang aggregate: EOpNull", gla::EATContinue);
        return false;
    }

    switch (node->getOp()) {
    case EOpSequence:
        return true;
    case EOpComma:
        gla::UnsupportedFunctionality("glslang aggregate: comma", gla::EATContinue);
        return false;
    case EOpFunction:
        if (preVisit) {
            if (node->getName() == "main(") {
                oit->inMain = true;
                oit->llvmBuilder.SetInsertPoint(oit->shaderEntry);
            }
        } else {
            oit->glaBuilder->leaveFunction(oit->inMain);
            oit->inMain = false;
        }
        return true;

    case EOpFunctionCall:
    case EOpParameters:

    case EOpConstructFloat:
    case EOpConstructVec2:
    case EOpConstructVec3:
    case EOpConstructVec4:
    case EOpConstructBool:
    case EOpConstructBVec2:
    case EOpConstructBVec3:
    case EOpConstructBVec4:
    case EOpConstructInt:
    case EOpConstructIVec2:
    case EOpConstructIVec3:
    case EOpConstructIVec4:
    case EOpConstructMat2:
    case EOpConstructMat3:
    case EOpConstructMat4:
    case EOpConstructStruct:

    case EOpLessThan:
    case EOpGreaterThan:
    case EOpLessThanEqual:
    case EOpGreaterThanEqual:
    case EOpVectorEqual:
    case EOpVectorNotEqual:

    case EOpMod:
    case EOpPow:

    case EOpAtan:

    case EOpMin:
    case EOpMax:
    case EOpClamp:
    case EOpMix:
    case EOpStep:
    case EOpSmoothStep:

    case EOpDistance:
    case EOpDot:
    case EOpCross:
    case EOpFaceForward:
    case EOpReflect:
    case EOpRefract:
    case EOpMul:

    default:
        gla::UnsupportedFunctionality("glslang aggregate", gla::EATContinue);
        return false;
    }

    return true;
}

bool TranslateSelection(bool /* preVisit */, TIntermSelection* node, TIntermTraverser* it)
{
    TGlslangToTopTraverser* oit = static_cast<TGlslangToTopTraverser*>(it);

    gla::UnsupportedFunctionality("glslang selection", gla::EATContinue);
    return false;

    node->getCondition()->traverse(it);

    if (node->getTrueBlock()) {
		node->getTrueBlock()->traverse(it);
	}

    if (node->getFalseBlock()) {
        node->getFalseBlock()->traverse(it);
    }

    return false;
}

void TranslateConstantUnion(TIntermConstantUnion* node, TIntermTraverser* it)
{
    TGlslangToTopTraverser* oit = static_cast<TGlslangToTopTraverser*>(it);

    int size = node->getType().getObjectSize();

    int nextConst = 0;
    llvm::Value* c = oit->createLLVMConstant(node->getType(), node->getUnionArrayPointer(), nextConst);
    oit->glaBuilder->clearAccessChain();
    oit->glaBuilder->setAccessChainRValue(c);
}

bool TranslateLoop(bool /* preVisit */, TIntermLoop* node, TIntermTraverser* it)
{
    TGlslangToTopTraverser* oit = static_cast<TGlslangToTopTraverser*>(it);

    gla::UnsupportedFunctionality("glslang loops", gla::EATContinue);
    return false;

    if (! node->testFirst())
        ;

    if (node->getTest()) {
        node->getTest()->traverse(it);
    }

    if (node->getBody()) {
        node->getBody()->traverse(it);
    }

    if (node->getTerminal()) {
        node->getTerminal()->traverse(it);
    }

    return false;
}

bool TranslateBranch(bool previsit, TIntermBranch* node, TIntermTraverser* it)
{
    TGlslangToTopTraverser* oit = static_cast<TGlslangToTopTraverser*>(it);

    if (node->getExpression())
        node->getExpression()->traverse(it);

    switch (node->getFlowOp()) {
    case EOpKill:
        oit->glaBuilder->makeDiscard(oit->inMain);
        break;
    case EOpBreak:
        oit->glaBuilder->makeLoopExit();
        break;
    case EOpContinue:
        oit->glaBuilder->makeLoopBackEdge();
        break;
    case EOpReturn:
        if (oit->inMain)
            oit->glaBuilder->makeMainReturn();
        else if (node->getExpression()) {
            // this path still needs to be tested/corrected (where does the value come from?)
            gla::UnsupportedFunctionality("glslang qualifier const", gla::EATContinue);
            oit->glaBuilder->makeReturn(false, oit->glaBuilder->accessChainLoad());
        } else
            oit->glaBuilder->makeReturn();

        oit->glaBuilder->clearAccessChain();
        break;

    default:
        ;
    }

    return false;
}

gla::Builder::SuperValue TGlslangToTopTraverser::createLLVMVariable(TIntermSymbol* node)
{
    llvm::Constant* initializer = 0;
    gla::Builder::EStorageQualifier storageQualifier;
    int constantBuffer = 0;

    switch (node->getQualifier()) {
    case EvqTemporary:
        storageQualifier = gla::Builder::ESQLocal;
        break;
    case EvqGlobal:
        storageQualifier = gla::Builder::ESQGlobal;
        break;
    case EvqConst:
        gla::UnsupportedFunctionality("glslang const variable", gla::EATContinue);
        break;
    case EvqAttribute:
    case EvqVaryingIn:
    case EvqFragCoord:
    case EvqFace:
        // inputs should all be pipeline reads or created at function creation time
        assert(! "no variable creations for inputs");
        break;
    case EvqVaryingOut:
    case EvqPosition:
    case EvqPointSize:
    case EvqClipVertex:
    case EvqFragColor:
    case EvqFragDepth:
        storageQualifier = gla::Builder::ESQOutput;
        break;
    case EvqUniform:
        storageQualifier = gla::Builder::ESQUniform;
        // TODO: need to generalize to N objects (constant buffers) for higher shader models
        constantBuffer = 0;
        break;
    case EvqIn:
    case EvqOut:
    case EvqInOut:
    case EvqConstReadOnly:

    default:
        gla::UnsupportedFunctionality("glslang qualifier", gla::EATContinue);
    }

    std::string* annotationAddr = 0;
    std::string annotation;
    if (IsSampler(node->getBasicType())) {
        annotation = TType::getBasicString(node->getBasicType());
        annotationAddr = &annotation;
        storageQualifier = gla::Builder::ESQResource;
    }

    const llvm::Type *llvmType = convertGlslangToGlaType(node->getType());

    return glaBuilder->createVariable(storageQualifier, constantBuffer, llvmType, node->getType().isMatrix(),
                                      initializer, annotationAddr, node->getSymbol().c_str());
}

const llvm::Type* TGlslangToTopTraverser::convertGlslangToGlaType(TType& type)
{
    const llvm::Type *glaType;

    switch(type.getBasicType()) {
    case EbtVoid:
        glaType = gla::GetVoidType(context);
        break;
    case EbtFloat:
        glaType = gla::GetFloatType(context);
        break;
    case EbtDouble:
        gla::UnsupportedFunctionality("basic type: double", gla::EATContinue);
        break;
    case EbtBool:
        glaType = gla::GetBoolType(context);
        break;
    case EbtInt:
    case EbtSampler1D:
    case EbtSampler2D:
    case EbtSampler3D:
    case EbtSamplerCube:
    case EbtSampler1DShadow:
    case EbtSampler2DShadow:
    case EbtSamplerRect:
    case EbtSamplerRectShadow:
        glaType = gla::GetIntType(context);
        break;
    case EbtStruct:
        gla::UnsupportedFunctionality("basic type: struct", gla::EATContinue);
        //{
        //    std::vector<const llvm::Type*> structFields;
        //    llvm::StructType* structType = structMap[type->name];
        //    if (structType) {
        //        // If we've seen this struct type, return it
        //        glaType = structType;
        //    } else {
        //        // Create a vector of struct types for LLVM to consume
        //        for (int i = 0; i < type->length; i++) {
        //            structFields.push_back(convertGlslangToGlaType(type->fields.structure[i].type));
        //        }
        //        structType = llvm::StructType::get(context, structFields, false);
        //        module->addTypeName(type->name, structType);
        //        structMap[type->name] = structType;
        //        glaType = structType;
        //    }
        //}
        break;

    default:
        gla::UnsupportedFunctionality("basic type", gla::EATContinue);
        break;
    }

    if (type.isMatrix())
        glaType = gla::Builder::Matrix::getType(glaType, type.getNominalSize(), type.getNominalSize());
    else {
        // If this variable has a vector element count greater than 1, create an LLVM vector
        if (type.getNominalSize() > 1)
            glaType = llvm::VectorType::get(glaType, type.getNominalSize());
    }

    if (type.isArray())
        glaType = llvm::ArrayType::get(glaType, type.getArraySize());

    return glaType;
}

gla::Builder::SuperValue TGlslangToTopTraverser::createBinaryOperation(TOperator op, gla::Builder::SuperValue left, gla::Builder::SuperValue right, bool isFloat, bool isSigned)
{
    gla::Builder::SuperValue result;
    llvm::Instruction::BinaryOps binOp = llvm::Instruction::BinaryOps(0);
    bool needsPromotion = true;

    switch(op) {
    case EOpAdd:
        if (isFloat)
            binOp = llvm::Instruction::FAdd;
        else
            binOp = llvm::Instruction::Add;
        break;
    case EOpSub:
        if (isFloat)
            binOp = llvm::Instruction::FSub;
        else
            binOp = llvm::Instruction::Sub;
        break;
    case EOpMul:
        if (isFloat)
            binOp = llvm::Instruction::FMul;
        else
            binOp = llvm::Instruction::Mul;
        break;
    case EOpDiv:
        if (isFloat)
            binOp = llvm::Instruction::FDiv;
        else if (isSigned)
            binOp = llvm::Instruction::SDiv;
        else
            binOp = llvm::Instruction::UDiv;
        break;
    case EOpMod:
        if (isFloat)
            binOp = llvm::Instruction::FRem;
        else if (isSigned)
            binOp = llvm::Instruction::SRem;
        else
            binOp = llvm::Instruction::URem;
        break;
    case EOpRightShift:
        binOp = llvm::Instruction::LShr;
        break;
    case EOpLeftShift:
        binOp = llvm::Instruction::Shl;
        break;
    case EOpAnd:
        binOp = llvm::Instruction::And;
        break;
    case EOpInclusiveOr:
        binOp = llvm::Instruction::Or;
        break;
    case EOpExclusiveOr:
    case EOpLogicalXor:
        binOp = llvm::Instruction::Xor;
        break;
    case EOpLogicalAnd:
        assert(gla::IsBoolean(left->getType()) && gla::IsScalar(left->getType()));
        assert(gla::IsBoolean(right->getType()) && gla::IsScalar(right->getType()));
        needsPromotion = false;
        binOp = llvm::Instruction::And;
        break;
    }

    if (binOp != 0) {
        if (needsPromotion)
            glaBuilder->promoteScalar(left, right);

        return llvmBuilder.CreateBinOp(binOp, left, right);
    }

    // Comparison instructions
    if (isFloat) {
        llvm::FCmpInst::Predicate pred = llvm::FCmpInst::Predicate(0);
        switch (op) {
        case EOpLessThan:
            pred = llvm::FCmpInst::FCMP_OLT;
            break;
        case EOpGreaterThan:
            pred = llvm::FCmpInst::FCMP_OGT;
            break;
        case EOpLessThanEqual:
            pred = llvm::FCmpInst::FCMP_OLE;
            break;
        case EOpGreaterThanEqual:
            pred = llvm::FCmpInst::FCMP_OGE;
            break;
        case EOpEqual:
            pred = llvm::FCmpInst::FCMP_OEQ;
            break;
        case EOpNotEqual:
            pred = llvm::FCmpInst::FCMP_ONE;
            break;
        }

        if (pred != 0)
            return llvmBuilder.CreateFCmp(pred, left, right);
    } else {
        llvm::ICmpInst::Predicate pred = llvm::ICmpInst::Predicate(0);
        if (isSigned) {
            switch (op) {
            case EOpLessThan:
                pred = llvm::ICmpInst::ICMP_SLT;
                break;
            case EOpGreaterThan:
                pred = llvm::ICmpInst::ICMP_SGT;
                break;
            case EOpLessThanEqual:
                pred = llvm::ICmpInst::ICMP_SLE;
                break;
            case EOpGreaterThanEqual:
                pred = llvm::ICmpInst::ICMP_SGE;
                break;
            case EOpEqual:
                pred = llvm::ICmpInst::ICMP_EQ;
                break;
            case EOpNotEqual:
                pred = llvm::ICmpInst::ICMP_NE;
                break;
            }
        } else {
            switch (op) {
            case EOpLessThan:
                pred = llvm::ICmpInst::ICMP_ULT;
                break;
            case EOpGreaterThan:
                pred = llvm::ICmpInst::ICMP_UGT;
                break;
            case EOpLessThanEqual:
                pred = llvm::ICmpInst::ICMP_ULE;
                break;
            case EOpGreaterThanEqual:
                pred = llvm::ICmpInst::ICMP_UGE;
                break;
            case EOpEqual:
                pred = llvm::ICmpInst::ICMP_EQ;
                break;
            case EOpNotEqual:
                pred = llvm::ICmpInst::ICMP_NE;
                break;
            }
        }

        if (pred != 0)
            return llvmBuilder.CreateICmp(pred, left, right);
    }

    return result;
}

llvm::Value* TGlslangToTopTraverser::createPipelineRead(TIntermSymbol* node, int slot)
{
    // For pipeline inputs, and we will generate a fresh pipeline read at each reference,
    // which we will optimize later.
    std::string name(node->getSymbol().c_str());
    const llvm::Type* readType;

    if (node->getType().isArray()) {
        gla::UnsupportedFunctionality("input array");
        return 0;
    } else {
        readType = convertGlslangToGlaType(node->getType());
    }

    gla::EInterpolationMethod method = gla::EIMSmooth;
    // TODO: set interpolation types
            //method = gla::EIMSmooth;
            //method = gla::EIMNoperspective;
            //method = gla::EIMNone;

    // Give each interpolant a temporary unique index
    return glaBuilder->readPipeline(readType, name, getNextInterpIndex(name), -1 /*mask*/, method);
}

int TGlslangToTopTraverser::getNextInterpIndex(std::string& name)
{
    // Get the index for this interpolant, or create a new unique one
    std::map<std::string, int>::iterator iter;
    iter = interpMap.find(name);

    if (interpMap.end() == iter) {
        interpMap[name] = interpIndex++;
    }

    return interpMap[name];
}

llvm::Constant* TGlslangToTopTraverser::createLLVMConstant(TType& glslangType, constUnion *consts, int& nextConst)
{
    // vector of constants for LLVM
    std::vector<llvm::Constant*> vals;

    // Type is used for struct and array constants
    const llvm::Type* type = convertGlslangToGlaType(glslangType);

    if (glslangType.isArray()) {
        TType nonArrayType = glslangType;
        nonArrayType.clearArrayness();
        for (int i = 0; i < glslangType.getArraySize(); ++i)
            vals.push_back(createLLVMConstant(nonArrayType, consts, nextConst));
    } else if (glslangType.isMatrix()) {
        gla::UnsupportedFunctionality("Matrix constants");
    } else if (glslangType.getStruct()) {
        TVector<TTypeLine>::iterator iter;
        for (iter = glslangType.getStruct()->begin(); iter != glslangType.getStruct()->end(); ++iter)
            vals.push_back(createLLVMConstant(*iter->type, consts, nextConst));
    } else {
        // a vector or scalar, both will work the same way
        // this is where we actually consume the constants, rather than walk a tree

        for (unsigned int i = 0; i < glslangType.getNominalSize(); ++i) {
            switch(consts->getType())
            {
            case EbtInt:
                vals.push_back(gla::MakeUnsignedConstant(context, consts[nextConst].getIConst()));
                break;
            case EbtFloat:
                vals.push_back(gla::MakeFloatConstant(context, consts[nextConst].getFConst()));
                break;
            case EbtDouble:
                vals.push_back(gla::MakeFloatConstant(context, consts[nextConst].getDConst()));
                break;
            case EbtBool:
                vals.push_back(gla::MakeBoolConstant(context, consts[nextConst].getBConst()));
                break;
            default:
                gla::UnsupportedFunctionality("scalar or vector element type");
                break;
            }
            ++nextConst;
        }
    }
    
    return glaBuilder->getConstant(vals, type);
}

//
// Set up the glslang traversal
//
void GlslangToTop(TIntermNode* root, gla::Manager* manager)
{
    if (root == 0)
        return;

    TGlslangToTopTraverser it(manager);

    it.visitAggregate = TranslateAggregate;
    it.visitBinary = TranslateBinary;
    it.visitConstantUnion = TranslateConstantUnion;
    it.visitSelection = TranslateSelection;
    it.visitSymbol = TranslateSymbol;
    it.visitUnary = TranslateUnary;
    it.visitLoop = TranslateLoop;
    it.visitBranch = TranslateBranch;

    root->traverse(&it);
}
