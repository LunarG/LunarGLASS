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
    const llvm::Type* convertGlslangToGlaType(const TType& type);

    void handleFunctionEntry(TIntermAggregate* node);
    void translateArguments(TIntermSequence& glslangArguments, std::vector<gla::Builder::SuperValue>& arguments);
    gla::Builder::SuperValue handleBuiltinFunctionCall(TIntermAggregate*);
    gla::Builder::SuperValue handleUserFunctionCall(TIntermAggregate*);

    gla::Builder::SuperValue createBinaryOperation(TOperator op, gla::Builder::SuperValue left, gla::Builder::SuperValue right);
    gla::Builder::SuperValue createUnaryOperation(TOperator op, const TType& destType, gla::Builder::SuperValue operand);
    gla::Builder::SuperValue createUnaryIntrinsic(TOperator op, gla::Builder::SuperValue operand, TBasicType);
    gla::Builder::SuperValue createIntrinsic(TOperator op, std::vector<gla::Builder::SuperValue>& operands, TBasicType);
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
    std::map<std::string, llvm::Function*> functionMap;
    std::map<std::string, int> interpMap;
    std::map<TTypeList*, llvm::StructType*> structMap;
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

    llvm::Instruction::BinaryOps binOp = llvm::Instruction::BinaryOps(0);
    bool needsPromotion = true;

    switch (node->getOp()) {
    case EOpAssign:
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
        // A bin-op assign "a += b" means the same thing as "a = a + b"
        // where a is evaluated before b. For a simple assignment, GLSL
        // says to evaluate the left before the right.  So, always, left
        // node then right node.
        {
            // get the left l-value, save it away
            oit->glaBuilder->clearAccessChain();
            node->getLeft()->traverse(oit);
            gla::Builder::AccessChain lValue = oit->glaBuilder->getAccessChain();

            // evaluate the right
            oit->glaBuilder->clearAccessChain();
            node->getRight()->traverse(oit);
            gla::Builder::SuperValue rValue = oit->glaBuilder->accessChainLoad();

            if (node->getOp() != EOpAssign) {
                // the left is also an r-value
                oit->glaBuilder->setAccessChain(lValue);
                gla::Builder::SuperValue leftRValue = oit->glaBuilder->accessChainLoad();

                // do the operation
                rValue = oit->createBinaryOperation(node->getOp(), leftRValue, rValue);
                if (rValue.isClear()) {
                    switch (node->getOp()) {
                    case EOpVectorTimesMatrixAssign:
                    case EOpMatrixTimesScalarAssign:
                    case EOpMatrixTimesMatrixAssign:
                        gla::UnsupportedFunctionality("matrix op-assign");
                        break;
                    default:
                        gla::UnsupportedFunctionality("unknown op-assign");
                    }
                }
            }

            // store the result
            oit->glaBuilder->setAccessChain(lValue);
            oit->glaBuilder->accessChainStore(rValue);

            // assignments are expressions having an rValue after they are evaluated...
            oit->glaBuilder->clearAccessChain();
            oit->glaBuilder->setAccessChainRValue(rValue);
            return false;
        }

    case EOpIndexDirect:
    case EOpIndexIndirect:
    case EOpIndexDirectStruct:
        {
            // this adapter is building access chains left to right
            // set up the access chain to the left
            node->getLeft()->traverse(oit);

            if (! node->getLeft()->getType().isArray() &&
                  node->getLeft()->getType().isVector() &&
                  node->getOp() == EOpIndexDirect) {
                // this is essentially a hard-coded vector swizzle of size 1,
                // so short circuit the GEP stuff with a swizzle
                std::vector<int> swizzle;
                swizzle.push_back(node->getRight()->getAsConstantUnion()->getUnionArrayPointer()->getIConst());
                oit->glaBuilder->accessChainPushSwizzleRight(swizzle, oit->convertGlslangToGlaType(node->getType()),
                                                             node->getLeft()->getNominalSize());
            } else if (node->getLeft()->getType().isMatrix()) {
                gla::UnsupportedFunctionality("matrix indexing");
            } else {
                // struct or array or indirection into a vector; will use native LLVM gep

                // save it so that computing the right side doesn't trash it
                gla::Builder::AccessChain partial = oit->glaBuilder->getAccessChain();

                // compute the next index
                oit->glaBuilder->clearAccessChain();
                node->getRight()->traverse(oit);
                gla::Builder::SuperValue index = oit->glaBuilder->accessChainLoad();

                // make the new access chain to date
                oit->glaBuilder->setAccessChain(partial);
                oit->glaBuilder->accessChainPushLeft(index);
            }

            return false;
        }

    case EOpVectorSwizzle:
        {
            node->getLeft()->traverse(oit);
            TIntermSequence& swizzleSequence = node->getRight()->getAsAggregate()->getSequence();
            std::vector<int> swizzle;
            for (int i = 0; i < swizzleSequence.size(); ++i)
                swizzle.push_back(swizzleSequence[i]->getAsConstantUnion()->getUnionArrayPointer()->getIConst());
            oit->glaBuilder->accessChainPushSwizzleRight(swizzle, oit->convertGlslangToGlaType(node->getType()),
                                                         node->getLeft()->getNominalSize());
            return false;
        }

    case EOpVectorTimesMatrix:
    case EOpMatrixTimesVector:
    case EOpMatrixTimesScalar:
    case EOpMatrixTimesMatrix:
    //case EOpOuterProduct:
    //    return glaBuilder->createMatrixMultiply(left, right);

        gla::UnsupportedFunctionality("glslang binary matrix", gla::EATContinue);
        return true;
    }

    // Assume generic binary op...
    oit->glaBuilder->clearAccessChain();
    node->getLeft()->traverse(oit);
    gla::Builder::SuperValue left = oit->glaBuilder->accessChainLoad();
    oit->glaBuilder->clearAccessChain();
    node->getRight()->traverse(oit);
    gla::Builder::SuperValue right = oit->glaBuilder->accessChainLoad();

    gla::Builder::SuperValue rValue;
    if (left.isMatrix() || right.isMatrix())
        gla::UnsupportedFunctionality("glslang matrix operation", gla::EATContinue);
    else
        rValue = oit->createBinaryOperation(node->getOp(), left, right);

    if (rValue.isClear()) {
        gla::UnsupportedFunctionality("glslang binary operation", gla::EATContinue);

        return true;
    } else {
        oit->glaBuilder->clearAccessChain();
        oit->glaBuilder->setAccessChainRValue(rValue);

        return false;
    }
}

bool TranslateUnary(bool /* preVisit */, TIntermUnary* node, TIntermTraverser* it)
{
    TGlslangToTopTraverser* oit = static_cast<TGlslangToTopTraverser*>(it);

    oit->glaBuilder->clearAccessChain();
    node->getOperand()->traverse(oit);
    gla::Builder::SuperValue operand = oit->glaBuilder->accessChainLoad();

    gla::Builder::SuperValue result = oit->createUnaryOperation(node->getOp(), node->getType(), operand);

    // it could be a LunarGLASS intrinsic instead of an operation
    if (result.isClear())
        result = oit->createUnaryIntrinsic(node->getOp(), operand, node->getBasicType());

    if (! result.isClear()) {
        oit->glaBuilder->clearAccessChain();
        oit->glaBuilder->setAccessChainRValue(result);
        return false; // done with this node
    }

    switch (node->getOp()) {
    case EOpPostIncrement:
    case EOpPostDecrement:
    case EOpPreIncrement:
    case EOpPreDecrement:
        gla::UnsupportedFunctionality("++/--", gla::EATContinue);
        return false;

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
            } else {
                oit->handleFunctionEntry(node);
            }
        } else {
            oit->glaBuilder->leaveFunction(oit->inMain);
            oit->inMain = false;
            oit->llvmBuilder.SetInsertPoint(oit->shaderEntry);
        }
        return true;

    case EOpParameters:
        // Parameters will have been consumed by EOpFunction processing, but not
        // the body, so we still visited the function node's children, making this
        // child redundant.
        return false;

    case EOpFunctionCall:
        {
            gla::Builder::SuperValue result;

            if (node->isUserDefined())
                result = oit->handleUserFunctionCall(node);
            else
                result = oit->handleBuiltinFunctionCall(node);

            if (result.isClear())
                gla::UnsupportedFunctionality("glslang function call");
            else {
                oit->glaBuilder->clearAccessChain();
                oit->glaBuilder->setAccessChainRValue(result);
            }

            return false;
        }

    case EOpConstructFloat:
    case EOpConstructVec2:
    case EOpConstructVec3:
    case EOpConstructVec4:
    case EOpConstructDouble:
    //case EOpConstructDvec2:
    //case EOpConstructDvec3:
    //case EOpConstructDvec4:
    case EOpConstructBool:
    case EOpConstructBVec2:
    case EOpConstructBVec3:
    case EOpConstructBVec4:
    case EOpConstructInt:
    case EOpConstructIVec2:
    case EOpConstructIVec3:
    case EOpConstructIVec4:
    case EOpConstructStruct:
        {
            std::vector<gla::Builder::SuperValue> arguments;
            oit->translateArguments(node->getSequence(), arguments);
            llvm::Value* constructed = oit->glaBuilder->createVariable(gla::Builder::ESQLocal, 0,
                                                                       oit->convertGlslangToGlaType(node->getType()),
                                                                       false, 0, 0, "constructed");
            if (node->getOp() == EOpConstructStruct) {
                //TODO: is there a more direct way to set a whole LLVM structure?
                //TODO: if not, move this inside Top Builder; too many indirections

                std::vector<llvm::Value*> gepChain;
                gepChain.push_back(gla::MakeIntConstant(oit->context, 0));
                for (int field = 0; field < arguments.size(); ++field) {
                    gepChain.push_back(gla::MakeIntConstant(oit->context, field));
                    llvm::Value* loadVal = oit->llvmBuilder.CreateStore(arguments[field],
                                                                        oit->glaBuilder->createGEP(constructed, gepChain));
                    gepChain.pop_back();
                }
                constructed = oit->llvmBuilder.CreateLoad(constructed);
            } else {
                constructed = oit->llvmBuilder.CreateLoad(constructed);
                constructed = oit->glaBuilder->createConstructor(arguments, constructed);
            }
            oit->glaBuilder->clearAccessChain();
            oit->glaBuilder->setAccessChainRValue(constructed);
            return false;
        }

    case EOpConstructMat2:
    case EOpConstructMat3:
    case EOpConstructMat4:
        gla::UnsupportedFunctionality("matrix constructor");
        return false;

    case EOpLessThan:
    case EOpGreaterThan:
    case EOpLessThanEqual:
    case EOpGreaterThanEqual:
    case EOpVectorEqual:
    case EOpVectorNotEqual:
        gla::UnsupportedFunctionality("aggregate comparison");
        return false;

    //case EOpRecip:
    //    return glaBuilder->createRecip(operand);

    case EOpArrayLength:
        gla::UnsupportedFunctionality("glsang array length");
        return false;
    }

    //
    // See if it maps to a regular operation or intrinsic.
    //

    TIntermSequence& glslangOperands = node->getSequence();
    std::vector<gla::Builder::SuperValue> operands;
    gla::Builder::SuperValue result;
    for (int i = 0; i < glslangOperands.size(); ++i) {
        oit->glaBuilder->clearAccessChain();
        glslangOperands[i]->traverse(oit);
        operands.push_back(oit->glaBuilder->accessChainLoad());
    }
    if (glslangOperands.size() == 1)
        result = oit->createUnaryIntrinsic(node->getOp(), operands.front(), glslangOperands[0]->getAsTyped()->getBasicType());
    else
        result = oit->createIntrinsic(node->getOp(), operands, glslangOperands[0]->getAsTyped()->getBasicType());

    if (result.isClear())
        gla::UnsupportedFunctionality("glsang aggregate");
    else {
        oit->glaBuilder->clearAccessChain();
        oit->glaBuilder->setAccessChainRValue(result);
    }

    return false;
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

const llvm::Type* TGlslangToTopTraverser::convertGlslangToGlaType(const TType& type)
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
//        gla::UnsupportedFunctionality("basic type: struct");
        {
            TTypeList* glslangStruct = type.getStruct();
            std::vector<const llvm::Type*> structFields;
            llvm::StructType* structType = structMap[glslangStruct];
            if (structType) {
                // If we've seen this struct type, return it
                glaType = structType;
            } else {
                // Create a vector of struct types for LLVM to consume
                for (int i = 0; i < glslangStruct->size(); i++)
                    structFields.push_back(convertGlslangToGlaType(*(*glslangStruct)[i].type));
                structType = llvm::StructType::get(context, structFields, false);
                module->addTypeName(type.getTypeName().c_str(), structType);
                structMap[glslangStruct] = structType;
                glaType = structType;
            }
        }
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

void TGlslangToTopTraverser::handleFunctionEntry(TIntermAggregate* node)
{
    std::vector<const llvm::Type*> paramTypes;
    TIntermSequence& parameters = node->getSequence()[0]->getAsAggregate()->getSequence();

    // At call time, space should be allocated for all the arguments,
    // and pointers to that space passed to the function as the formal parameters.
    for (int i = 0; i < parameters.size(); ++i) {
        const llvm::Type* type = convertGlslangToGlaType(parameters[i]->getAsTyped()->getType());        
        paramTypes.push_back(llvm::PointerType::get(type, gla::GlobalAddressSpace));
    }

    llvm::BasicBlock* functionBlock;
    llvm::Function *function = glaBuilder->makeFunctionEntry(convertGlslangToGlaType(node->getType()), node->getName().c_str(),
                                                             paramTypes, &functionBlock);
    function->addFnAttr(llvm::Attribute::AlwaysInline);
    llvmBuilder.SetInsertPoint(functionBlock);

    // Visit parameter list again to create mappings to local variables and set attributes.
    llvm::Function::arg_iterator arg = function->arg_begin();
    for (int i = 0; i < parameters.size(); ++i, ++arg)
        namedValues[parameters[i]->getAsSymbolNode()->getId()] = &(*arg);

    // Track our user function to call later
    functionMap[node->getName().c_str()] = function;
}

void TGlslangToTopTraverser::translateArguments(TIntermSequence& glslangArguments, std::vector<gla::Builder::SuperValue>& arguments)
{
    for (int i = 0; i < glslangArguments.size(); ++i) {
        glaBuilder->clearAccessChain();
        glslangArguments[i]->traverse(this);
        arguments.push_back(glaBuilder->accessChainLoad());
    }
}

gla::Builder::SuperValue TGlslangToTopTraverser::handleBuiltinFunctionCall(TIntermAggregate* node)
{
    std::vector<gla::Builder::SuperValue> arguments;
    translateArguments(node->getSequence(), arguments);

    gla::Builder::SuperValue result;
    llvm::Intrinsic::ID intrinsicID = llvm::Intrinsic::ID(0);

    if (node->getName() == "ftransform(") {
        gla::Builder::SuperValue vertex; // TODO: simulate access to gl_Vertex
        gla::Builder::SuperValue matrix; // TODO: simulate access to gl_ModelViewProjectionMatrix
        gla::UnsupportedFunctionality("ftransform");

        return glaBuilder->createIntrinsicCall(llvm::Intrinsic::gla_fFixedTransform, vertex, matrix);
    }

    if (node->getName().substr(0, 7) == "texture") {
        intrinsicID = llvm::Intrinsic::gla_fTextureSample;
        gla::Builder::TextureParameters params = {};
        int texFlags = 0;
        params.ETPSampler = arguments[0];
        params.ETPCoords = arguments[1];

        if (node->getName().find("Lod", 0) != std::string::npos) {
            texFlags |= gla::ETFLod;
            params.ETPBiasLod = arguments[2];
        }

        // TODO: flesh all this out after glslang has modern texturing functions

        if (node->getName().find("Proj", 0) != std::string::npos)
            texFlags |= gla::ETFProjected;

        return glaBuilder->createTextureCall(convertGlslangToGlaType(node->getType()), gla::ESampler2D, texFlags, params);
    }

    return result;
}

gla::Builder::SuperValue TGlslangToTopTraverser::handleUserFunctionCall(TIntermAggregate* node)
{
    // Overall design is to pass pointers to the arguments, as described:
    //
    // For input arguments, they could be expressions, and their value could be
    // overwritten without impacting anything in the caller, so store the answer
    // and pass a pointer to it.
    //
    // For output arguments, there could still be a conversion needed, so
    // so make space for the answer, and convert in before sticking it into
    // the original l-value provide.  (Pass the pointer to the space made.)
    //
    // For inout, just do both the above, but using a single space/pointer
    // to do it.
    //

    // Grab the pointer from the previously created function
    llvm::Function* function = functionMap[node->getName().c_str()];

    // First step:  Allocate the space for the arguments and build llvm
    // pointers to it as the passed in arguments.
    llvm::SmallVector<llvm::Value*, 4> llvmArgs;
    llvm::Function::arg_iterator param;
    llvm::Function::arg_iterator end = function->arg_end();
    for (param = function->arg_begin(); param != end; ++param) {
        // param->getType() should be a pointer, we need the type it points to
        llvm::Value* space = glaBuilder->createVariable(gla::Builder::ESQLocal, 0, param->getType()->getContainedType(0), 0, 0, 0, "param");
        llvmArgs.push_back(space);
    }

    // Copy-in time...
    // Compute the access chains of output argument l-values before making the call, 
    // to be used after making the call.  Also compute r-values of inputs and store
    // them into the space allocated above.
    TIntermSequence& glslangArgs = node->getSequence();
    TQualifierList& qualifiers = node->getQualifier();
    llvm::SmallVector<gla::Builder::AccessChain, 2> lValuesOut;
    for (int i = 0; i < glslangArgs.size(); ++i) {
        // build l-value
        glaBuilder->clearAccessChain();
        glslangArgs[i]->traverse(this);
        if (qualifiers[i] == EvqOut || qualifiers[i] == EvqInOut) {
            // save l-value
            lValuesOut.push_back(glaBuilder->getAccessChain());
        }
        if (qualifiers[i] == EvqIn || qualifiers[i] == EvqConstReadOnly || qualifiers[i] == EvqInOut) {
            // process r-value
            glaBuilder->createStore(glaBuilder->accessChainLoad(), llvmArgs[i]);
        }
    }

    gla::Builder::SuperValue result = llvmBuilder.Insert(llvm::CallInst::Create(function, llvmArgs.begin(), llvmArgs.end()));

    // Copy-out time...
    // Convert outputs to correct type before storing into the l-value
    llvm::SmallVector<gla::Builder::AccessChain, 2>::iterator savedIt = lValuesOut.begin();
    for (int i = 0; i < glslangArgs.size(); ++i) {
        if (qualifiers[i] == EvqOut || qualifiers[i] == EvqInOut) {
            glaBuilder->setAccessChain(*savedIt);
            llvm::Value* output = glaBuilder->createLoad(llvmArgs[i]);
            if (convertGlslangToGlaType(glslangArgs[i]->getAsTyped()->getType()) != llvmArgs[i]->getType()->getContainedType(0))
                gla::UnsupportedFunctionality("conversion of function call output parameter to different type");
            glaBuilder->accessChainStore(output);
            ++savedIt;
        }
    }

    return result;
}

gla::Builder::SuperValue TGlslangToTopTraverser::createBinaryOperation(TOperator op, gla::Builder::SuperValue left, gla::Builder::SuperValue right)
{
    gla::Builder::SuperValue result;
    llvm::Instruction::BinaryOps binOp = llvm::Instruction::BinaryOps(0);
    bool needsPromotion = true;
    bool leftIsFloat = (gla::GetBasicTypeID(left) == llvm::Type::FloatTyID);
    bool isSigned = true;

    switch(op) {
    case EOpAdd:
    case EOpAddAssign:
        if (leftIsFloat)
            binOp = llvm::Instruction::FAdd;
        else
            binOp = llvm::Instruction::Add;
        break;
    case EOpSub:
    case EOpSubAssign:
        if (leftIsFloat)
            binOp = llvm::Instruction::FSub;
        else
            binOp = llvm::Instruction::Sub;
        break;
    case EOpMul:
    case EOpMulAssign:
    case EOpVectorTimesScalar:
    case EOpVectorTimesScalarAssign:
        if (leftIsFloat)
            binOp = llvm::Instruction::FMul;
        else
            binOp = llvm::Instruction::Mul;
        break;
    case EOpDiv:
    case EOpDivAssign:
        if (leftIsFloat)
            binOp = llvm::Instruction::FDiv;
        else if (isSigned)
            binOp = llvm::Instruction::SDiv;
        else
            binOp = llvm::Instruction::UDiv;
        break;
    case EOpMod:
    case EOpModAssign:
        if (leftIsFloat)
            binOp = llvm::Instruction::FRem;
        else if (isSigned)
            binOp = llvm::Instruction::SRem;
        else
            binOp = llvm::Instruction::URem;
        break;
    case EOpRightShift:
    case EOpRightShiftAssign:
        binOp = llvm::Instruction::LShr;
        break;
    case EOpLeftShift:
    case EOpLeftShiftAssign:
        binOp = llvm::Instruction::Shl;
        break;
    case EOpAnd:
    case EOpAndAssign:
        binOp = llvm::Instruction::And;
        break;
    case EOpInclusiveOr:
    case EOpInclusiveOrAssign:
    case EOpLogicalOr:
        binOp = llvm::Instruction::Or;
        break;
    case EOpExclusiveOr:
    case EOpExclusiveOrAssign:
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
    if (leftIsFloat) {
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

gla::Builder::SuperValue TGlslangToTopTraverser::createUnaryOperation(TOperator op, const TType& destType, gla::Builder::SuperValue operand)
{
    gla::Builder::SuperValue result;

    // Unary ops that map to llvm operations
    switch (op) {
    case EOpNegative:
        if (destType.getBasicType() == EbtFloat)
            return llvmBuilder.CreateFNeg(operand);
        else
            return llvmBuilder.CreateNeg (operand);
    case EOpLogicalNot:
    case EOpVectorLogicalNot:
    case EOpBitwiseNot:
        return llvmBuilder.CreateNot(operand);
    }

    // Cast ops
    llvm::Instruction::CastOps castOp = llvm::Instruction::CastOps(0);
    switch(op) {
    case EOpConvFloatToInt:
        castOp = llvm::Instruction::FPToSI;
        break;
    case EOpConvIntToFloat:
        castOp = llvm::Instruction::SIToFP;
        break;
    case EOpConvFloatToBool:
        castOp = llvm::Instruction::FPToUI;
        break;
    case EOpConvBoolToFloat:
        castOp = llvm::Instruction::UIToFP;
        break;
    case EOpConvIntToBool:
        // any non-zero integer should return true
        return createBinaryOperation(EOpNotEqual, operand, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0));
    case EOpConvBoolToInt:
        castOp = llvm::Instruction::ZExt;
        break;
    //case EOpConvFloatToUnsigned:
    //    castOp = llvm::Instruction::UIToFP;
    //    break;
    //case EOpConvUnsignedToFloat:
    //    castOp = llvm::Instruction::UIToFP;
    //    break;
    }

    if (castOp != 0)
        return llvmBuilder.CreateCast(castOp, operand, convertGlslangToGlaType(destType));

    return result;
}

gla::Builder::SuperValue TGlslangToTopTraverser::createUnaryIntrinsic(TOperator op, gla::Builder::SuperValue operand, TBasicType basicType)
{
    // Unary ops that require an intrinsic
    gla::Builder::SuperValue result;
    llvm::Intrinsic::ID intrinsicID = llvm::Intrinsic::ID(0);

    switch(op) {
    case EOpRadians:
        intrinsicID = llvm::Intrinsic::gla_fRadians;
        break;
    case EOpDegrees:
        intrinsicID = llvm::Intrinsic::gla_fDegrees;
        break;

    case EOpSin:
        intrinsicID = llvm::Intrinsic::gla_fSin;
        break;
    case EOpCos:
        intrinsicID = llvm::Intrinsic::gla_fCos;
        break;
    case EOpTan:
        intrinsicID = llvm::Intrinsic::gla_fTan;
        break;
    case EOpAcos:
        intrinsicID = llvm::Intrinsic::gla_fAcos;
        break;
    case EOpAsin:
        intrinsicID = llvm::Intrinsic::gla_fAsin;
        break;
    case EOpAtan:
        intrinsicID = llvm::Intrinsic::gla_fAtan;
        break;

    //case EOpAcosh:
    //    intrinsicID = llvm::Intrinsic::gla_fAcosh;
    //    break;
    //case EOpAsinh:
    //    intrinsicID = llvm::Intrinsic::gla_fAsinh;
    //    break;
    //case EOpAtanh:
    //    intrinsicID = llvm::Intrinsic::gla_fAtanh;
    //    break;
    //case EOpTanh:
    //    intrinsicID = llvm::Intrinsic::gla_fTanh;
    //    break;
    //case EOpCosh:
    //    intrinsicID = llvm::Intrinsic::gla_fCosh;
    //    break;
    //case EOpSinh:
    //    intrinsicID = llvm::Intrinsic::gla_fSinh;
    //    break;

    case EOpLength:
        intrinsicID = llvm::Intrinsic::gla_fLength;
        break;
    case EOpNormalize:
        intrinsicID = llvm::Intrinsic::gla_fNormalize;
        break;

    case EOpExp:
        intrinsicID = llvm::Intrinsic::gla_fExp;
        break;
    case EOpLog:
        intrinsicID = llvm::Intrinsic::gla_fLog;
        break;
    case EOpExp2:
        intrinsicID = llvm::Intrinsic::gla_fExp2;
        break;
    case EOpLog2:
        intrinsicID = llvm::Intrinsic::gla_fLog2;
        break;
    case EOpSqrt:
        intrinsicID = llvm::Intrinsic::gla_fSqrt;
        break;
    case EOpInverseSqrt:
        intrinsicID = llvm::Intrinsic::gla_fInverseSqrt;
        break;

    case EOpFloor:
        intrinsicID = llvm::Intrinsic::gla_fFloor;
        break;
    case EOpCeil:
        intrinsicID = llvm::Intrinsic::gla_fCeiling;
        break;
    case EOpFract:
        intrinsicID = llvm::Intrinsic::gla_fFraction;
        break;

    //case EOpRoundEven:
    //    intrinsicID = llvm::Intrinsic::gla_fRoundEven;
    //    break;
    //case EOpTrunc:
    //    intrinsicID = llvm::Intrinsic::gla_fRoundZero;
    //    break;

    case EOpDPdx:
        intrinsicID = llvm::Intrinsic::gla_fDFdx;
        break;
    case EOpDPdy:
        intrinsicID = llvm::Intrinsic::gla_fDFdy;
        break;
    case EOpFwidth:
        intrinsicID = llvm::Intrinsic::gla_fFilterWidth;
        break;

    case EOpAny:
        intrinsicID = llvm::Intrinsic::gla_any;
        break;
    case EOpAll:
        intrinsicID = llvm::Intrinsic::gla_all;
        break;

    case EOpAbs:
        if (basicType == EbtFloat)
            intrinsicID = llvm::Intrinsic::gla_fAbs;
        else
            intrinsicID = llvm::Intrinsic::gla_abs;
        break;
    case EOpSign:
        if (basicType == EbtFloat)
            intrinsicID = llvm::Intrinsic::gla_fSign;
        else
            gla::UnsupportedFunctionality("Integer sign()");
        break;
    }

    if (intrinsicID != 0)
        return glaBuilder->createIntrinsicCall(intrinsicID, operand);

    return result;
}

gla::Builder::SuperValue TGlslangToTopTraverser::createIntrinsic(TOperator op, std::vector<gla::Builder::SuperValue>& operands, TBasicType basicType)
{
    // Binary ops that require an intrinsic
    gla::Builder::SuperValue result;
    llvm::Intrinsic::ID intrinsicID = llvm::Intrinsic::ID(0);

    switch (op) {
    case EOpMin:
        if (basicType == EbtFloat)
            intrinsicID = llvm::Intrinsic::gla_fMin;
        else
            intrinsicID = llvm::Intrinsic::gla_sMin;
        break;
    case EOpMax:
        if (basicType == EbtFloat)
            intrinsicID = llvm::Intrinsic::gla_fMax;
        else
            intrinsicID = llvm::Intrinsic::gla_sMax;
        break;
    case EOpPow:
        if (basicType == EbtFloat)
            intrinsicID = llvm::Intrinsic::gla_fPow;
        else
            intrinsicID = llvm::Intrinsic::gla_fPowi;
        break;
    case EOpDot:
        switch (gla::GetComponentCount(operands[0])) {
        case 2:
            intrinsicID = llvm::Intrinsic::gla_fDot2;
            break;
        case 3:
            intrinsicID = llvm::Intrinsic::gla_fDot3;
            break;
        case 4:
            intrinsicID = llvm::Intrinsic::gla_fDot4;
            break;
        default:
            assert(! "bad component count for dot");
        }
        break;
    case EOpAtan:
        intrinsicID = llvm::Intrinsic::gla_fAtan2;
        break;

    case EOpClamp:
        intrinsicID = llvm::Intrinsic::gla_fClamp;
        break;
    case EOpMix:
        intrinsicID = llvm::Intrinsic::gla_fMix;
        break;
    case EOpStep:
        intrinsicID = llvm::Intrinsic::gla_fStep;
        break;
    case EOpSmoothStep:
        intrinsicID = llvm::Intrinsic::gla_fSmoothStep;
        break;

    case EOpDistance:
        intrinsicID = llvm::Intrinsic::gla_fDistance;
        break;
    case EOpCross:
        intrinsicID = llvm::Intrinsic::gla_fCross;
        break;
    case EOpFaceForward:
        intrinsicID = llvm::Intrinsic::gla_fFaceForward;
        break;
    case EOpReflect:
        intrinsicID = llvm::Intrinsic::gla_fReflect;
        break;
    case EOpRefract:
        intrinsicID = llvm::Intrinsic::gla_fRefract;
        break;
    case EOpMul:
        gla::UnsupportedFunctionality("matrix component multiply");
        break;
    case EOpMod:
        intrinsicID = llvm::Intrinsic::gla_fModF; // TODO: verify
        break;
    }

    // If intrinsic was assigned, then call the function and return
    if (intrinsicID != 0) {
        switch (operands.size()) {
        case 0:
            // ftransform only, which goes through the function call path
            gla::UnsupportedFunctionality("intrinsic with no arguments");
            break;
        case 1:
            // should all be handled by createUnaryIntrinsic
            assert(0);
            break;
        case 2:
            result = glaBuilder->createIntrinsicCall(intrinsicID, operands[0], operands[1]);
            break;
        case 3:
            result = glaBuilder->createIntrinsicCall(intrinsicID, operands[0], operands[1], operands[2]);
            break;
        default:
            gla::UnsupportedFunctionality("intrinsic with more than 3 operands");
        }
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
            switch(consts[nextConst].getType()) {
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
