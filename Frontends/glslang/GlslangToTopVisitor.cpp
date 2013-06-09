//===- GlslangToTop.cpp - Translate GLSL IR to LunarGLASS Top IR ---------===//
//
//Copyright (C) 2012-2013 LunarG, Inc.
//
//All rights reserved.
//
//Redistribution and use in source and binary forms, with or without
//modification, are permitted provided that the following conditions
//are met:
//
//    Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
//
//    Redistributions in binary form must reproduce the above
//    copyright notice, this list of conditions and the following
//    disclaimer in the documentation and/or other materials provided
//    with the distribution.
//
//    Neither the name of LunarG Inc. nor the names of its
//    contributors may be used to endorse or promote products derived
//    from this software without specific prior written permission.
//
//THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
//FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
//COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
//INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
//BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
//LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
//CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
//LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
//ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
//POSSIBILITY OF SUCH DAMAGE.
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
#include "Exceptions.h"
#include "TopBuilder.h"
#include "metadata.h"

// LLVM includes
#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Intrinsics.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/IRBuilder.h"
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

    llvm::Value* createLLVMVariable(TIntermSymbol* node);
    llvm::Type* convertGlslangToGlaType(const TType& type);

    bool isShaderEntrypoint(const TIntermAggregate* node);
    void makeFunctions(const TIntermSequence&);
    void handleFunctionEntry(TIntermAggregate* node);
    void translateArguments(TIntermSequence& glslangArguments, std::vector<llvm::Value*>& arguments);
    llvm::Value* handleBuiltinFunctionCall(TIntermAggregate*);
    llvm::Value* handleUserFunctionCall(TIntermAggregate*);

    llvm::Value* createBinaryOperation(TOperator op, gla::EMdPrecision, llvm::Value* left, llvm::Value* right, bool isUnsigned, bool reduceComparison = true);
    llvm::Value* createUnaryOperation(TOperator op, gla::EMdPrecision, llvm::Value* operand);
    llvm::Value* createConversion(TOperator op, gla::EMdPrecision, llvm::Type*, llvm::Value* operand);
    llvm::Value* createUnaryIntrinsic(TOperator op, gla::EMdPrecision, llvm::Value* operand);
    llvm::Value* createIntrinsic(TOperator op, gla::EMdPrecision, std::vector<llvm::Value*>& operands, bool isUnsigned);
    void createPipelineRead(TIntermSymbol*, llvm::Value* storage, int slot, llvm::MDNode*);
    int assignSlot(TIntermSymbol* node, bool input);
    llvm::Value* createLLVMConstant(const TType& type, constUnion *consts, int& nextConst);
    void setAccessChainMetadata(TIntermSymbol* node, llvm::Value* typeProxy);
    llvm::MDNode* declareMdDefaultUniform(TIntermSymbol*, llvm::Value*);
    llvm::MDNode* makeMdSampler(const TType&, llvm::Value* typeProxy);
    llvm::MDNode* declareMdUniformBlock(gla::EMdInputOutput ioType, const TIntermSymbol* node, llvm::Value* typeProxy);
    llvm::MDNode* declareMdType(const TType&);
    void setOutputMetadata(TIntermSymbol* node, llvm::Value* typeProxy, int slot);
    llvm::MDNode* makeInputMetadata(TIntermSymbol* node, llvm::Value* typeProxy, int slot);

    llvm::LLVMContext &context;
    llvm::BasicBlock* shaderEntry;
    llvm::IRBuilder<> llvmBuilder;
    llvm::Module* module;
    gla::Metadata metadata;

    gla::Builder* glaBuilder;
    int nextSlot;                // non-user set interpolations slots, virtual space, so inputs and outputs can both share it
    bool inMain;

    std::map<int, llvm::Value*> namedValues;
    std::map<std::string, llvm::Function*> functionMap;
    std::map<std::string, int> slotMap;
    std::map<int, llvm::MDNode*> inputMdMap;
    std::map<std::string, llvm::MDNode*> uniformMdMap;
    std::map<TTypeList*, llvm::StructType*> structMap;
    std::stack<bool> breakForLoop;  // false means break for switch
};

namespace {

// Helper functions for translating glslang to metadata, so that information
// not representable in LLVM does not get lost.

gla::EMdInputOutput getMdQualifier(TIntermSymbol* node)
{
    gla::EMdInputOutput mdQualifier;
    switch (node->getQualifier().storage) {

    // inputs
    case EvqVertexId:   mdQualifier = gla::EMioVertexId;        break;
    case EvqInstanceId: mdQualifier = gla::EMioInstanceId;      break;
    case EvqFace:       mdQualifier = gla::EMioFragmentFace;    break;
    case EvqPointCoord: mdQualifier = gla::EMioPointCoord;      break;
    case EvqFragCoord:  mdQualifier = gla::EMioFragmentCoord;   break;
    case EvqVaryingIn:  mdQualifier = gla::EMioPipeIn;          break;

    // outputs
    case EvqPosition:   mdQualifier = gla::EMioVertexPosition;    break;
    case EvqPointSize:  mdQualifier = gla::EMioPointSize;         break;
    case EvqClipVertex: mdQualifier = gla::EMioClipVertex;        break;
    case EvqVaryingOut: mdQualifier = gla::EMioPipeOut;           break;
    case EvqFragColor:  mdQualifier = gla::EMioPipeOut;           break;
    case EvqFragDepth:  mdQualifier = gla::EMioFragmentDepth;     break;

    // uniforms
    case EVqBuffer:     mdQualifier = gla::EMioBufferBlockMember; break;
    case EvqUniform:    
                    if (node->getType().getBasicType() == EbtBlock)
                        mdQualifier = gla::EMioUniformBlockMember;
                    else
                        mdQualifier = gla::EMioDefaultUniform;
                                                                  break;
    default:
        mdQualifier = gla::EMioNone;
        break;
    }

    return mdQualifier;
}

gla::EMdTypeLayout getMdTypeLayout(const TType& type)
{
    gla::EMdTypeLayout mdType;

    if (type.isMatrix()) {
        switch (type.getQualifier().layoutMatrix) {
        case ElmRowMajor: mdType = gla::EMtlRowMajorMatrix;   break;
        default:          mdType = gla::EMtlColMajorMatrix;   break;
        }
    } else {
        switch (type.getBasicType()) {
        case EbtSampler:  mdType = gla::EMtlSampler;    break;
        case EbtStruct:   mdType = gla::EMtlAggregate;  break;
        case EbtUint:     mdType = gla::EMtlUnsigned;   break;
        case EbtBlock:
            switch (type.getQualifier().layoutPacking) {
            case ElpShared:  return gla::EMtlShared;
            case ElpStd140:  return gla::EMtlStd140;
            case ElpStd430:  return gla::EMtlStd430;
            case ElpPacked:  return gla::EMtlPacked;
            default:
                gla::UnsupportedFunctionality("block layout", gla::EATContinue);
                return gla::EMtlShared;
            }

        default:          mdType = gla::EMtlNone;       break;
        }
    }

    return mdType;
}

gla::EMdSampler getMdSampler(const TType& type)
{
    if (type.getSampler().image)
        return gla::EMsImage;
    else
        return gla::EMsTexture;
}

gla::EMdSamplerDim getMdSamplerDim(const TType& type)
{
    switch (type.getSampler().dim) {
    case Esd1D:     return gla::EMsd1D;
    case Esd2D:     return gla::EMsd2D;
    case Esd3D:     return gla::EMsd3D;
    case EsdCube:   return gla::EMsdCube;
    case EsdRect:   return gla::EMsdRect;
    case EsdBuffer: return gla::EMsdBuffer;
    default:
        gla::UnsupportedFunctionality("unknown sampler dimension", gla::EATContinue);
        return gla::EMsd2D;
    }
}

gla::EMdSamplerBaseType getMdSamplerBaseType(TBasicType type)
{
    switch (type) {
    case EbtFloat:    return gla::EMsbFloat;
    case EbtInt:      return gla::EMsbInt;
    case EbtUint:     return gla::EMsbUint;
    default:
        gla::UnsupportedFunctionality("base type of sampler return type", gla::EATContinue);
        return gla::EMsbFloat;
    }
}

gla::EMdPrecision getMdPrecision(const TType& type)
{
    switch (type.getQualifier().precision) {
    case EpqNone:    return gla::EMpNone;
    case EpqLow:     return gla::EMpLow;
    case EpqMedium:  return gla::EMpMedium;
    case EpqHigh:    return gla::EMpHigh;
    default:         return gla::EMpNone;
    }
}

llvm::Value* makePermanentTypeProxy(llvm::Value* value)
{
    // Make a type proxy that won't be optimized away (we still want the real llvm::Value to get optimized away when it can)
    llvm::Type* type = value->getType();
    while (type->getTypeID() == llvm::Type::PointerTyID)
        type = llvm::dyn_cast<llvm::PointerType>(type)->getContainedType(0);

    // TODO: memory: who/how owns tracking and deleting this allocation?
    return new llvm::GlobalVariable(type, true, llvm::GlobalVariable::ExternalLinkage, 0, value->getName() + "_typeProxy");
}

};  // end anonymous namespace


// A fully functionaling front end will know all array sizes,
// this is just a back up size.
const int UnknownArraySize = 8;

TGlslangToTopTraverser::TGlslangToTopTraverser(gla::Manager* manager)
    : context(llvm::getGlobalContext()), llvmBuilder(context),
      module(manager->getModule()), metadata(context, module),
      nextSlot(gla::MaxUserLayoutLocation), inMain(false), shaderEntry(0)
{
    // do this after the builder knows the module
    glaBuilder = new gla::Builder(llvmBuilder, manager, metadata);
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


//
// Symbols can turn into 
//  - pipeline reads, right now, as intrinic reads into shadow storage
//  - pipeline writes, sometime in the future, as intrinsic writes of shadow storage
//  - complex lvalue base setups:  foo.bar[3]....  , where we see foo and start up an access chain
//  - something simple that degenerates into the last bullet
//
// Uniforms, inputs, and outputs also get metadata hooked up for future linker consumption.
//
// Sort out what the deal is...
//
void TranslateSymbol(TIntermSymbol* node, TIntermTraverser* it)
{
    TGlslangToTopTraverser* oit = static_cast<TGlslangToTopTraverser*>(it);

    bool input = node->getType().getQualifier().isPipeInput();
    bool output = node->getType().getQualifier().isPipeOutput();

    // L-value chains will be computed purely left to right, so now is "clear" time
    // (since we are on the symbol; the base of the expression, which is left-most)
    oit->glaBuilder->clearAccessChain();

    // we will shadow inputs in global variables, so everything gets a variable
    // allocated, see if we've cached it
    std::map<int, llvm::Value*>::iterator iter;
    iter = oit->namedValues.find(node->getId());
    llvm::Value* storage;
    if (oit->namedValues.end() == iter) {
        // it was not found, create it
        storage = oit->createLLVMVariable(node);
        oit->namedValues[node->getId()] = storage;

        // set up metadata for future pipeline intrinsic writes
        if (output) {
            int slot = oit->assignSlot(node, input);
            oit->setOutputMetadata(node, storage, slot);
        }
    } else
        storage = iter->second;

    // Track the current value
    oit->glaBuilder->setAccessChainLValue(storage);

    // Set up metadata for uniform/sampler inputs
    if (node->getType().getQualifier().isUniform())
        oit->setAccessChainMetadata(node, storage);

    // If it's an arrayed output, we also want to know which indices
    // are live.
    if (node->isArray()) {
        switch (node->getQualifier().storage) {
        case EvqVaryingOut:
        case EvqClipVertex:
        case EvqFragColor:
            oit->glaBuilder->accessChainTrackOutputIndex();
        default:
            break;
        }
    }

    if (input) {
        int slot = oit->assignSlot(node, input);
        llvm::MDNode* mdNode = oit->inputMdMap[slot];
        if (mdNode == 0) {
            // set up metadata for pipeline intrinsic read
            mdNode = oit->makeInputMetadata(node, storage, slot);
            oit->inputMdMap[slot] = mdNode;
        }

        // do the actual read
        oit->createPipelineRead(node, storage, slot, mdNode);
    }
}

bool TranslateBinary(bool /* preVisit */, TIntermBinary* node, TIntermTraverser* it)
{
    TGlslangToTopTraverser* oit = static_cast<TGlslangToTopTraverser*>(it);

    llvm::Instruction::BinaryOps binOp = llvm::Instruction::BinaryOps(0);
    bool needsPromotion = true;

    // First, handle special cases
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
            llvm::Value* rValue = oit->glaBuilder->accessChainLoad(getMdPrecision(node->getRight()->getType()));

            if (node->getOp() != EOpAssign) {
                // the left is also an r-value
                oit->glaBuilder->setAccessChain(lValue);
                llvm::Value* leftRValue = oit->glaBuilder->accessChainLoad(getMdPrecision(node->getLeft()->getType()));

                // do the operation
                rValue = oit->createBinaryOperation(node->getOp(), getMdPrecision(node->getType()), leftRValue, rValue, node->getType().getBasicType() == EbtUint);

                // these all need their counterparts in createBinaryOperation()
                assert(rValue);
            }

            // store the result
            oit->glaBuilder->setAccessChain(lValue);
            oit->glaBuilder->accessChainStore(rValue);

            // assignments are expressions having an rValue after they are evaluated...
            oit->glaBuilder->clearAccessChain();
            oit->glaBuilder->setAccessChainRValue(rValue);
        }
        return false;
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
                                                             node->getLeft()->getVectorSize());
            } else {
                // struct or array or indirection into a vector; will use native LLVM gep
                // matrices are arrays of vectors, so will also work for a matrix

                // save it so that computing the right side doesn't trash it
                gla::Builder::AccessChain partial = oit->glaBuilder->getAccessChain();

                // compute the next index
                oit->glaBuilder->clearAccessChain();
                node->getRight()->traverse(oit);
                llvm::Value* index = oit->glaBuilder->accessChainLoad(getMdPrecision(node->getRight()->getType()));

                // make the new access chain to date
                oit->glaBuilder->setAccessChain(partial);
                oit->glaBuilder->accessChainPushLeft(index);
            }
        }
        return false;
    case EOpVectorSwizzle:
        {
            node->getLeft()->traverse(oit);
            TIntermSequence& swizzleSequence = node->getRight()->getAsAggregate()->getSequence();
            std::vector<int> swizzle;
            for (int i = 0; i < swizzleSequence.size(); ++i)
                swizzle.push_back(swizzleSequence[i]->getAsConstantUnion()->getUnionArrayPointer()->getIConst());
            oit->glaBuilder->accessChainPushSwizzleRight(swizzle, oit->convertGlslangToGlaType(node->getType()),
                                                         node->getLeft()->getVectorSize());
        }
        return false;
    }

    // Assume generic binary op...

    // Get the operands
    oit->glaBuilder->clearAccessChain();
    node->getLeft()->traverse(oit);
    llvm::Value* left = oit->glaBuilder->accessChainLoad(getMdPrecision(node->getLeft()->getType()));

    oit->glaBuilder->clearAccessChain();
    node->getRight()->traverse(oit);
    llvm::Value* right = oit->glaBuilder->accessChainLoad(getMdPrecision(node->getRight()->getType()));

    llvm::Value* result;
    gla::EMdPrecision precision = getMdPrecision(node->getType());

    switch (node->getOp()) {
    case EOpVectorTimesMatrix:
    case EOpMatrixTimesVector:
    case EOpMatrixTimesScalar:
    case EOpMatrixTimesMatrix:
        result = oit->glaBuilder->createMatrixMultiply(precision, left, right);
        break;
    default:
        result = oit->createBinaryOperation(node->getOp(), precision, left, right, node->getType().getBasicType() == EbtUint);
    }

    if (! result) {
        gla::UnsupportedFunctionality("glslang binary operation", gla::EATContinue);
    } else {
        oit->glaBuilder->clearAccessChain();
        oit->glaBuilder->setAccessChainRValue(result);

        return false;
    }

    return true;
}

bool TranslateUnary(bool /* preVisit */, TIntermUnary* node, TIntermTraverser* it)
{
    TGlslangToTopTraverser* oit = static_cast<TGlslangToTopTraverser*>(it);

    oit->glaBuilder->clearAccessChain();
    node->getOperand()->traverse(oit);
    llvm::Value* operand = oit->glaBuilder->accessChainLoad(getMdPrecision(node->getOperand()->getType()));

    gla::EMdPrecision precision = getMdPrecision(node->getType());

    // it could be a conversion
    llvm::Value* result = oit->createConversion(node->getOp(), precision, oit->convertGlslangToGlaType(node->getType()), operand);

    // if not, then possibly an operation
    if (! result)
        result = oit->createUnaryOperation(node->getOp(), precision, operand);

    // if not, then possibly a LunarGLASS intrinsic
    if (! result)
        result = oit->createUnaryIntrinsic(node->getOp(), precision, operand);

    if (result) {
        oit->glaBuilder->clearAccessChain();
        oit->glaBuilder->setAccessChainRValue(result);

        return false; // done with this node
    }

    // it must be a special case, check...
    switch (node->getOp()) {
    case EOpPostIncrement:
    case EOpPostDecrement:
    case EOpPreIncrement:
    case EOpPreDecrement:
        {
            // we need the integer value "1" or the floating point "1.0" to add/subtract
            llvm::Value* one = gla::GetBasicTypeID(operand) == llvm::Type::FloatTyID ?
                                     gla::MakeFloatConstant(oit->context, 1.0) :
                                     gla::MakeIntConstant(oit->context, 1);
            TOperator op;
            if (node->getOp() == EOpPreIncrement ||
                node->getOp() == EOpPostIncrement)
                op = EOpAdd;
            else
                op = EOpSub;

            llvm::Value* result = oit->createBinaryOperation(op, getMdPrecision(node->getType()), operand, one, node->getType().getBasicType() == EbtUint);

            // The result of operation is always stored, but conditionally the
            // consumed result.  The consumed result is always an r-value.
            oit->glaBuilder->accessChainStore(result);
            oit->glaBuilder->clearAccessChain();
            if (node->getOp() == EOpPreIncrement ||
                node->getOp() == EOpPreDecrement)
                oit->glaBuilder->setAccessChainRValue(result);
            else
                oit->glaBuilder->setAccessChainRValue(operand);
        }
        return false;
    default:
        gla::UnsupportedFunctionality("glslang unary", gla::EATContinue);
    }

    return true;
}

bool TranslateAggregate(bool preVisit, TIntermAggregate* node, TIntermTraverser* it)
{
    TGlslangToTopTraverser* oit = static_cast<TGlslangToTopTraverser*>(it);
    llvm::Value* result;
    TOperator binOp = EOpNull;
    bool reduceComparison = true;
    bool isMatrix = false;

    assert(node->getOp());

    gla::EMdPrecision precision = getMdPrecision(node->getType());

    switch (node->getOp()) {
    case EOpSequence:
        {
            // If this is the parent node of all the functions, we want to see them
            // early, so all call points have actual LLVM functions to reference.  
            // In all cases, still let the traverser visit the children for us.
            if (preVisit)
                oit->makeFunctions(node->getAsAggregate()->getSequence());
        }

        return true;
    case EOpComma:
        {
            // processing from left to right naturally leaves the right-most
            // lying around in the access chain
            TIntermSequence& glslangOperands = node->getSequence();
            for (int i = 0; i < glslangOperands.size(); ++i)
                glslangOperands[i]->traverse(oit);
        }

        return false;
    case EOpFunction:
        if (preVisit) {
            if (oit->isShaderEntrypoint(node)) {
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
            if (node->isUserDefined())
                result = oit->handleUserFunctionCall(node);
            else
                result = oit->handleBuiltinFunctionCall(node);

            if (! result)
                gla::UnsupportedFunctionality("glslang function call", gla::EATContinue);
            else {
                oit->glaBuilder->clearAccessChain();
                oit->glaBuilder->setAccessChainRValue(result);

                return false;
            }
        }

        return true;
    case EOpConstructMat2x2:
    case EOpConstructMat2x3:
    case EOpConstructMat2x4:
    case EOpConstructMat3x2:
    case EOpConstructMat3x3:
    case EOpConstructMat3x4:
    case EOpConstructMat4x2:
    case EOpConstructMat4x3:
    case EOpConstructMat4x4:
    case EOpConstructDMat2x2:
    case EOpConstructDMat2x3:
    case EOpConstructDMat2x4:
    case EOpConstructDMat3x2:
    case EOpConstructDMat3x3:
    case EOpConstructDMat3x4:
    case EOpConstructDMat4x2:
    case EOpConstructDMat4x3:
    case EOpConstructDMat4x4:
        isMatrix = true;
        // fall through
    case EOpConstructFloat:
    case EOpConstructVec2:
    case EOpConstructVec3:
    case EOpConstructVec4:
    case EOpConstructDouble:
    case EOpConstructDVec2:
    case EOpConstructDVec3:
    case EOpConstructDVec4:
    case EOpConstructBool:
    case EOpConstructBVec2:
    case EOpConstructBVec3:
    case EOpConstructBVec4:
    case EOpConstructInt:
    case EOpConstructIVec2:
    case EOpConstructIVec3:
    case EOpConstructIVec4:
    case EOpConstructUint:
    case EOpConstructUVec2:
    case EOpConstructUVec3:
    case EOpConstructUVec4:
    case EOpConstructStruct:
        {
            std::vector<llvm::Value*> arguments;
            oit->translateArguments(node->getSequence(), arguments);
            llvm::Value* constructed = oit->glaBuilder->createVariable(gla::Builder::ESQLocal, 0,
                                                                        oit->convertGlslangToGlaType(node->getType()),
                                                                        0, 0, "constructed");
            if (node->getOp() == EOpConstructStruct) {
                //TODO: clean up: is there a more direct way to set a whole LLVM structure?
                //                if not, move this inside Top Builder; too many indirections

                std::vector<llvm::Value*> gepChain;
                gepChain.push_back(gla::MakeIntConstant(oit->context, 0));
                for (int field = 0; field < arguments.size(); ++field) {
                    gepChain.push_back(gla::MakeIntConstant(oit->context, field));
                    llvm::Value* loadVal = oit->llvmBuilder.CreateStore(arguments[field],
                                                                        oit->glaBuilder->createGEP(constructed, gepChain));
                    gepChain.pop_back();
                }
                oit->glaBuilder->clearAccessChain();
                oit->glaBuilder->setAccessChainLValue(constructed);
            } else {
                constructed = oit->glaBuilder->createLoad(constructed);
                if (isMatrix)
                    constructed = oit->glaBuilder->createMatrixConstructor(precision, arguments, constructed);
                else
                    constructed = oit->glaBuilder->createConstructor(precision, arguments, constructed);
                oit->glaBuilder->clearAccessChain();
                oit->glaBuilder->setAccessChainRValue(constructed);
            }

            return false;
        }

    // These six are component-wise compares with component-wise results.
    // Forward on to createBinaryOperation(), requesting a vector result.
    case EOpLessThan:
    case EOpGreaterThan:
    case EOpLessThanEqual:
    case EOpGreaterThanEqual:
    case EOpVectorEqual:
    case EOpVectorNotEqual:
        {
            // Map the operation to a binary
            binOp = node->getOp();
            reduceComparison = false;
            switch (node->getOp()) {
            case EOpVectorEqual:     binOp = EOpEqual;      break;
            case EOpVectorNotEqual:  binOp = EOpNotEqual;   break;
            default:                 binOp = node->getOp(); break;
            }
        }
        break;

    //case EOpRecip:
    //    return glaBuilder->createRecip(operand);

    case EOpMul:
        // compontent-wise matrix multiply      
        binOp = EOpMul;
        break;
    case EOpOuterProduct:
        // two vectors multiplied to make a matrix
        binOp = EOpOuterProduct;
        break;
    case EOpDot:
        {
            // for scalar dot product, use multiply        
            TIntermSequence& glslangOperands = node->getSequence();
            if (! glslangOperands[0]->getAsTyped()->isVector())
                binOp = EOpMul;
            break;
        }
    case EOpMod:
        // when an aggregate, this is the floating-point mod built-in function,
        // which can be emitted by the one it createBinaryOperation()
        binOp = EOpMod;
        break;
    case EOpArrayLength:
        {
            TIntermTyped* typedNode = node->getSequence()[0]->getAsTyped();
            assert(typedNode);
            llvm::Value* length = gla::MakeIntConstant(oit->context, typedNode->getType().getArraySize());

            oit->glaBuilder->clearAccessChain();
            oit->glaBuilder->setAccessChainRValue(length);
        }

        return false;
    }

    //
    // See if it maps to a regular operation or intrinsic.
    //

    if (binOp != EOpNull) {
        oit->glaBuilder->clearAccessChain();
        node->getSequence()[0]->traverse(oit);
        llvm::Value* left = oit->glaBuilder->accessChainLoad(getMdPrecision(node->getSequence()[0]->getAsTyped()->getType()));

        oit->glaBuilder->clearAccessChain();
        node->getSequence()[1]->traverse(oit);
        llvm::Value* right = oit->glaBuilder->accessChainLoad(getMdPrecision(node->getSequence()[1]->getAsTyped()->getType()));

        if (binOp == EOpOuterProduct)
            result = oit->glaBuilder->createMatrixMultiply(precision, left, right);
        else if (gla::IsAggregate(left) && binOp == EOpMul)
            result = oit->glaBuilder->createMatrixOp(precision, llvm::Instruction::FMul, left, right);
        else
            result = oit->createBinaryOperation(binOp, precision, left, right, node->getType().getBasicType() == EbtUint, reduceComparison);

        // code above should only make binOp that exists in createBinaryOperation
        assert(result);

        oit->glaBuilder->clearAccessChain();
        oit->glaBuilder->setAccessChainRValue(result);

        return false;
    }

    TIntermSequence& glslangOperands = node->getSequence();
    std::vector<llvm::Value*> operands;
    for (int i = 0; i < glslangOperands.size(); ++i) {
        oit->glaBuilder->clearAccessChain();
        glslangOperands[i]->traverse(oit);
        operands.push_back(oit->glaBuilder->accessChainLoad(getMdPrecision(glslangOperands[i]->getAsTyped()->getType())));
    }
    if (glslangOperands.size() == 1)
        result = oit->createUnaryIntrinsic(node->getOp(), precision, operands.front());
    else
        result = oit->createIntrinsic(node->getOp(), precision, operands, glslangOperands.front()->getAsTyped()->getBasicType() == EbtUint);

    if (! result)
        gla::UnsupportedFunctionality("glslang aggregate", gla::EATContinue);
    else {
        oit->glaBuilder->clearAccessChain();
        oit->glaBuilder->setAccessChainRValue(result);

        return false;
    }

    return true;
}

bool TranslateSelection(bool /* preVisit */, TIntermSelection* node, TIntermTraverser* it)
{
    TGlslangToTopTraverser* oit = static_cast<TGlslangToTopTraverser*>(it);

    // This path handles both if-then-else and ?:
    // The if-then-else has a node type of void, while
    // ?: has a non-void node type
    llvm::Value* result = 0;
    if (node->getBasicType() != EbtVoid) {
        // don't handle this as just on-the-fly temporaries, because there will be two names
        // and better to leave SSA to LLVM passes
        result = oit->glaBuilder->createVariable(gla::Builder::ESQLocal, 0, oit->convertGlslangToGlaType(node->getType()),
                                                 0, 0, "ternary");
    }

    // emit the condition before doing anything with selection
    node->getCondition()->traverse(it);

    // make an "if" based on the value created by the condition
    gla::Builder::If ifBuilder(oit->glaBuilder->accessChainLoad(gla::EMpNone), oit->glaBuilder);

    if (node->getTrueBlock()) {
        // emit the "then" statement
		node->getTrueBlock()->traverse(it);
        if (result)
            oit->glaBuilder->createStore(oit->glaBuilder->accessChainLoad(getMdPrecision(node->getTrueBlock()->getAsTyped()->getType())), result);
	}

    if (node->getFalseBlock()) {
        ifBuilder.makeBeginElse();
        // emit the "else" statement
        node->getFalseBlock()->traverse(it);
        if (result)
            oit->glaBuilder->createStore(oit->glaBuilder->accessChainLoad(getMdPrecision(node->getFalseBlock()->getAsTyped()->getType())), result);
    }

    ifBuilder.makeEndIf();

    if (result) {
        // GLSL only has r-values as the result of a :?, but
        // if we have an l-value, that can be more efficient if it will
        // become the base of a complex r-value expression, because the
        // next layer copies r-values into memory to use the GEP mechanism
        oit->glaBuilder->clearAccessChain();
        oit->glaBuilder->setAccessChainLValue(result);
    }

    return false;
}

bool TranslateSwitch(bool /* preVisit */, TIntermSwitch* node, TIntermTraverser* it)
{
    TGlslangToTopTraverser* oit = static_cast<TGlslangToTopTraverser*>(it);

    // emit and get the condition before doing anything with switch
    node->getCondition()->traverse(it);
    llvm::Value* condition = oit->glaBuilder->accessChainLoad(getMdPrecision(node->getCondition()->getAsTyped()->getType()));

    // browse the children to sort out code segments
    int defaultSegment = -1;
    std::vector<TIntermNode*> codeSegments;
    TIntermSequence& sequence = node->getBody()->getSequence();
    std::vector<llvm::ConstantInt*> caseValues;
    std::vector<int> valueToSegment(sequence.size());  // note: probably not all are used, it is an overestimate
    for (TIntermSequence::iterator c = sequence.begin(); c != sequence.end(); ++c) {
        TIntermNode* child = *c;
        if (child->getAsBranchNode() && child->getAsBranchNode()->getFlowOp() == EOpDefault)
            defaultSegment = codeSegments.size();
        else if (child->getAsBranchNode() && child->getAsBranchNode()->getFlowOp() == EOpCase) {
            valueToSegment[caseValues.size()] = codeSegments.size();
            caseValues.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(oit->context), 
                                                        child->getAsBranchNode()->getExpression()->getAsConstantUnion()->getUnionArrayPointer()[0].getIConst(), 
                                                        false));
        } else
            codeSegments.push_back(child);
    }

    // make the switch statement
    std::vector<llvm::BasicBlock*> segmentBB;
    oit->glaBuilder->makeSwitch(condition, codeSegments.size(), caseValues, valueToSegment, defaultSegment, segmentBB);

    // emit all the code in the segments
    oit->breakForLoop.push(false);
    for (unsigned int s = 0; s < codeSegments.size(); ++s) {
        oit->glaBuilder->nextSwitchSegment(segmentBB, s);
        codeSegments[s]->traverse(it);
    }
    oit->breakForLoop.pop();

    oit->glaBuilder->endSwitch(segmentBB);

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
    bool bodyOut = false;

    // Note: inductive loops are getting recognized at a lower level,
	// so no need to worry about them now.  I.e., don't bother to use
    // makeNewLoop(<lots of arguments>).

    oit->glaBuilder->makeNewLoop();

    if (! node->testFirst()) {
        if (node->getBody()) {
            oit->breakForLoop.push(true);
            node->getBody()->traverse(it);
            oit->breakForLoop.pop();
        }
        bodyOut = true;
    }

    if (node->getTest()) {
        node->getTest()->traverse(it);
        // the AST only contained the test, not the branch, we have to add it

        // make the following
        //     if (! condition from test traversal)
        //         break;
        llvm::Value* condition = oit->glaBuilder->accessChainLoad(getMdPrecision(node->getTest()->getType()));
        condition = oit->llvmBuilder.CreateNot(condition);
        gla::Builder::If ifBuilder(condition, oit->glaBuilder);
        oit->glaBuilder->makeLoopExit();
        ifBuilder.makeEndIf();
    }

    if (! bodyOut && node->getBody()) {
        oit->breakForLoop.push(true);
        node->getBody()->traverse(it);
        oit->breakForLoop.pop();
    }

    if (node->getTerminal())
        node->getTerminal()->traverse(it);

    oit->glaBuilder->makeLoopBackEdge();
    oit->glaBuilder->closeLoop();

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
        if (oit->breakForLoop.top())
            oit->glaBuilder->makeLoopExit();
        else
            oit->glaBuilder->addSwitchBreak();
        break;
    case EOpContinue:
        oit->glaBuilder->makeLoopBackEdge();
        break;
    case EOpReturn:
        if (oit->inMain)
            oit->glaBuilder->makeMainReturn();
        else if (node->getExpression()) {
            oit->glaBuilder->makeReturn(false, oit->glaBuilder->accessChainLoad(getMdPrecision(node->getExpression()->getType())));
        } else
            oit->glaBuilder->makeReturn();

        oit->glaBuilder->clearAccessChain();
        break;

    default:
        gla::UnsupportedFunctionality("branch type");
    }

    return false;
}

llvm::Value* TGlslangToTopTraverser::createLLVMVariable(TIntermSymbol* node)
{
    llvm::Constant* initializer = 0;
    gla::Builder::EStorageQualifier storageQualifier;
    int constantBuffer = 0;

    switch (node->getQualifier().storage) {
    case EvqTemporary:
        storageQualifier = gla::Builder::ESQLocal;
        break;
    case EvqGlobal:
        storageQualifier = gla::Builder::ESQGlobal;
        break;
    case EvqConst:
        gla::UnsupportedFunctionality("glslang const variable", gla::EATContinue);
        storageQualifier = gla::Builder::ESQLocal;
        break;
    case EvqVaryingIn:
    case EvqFragCoord:
    case EvqPointCoord:
    case EvqFace:
    case EvqVertexId:
    case EvqInstanceId:
        // Pipeline reads: If we are here, it must be to create a shadow which
        // will shadow the actual pipeline reads, which must still be done elsewhere.
        // The top builder will make a global shadow for ESQInput.
        storageQualifier = gla::Builder::ESQInput;
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
    case EVqBuffer:
        storageQualifier = gla::Builder::ESQUniform;
        // TODO: linker functionality: uniform buffers? need to generalize to N objects (constant buffers) for higher shader models
        constantBuffer = 0;
        break;
    case EvqIn:
    case EvqOut:
    case EvqInOut:
    case EvqConstReadOnly:
        // parameter qualifiers should not come through here
    default:
        gla::UnsupportedFunctionality("glslang qualifier", gla::EATContinue);
        storageQualifier = gla::Builder::ESQLocal;
    }

    if (node->getBasicType() == EbtSampler) {
        storageQualifier = gla::Builder::ESQResource;
    }

    std::string name(node->getSymbol().c_str());

    llvm::Type *llvmType = convertGlslangToGlaType(node->getType());

    return glaBuilder->createVariable(storageQualifier, constantBuffer, llvmType,
                                      initializer, 0, name);
}

llvm::Type* TGlslangToTopTraverser::convertGlslangToGlaType(const TType& type)
{
    llvm::Type *glaType;

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
    case EbtSampler:
        glaType = gla::GetIntType(context);
        break;
    case EbtUint:
        glaType = gla::GetUintType(context);
        break;
    case EbtStruct:
    case EbtBlock:
        {
            TTypeList* glslangStruct = type.getStruct();
            std::vector<llvm::Type*> structFields;
            llvm::StructType* structType = structMap[glslangStruct];
            if (structType) {
                // If we've seen this struct type, return it
                glaType = structType;
            } else {
                // Create a vector of struct types for LLVM to consume
                for (int i = 0; i < glslangStruct->size(); i++)
                    structFields.push_back(convertGlslangToGlaType(*(*glslangStruct)[i].type));
                structType = llvm::StructType::create(context, structFields, type.getTypeName().c_str());
                structMap[glslangStruct] = structType;
                glaType = structType;
            }
        }
        break;
    default:
        gla::UnsupportedFunctionality("basic type");
    }

    if (type.isMatrix())
        glaType = gla::Builder::getMatrixType(glaType, type.getMatrixCols(), type.getMatrixRows());
    else {
        // If this variable has a vector element count greater than 1, create an LLVM vector
        if (type.getVectorSize() > 1)
            glaType = llvm::VectorType::get(glaType, type.getVectorSize());
    }

    if (type.isArray()) {
        int arraySize = type.getArraySize();
        if (arraySize == 0) {
            gla::UnsupportedFunctionality("unsized array", gla::EATContinue);
            arraySize = UnknownArraySize;
        }
        glaType = llvm::ArrayType::get(glaType, arraySize);
    }

    return glaType;
}

bool TGlslangToTopTraverser::isShaderEntrypoint(const TIntermAggregate* node)
{
    return node->getName() == "main(";
}

void TGlslangToTopTraverser::makeFunctions(const TIntermSequence& glslFunctions)
{
    for (int f = 0; f < glslFunctions.size(); ++f) {
        TIntermAggregate* glslFunction = glslFunctions[f]->getAsAggregate();

        // TODO: compile-time performance: find a way to skip this loop if we aren't
        // a child of the root node of the compilation unit, which should be the only
        // one holding a list of functions.
        if (! glslFunction || glslFunction->getOp() != EOpFunction || isShaderEntrypoint(glslFunction))
            continue;

        std::vector<llvm::Type*> paramTypes;
        TIntermSequence& parameters = glslFunction->getSequence()[0]->getAsAggregate()->getSequence();

        // At call time, space should be allocated for all the arguments,
        // and pointers to that space passed to the function as the formal parameters.
        for (int i = 0; i < parameters.size(); ++i) {
            llvm::Type* type = convertGlslangToGlaType(parameters[i]->getAsTyped()->getType());
            paramTypes.push_back(llvm::PointerType::get(type, gla::GlobalAddressSpace));
        }

        llvm::BasicBlock* functionBlock;
        llvm::Function *function = glaBuilder->makeFunctionEntry(convertGlslangToGlaType(glslFunction->getType()), glslFunction->getName().c_str(),
                                                                 paramTypes, &functionBlock);
        function->addFnAttr(llvm::Attributes::AlwaysInline);

        // Visit parameter list again to create mappings to local variables and set attributes.
        llvm::Function::arg_iterator arg = function->arg_begin();
        for (int i = 0; i < parameters.size(); ++i, ++arg)
            namedValues[parameters[i]->getAsSymbolNode()->getId()] = &(*arg);

        // Track function to emit/call later
        functionMap[glslFunction->getName().c_str()] = function;
    }
}

void TGlslangToTopTraverser::handleFunctionEntry(TIntermAggregate* node)
{
    // LLVM functions should already be in the functionMap from the prepass 
    // that called makeFunctions.
    llvm::Function* function = functionMap[node->getName().c_str()];
    llvm::BasicBlock& functionBlock = function->getEntryBlock();
    llvmBuilder.SetInsertPoint(&functionBlock);
}

void TGlslangToTopTraverser::translateArguments(TIntermSequence& glslangArguments, std::vector<llvm::Value*>& arguments)
{
    for (int i = 0; i < glslangArguments.size(); ++i) {
        glaBuilder->clearAccessChain();
        glslangArguments[i]->traverse(this);
        arguments.push_back(glaBuilder->accessChainLoad(getMdPrecision(glslangArguments[i]->getAsTyped()->getType())));
    }
}

llvm::Value* TGlslangToTopTraverser::handleBuiltinFunctionCall(TIntermAggregate* node)
{
    std::vector<llvm::Value*> arguments;
    translateArguments(node->getSequence(), arguments);

    llvm::Intrinsic::ID intrinsicID = llvm::Intrinsic::ID(0);

    gla::EMdPrecision precision = getMdPrecision(node->getType());

    if (node->getName() == "ftransform(") {
        // TODO: back-end functionality: if this needs to support decomposition, need to simulate
        // access to the external gl_Vertex and gl_ModelViewProjectionMatrix.
        // For now, pass in dummy arguments, which are thrown away anyway
        // if ftransform is consumed by the backend without decomposition.
        llvm::Value* vertex = glaBuilder->createVariable(gla::Builder::ESQGlobal, 0, llvm::VectorType::get(gla::GetFloatType(context), 4),
                                                                     0, 0, "gl_Vertex_sim");
        llvm::Value* matrix = glaBuilder->createVariable(gla::Builder::ESQGlobal, 0, llvm::VectorType::get(gla::GetFloatType(context), 4),
                                                                     0, 0, "gl_ModelViewProjectionMatrix_sim");

        return glaBuilder->createIntrinsicCall(precision, llvm::Intrinsic::gla_fFixedTransform, glaBuilder->createLoad(vertex), glaBuilder->createLoad(matrix));
    }

    if (node->getName().substr(0, 7) == "texture" || node->getName().substr(0, 5) == "texel" || node->getName().substr(0, 6) == "shadow") {
        gla::ESamplerType samplerType;
        switch (node->getSequence()[0]->getAsTyped()->getType().getSampler().dim) {
        case Esd1D:       samplerType = gla::ESampler1D;      break;
        case Esd2D:       samplerType = gla::ESampler2D;      break;
        case Esd3D:       samplerType = gla::ESampler3D;      break;
        case EsdCube:     samplerType = gla::ESamplerCube;    break;
        case EsdRect:     samplerType = gla::ESampler2DRect;  break;
        case EsdBuffer:   samplerType = gla::ESamplerBuffer;  break;
        default:
            gla::UnsupportedFunctionality("sampler type");
        }

        if (node->getName().find("Size", 0) != std::string::npos) {
            if (node->getSequence()[0]->getAsTyped()->getType().getSampler().ms ||
                                                  samplerType == gla::ESamplerBuffer)
                gla::UnsupportedFunctionality("TextureSize of multi-sample or buffer texture");
                
            return glaBuilder->createTextureQueryCall(precision,
                                                       llvm::Intrinsic::gla_queryTextureSize, 
                                                       convertGlslangToGlaType(node->getType()), 
                                                       MakeIntConstant(context, samplerType), 
                                                       arguments[0], arguments[1]);
        }

        if (node->getName().find("Query", 0) != std::string::npos) {
            if (node->getName().find("Lod", 0) != std::string::npos) {
                gla::UnsupportedFunctionality("textureQueryLod");
                return glaBuilder->createTextureQueryCall(precision,
                                                           llvm::Intrinsic::gla_fQueryTextureLod,
                                                           convertGlslangToGlaType(node->getType()), 
                                                           MakeIntConstant(context, samplerType), 
                                                           arguments[0], 0);
            } else if (node->getName().find("Levels", 0) != std::string::npos) {
                gla::UnsupportedFunctionality("textureQueryLevels");
            }
        }

        int texFlags = 0;
        if (node->getName().find("Lod", 0) != std::string::npos) {
            texFlags |= gla::ETFLod;
            texFlags |= gla::ETFBiasLodArg;
        }

        if (node->getName().find("Proj", 0) != std::string::npos)
            texFlags |= gla::ETFProjected;

        if (node->getName().find("Offset", 0) != std::string::npos) {
            texFlags |= gla::ETFOffsetArg;
        }

        if (node->getName().find("Fetch", 0) != std::string::npos) {
            texFlags |= gla::ETFFetch;
        }

        if (node->getSequence()[0]->getAsTyped()->getType().getSampler().shadow)
            texFlags |= gla::ETFShadow;
        
        if (node->getSequence()[0]->getAsTyped()->getType().getSampler().ms)
            samplerType = gla::ESampler2D;

        if (node->getSequence()[0]->getAsTyped()->getType().getSampler().arrayed)
            texFlags |= gla::ETFArrayed;

        // check for bias argument
        if (! (texFlags & gla::ETFLod)) {
            int nonBiasArgCount = 2;
            if (texFlags & gla::ETFOffsetArg)
                ++nonBiasArgCount;
            if (texFlags & gla::ETFBiasLodArg)
                ++nonBiasArgCount;
            if (node->getName().find("Grad", 0) != std::string::npos)
                nonBiasArgCount += 2;

            if (arguments.size() > nonBiasArgCount) {
                texFlags |= gla::ETFBias;
                texFlags |= gla::ETFBiasLodArg;
            }
        }

        // TODO: 4.0 functionality: handle 'compare' argument

        // set the arguments        
        gla::Builder::TextureParameters params = {};
        params.ETPSampler = arguments[0];
        params.ETPCoords = arguments[1];
        int extraArgs = 0;
        if (texFlags & gla::ETFLod) {
            params.ETPBiasLod = arguments[2];
            ++extraArgs;
        }
        if (node->getName().find("Grad", 0) != std::string::npos) {
            params.ETPGradX = arguments[2 + extraArgs];
            params.ETPGradY = arguments[3 + extraArgs];
            extraArgs += 2;
        }
        if (texFlags & gla::ETFOffsetArg) {
            params.ETPOffset = arguments[2 + extraArgs];
            ++extraArgs;
        }
        if (texFlags & gla::ETFBias) {
            params.ETPBiasLod = arguments[2 + extraArgs];
            ++extraArgs;
        }

        return glaBuilder->createTextureCall(precision, convertGlslangToGlaType(node->getType()), samplerType, texFlags, params);
    }

    return 0;
}

llvm::Value* TGlslangToTopTraverser::handleUserFunctionCall(TIntermAggregate* node)
{
    // Overall design is to pass pointers to the arguments, as described:
    //
    // For input arguments, they could be expressions, and their value could be
    // overwritten without impacting anything in the caller, so store the answer
    // and pass a pointer to it.
    //
    // For output arguments, there could still be a conversion needed, so
    // so make space for the answer, and convert it before sticking it into
    // the original l-value provide.  (Pass the pointer to the space made.)
    //
    // For inout, just do both the above, but using a single space/pointer
    // to do it.
    //

    // Grab the pointer from the previously created function
    llvm::Function* function = functionMap[node->getName().c_str()];
    if (! function)
        return 0;

    // First step:  Allocate the space for the arguments and build llvm
    // pointers to it as the passed in arguments.
    llvm::SmallVector<llvm::Value*, 4> llvmArgs;
    llvm::Function::arg_iterator param;
    llvm::Function::arg_iterator end = function->arg_end();
    for (param = function->arg_begin(); param != end; ++param) {
        // param->getType() should be a pointer, we need the type it points to
        llvm::Value* space = glaBuilder->createVariable(gla::Builder::ESQLocal, 0, param->getType()->getContainedType(0), 0, 0, "param");
        llvmArgs.push_back(space);
    }

    // Copy-in time...
    // Compute the access chains of output argument l-values before making the call,
    // to be used after making the call.  Also compute r-values of inputs and store
    // them into the space allocated above.
    TIntermSequence& glslangArgs = node->getSequence();
    TQualifierList& qualifiers = node->getQualifierList();
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
            glaBuilder->createStore(glaBuilder->accessChainLoad(getMdPrecision(glslangArgs[i]->getAsTyped()->getType())), llvmArgs[i]);
        }
    }

    llvm::Value* result = llvmBuilder.Insert(llvm::CallInst::Create(function, llvmArgs));

    // Copy-out time...
    // Convert outputs to correct type before storing into the l-value
    llvm::SmallVector<gla::Builder::AccessChain, 2>::iterator savedIt = lValuesOut.begin();
    for (int i = 0; i < glslangArgs.size(); ++i) {
        if (qualifiers[i] == EvqOut || qualifiers[i] == EvqInOut) {
            glaBuilder->setAccessChain(*savedIt);
            llvm::Value* output = glaBuilder->createLoad(llvmArgs[i]);
            llvm::Type* destType = convertGlslangToGlaType(glslangArgs[i]->getAsTyped()->getType());
            if (destType != output->getType()) {
                // TODO: non-ES testing: test this after the front-end can support it
                TOperator op = EOpNull;
                if (gla::GetBasicTypeID(destType) == llvm::Type::FloatTyID &&
                    gla::GetBasicTypeID(output->getType())) {
                    op = EOpConvIntToFloat;
                } // TODO: desktop functionality: more cases will go here for future versions

                if (op != EOpNull) {
                    output = createConversion(op, gla::EMpNone, destType, output);
                    assert(output);
                } else
                    gla::UnsupportedFunctionality("unexpected output parameter conversion");
            }
            glaBuilder->accessChainStore(output);
            ++savedIt;
        }
    }

    return result;
}

llvm::Value* TGlslangToTopTraverser::createBinaryOperation(TOperator op, gla::EMdPrecision precision, llvm::Value* left, llvm::Value* right, bool isUnsigned, bool reduceComparison)
{
    llvm::Instruction::BinaryOps binOp = llvm::Instruction::BinaryOps(0);
    bool needsPromotion = true;
    bool leftIsFloat = (gla::GetBasicTypeID(left) == llvm::Type::FloatTyID);
    bool comparison = false;

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
    case EOpVectorTimesMatrixAssign:
    case EOpMatrixTimesScalarAssign:
    case EOpMatrixTimesMatrixAssign:
        if (leftIsFloat)
            binOp = llvm::Instruction::FMul;
        else
            binOp = llvm::Instruction::Mul;
        break;
    case EOpDiv:
    case EOpDivAssign:
        if (leftIsFloat)
            binOp = llvm::Instruction::FDiv;
        else if (isUnsigned)
            binOp = llvm::Instruction::UDiv;
        else
            binOp = llvm::Instruction::SDiv;
        break;
    case EOpMod:
    case EOpModAssign:
        if (leftIsFloat)
            binOp = llvm::Instruction::FRem;
        else if (isUnsigned)
            binOp = llvm::Instruction::URem;
        else
            binOp = llvm::Instruction::SRem;
        break;
    case EOpRightShift:
    case EOpRightShiftAssign:
        if (isUnsigned)
            binOp = llvm::Instruction::LShr;
        else
            binOp = llvm::Instruction::AShr;
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

    case EOpLessThan:
    case EOpGreaterThan:
    case EOpLessThanEqual:
    case EOpGreaterThanEqual:
    case EOpEqual:
    case EOpNotEqual:
        comparison = true;
        break;
    }

    if (binOp != 0) {
        if (gla::IsAggregate(left) || gla::IsAggregate(right)) {
            switch(op) {
            case EOpVectorTimesMatrixAssign:
            case EOpMatrixTimesScalarAssign:
            case EOpMatrixTimesMatrixAssign:
                return glaBuilder->createMatrixMultiply(precision, left, right);
            default:
                return glaBuilder->createMatrixOp(precision, binOp, left, right);
            }
        }

        if (needsPromotion)
            glaBuilder->promoteScalar(precision, left, right);

        llvm::Value* value = llvmBuilder.CreateBinOp(binOp, left, right);
        glaBuilder->setInstructionPrecision(value, precision);

        return value;
    }

    if (! comparison)
        return 0;

    // Comparison instructions

    if (reduceComparison && (gla::IsVector(left) || gla::IsAggregate(left))) {
        assert(op == EOpEqual || op == EOpNotEqual);

        return glaBuilder->createCompare(precision, left, right, op == EOpEqual);
    }

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

        if (pred != 0) {
            llvm::Value* result = llvmBuilder.CreateFCmp(pred, left, right);
            glaBuilder->setInstructionPrecision(result, precision);

            return result;
        }
    } else {
        llvm::ICmpInst::Predicate pred = llvm::ICmpInst::Predicate(0);
        if (isUnsigned) {
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
        } else {
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
        }

        if (pred != 0) {
            llvm::Value* result = llvmBuilder.CreateICmp(pred, left, right);
            glaBuilder->setInstructionPrecision(result, precision);

            return result;
        }
    }

    return 0;
}

llvm::Value* TGlslangToTopTraverser::createUnaryOperation(TOperator op, gla::EMdPrecision precision, llvm::Value* operand)
{
    // Unary ops that map to llvm operations
    switch (op) {
    case EOpNegative:
        if (gla::IsAggregate(operand)) {
            // emulate by subtracting from 0.0
            llvm::Value* zero = gla::MakeFloatConstant(context, 0.0);

            return glaBuilder->createMatrixOp(precision, llvm::Instruction::FSub, zero, operand);
        }

        llvm::Value* result;
        if (gla::GetBasicTypeID(operand) == llvm::Type::FloatTyID)
            result = llvmBuilder.CreateFNeg(operand);
        else
            result = llvmBuilder.CreateNeg (operand);
        glaBuilder->setInstructionPrecision(result, precision);

        return result;

    case EOpLogicalNot:
    case EOpVectorLogicalNot:
    case EOpBitwiseNot:
        return llvmBuilder.CreateNot(operand);
    
    case EOpDeterminant:
        return glaBuilder->createMatrixDeterminant(precision, operand);
    case EOpMatrixInverse:
        return glaBuilder->createMatrixInverse(precision, operand);
    case EOpTranspose:
        return glaBuilder->createMatrixTranspose(precision, operand);
    }

    return 0;
}

llvm::Value* TGlslangToTopTraverser::createConversion(TOperator op, gla::EMdPrecision precision, llvm::Type* destType, llvm::Value* operand)
{
    llvm::Instruction::CastOps castOp = llvm::Instruction::CastOps(0);
    switch(op) {
    case EOpConvIntToBool:
    case EOpConvUintToBool:
    case EOpConvFloatToBool:
        {
            // any non-zero should return true
            llvm::Value* zero;
            if (op == EOpConvFloatToBool)
                zero = gla::MakeFloatConstant(context, 0.0f);
            else
                zero = gla::MakeIntConstant(context, 0);

            if (gla::GetComponentCount(operand) > 1)
                zero = glaBuilder->smearScalar(gla::EMpNone, zero, operand->getType());

            return createBinaryOperation(EOpNotEqual, precision, operand, zero, false, false);
        }

    case EOpConvIntToFloat:
        castOp = llvm::Instruction::SIToFP;
        break;
    case EOpConvBoolToFloat:
        castOp = llvm::Instruction::UIToFP;
        break;
    case EOpConvUintToFloat:
        castOp = llvm::Instruction::UIToFP;
        break;

    case EOpConvFloatToInt:
        castOp = llvm::Instruction::FPToSI;
        break;
    case EOpConvBoolToInt:
        // GLSL says true is converted to 1
        castOp = llvm::Instruction::ZExt;
        break;
    case EOpConvUintToInt:

        return operand;

    case EOpConvBoolToUint:
        // GLSL says true is converted to 1
        castOp = llvm::Instruction::ZExt;
        break;
    case EOpConvFloatToUint:
        castOp = llvm::Instruction::FPToUI;
        break;
    case EOpConvIntToUint:

        return operand;

    case EOpConvDoubleToInt:
    case EOpConvDoubleToBool:
    case EOpConvDoubleToFloat:
    case EOpConvDoubleToUint:
    case EOpConvIntToDouble:
    case EOpConvUintToDouble:
    case EOpConvFloatToDouble:
    case EOpConvBoolToDouble:
        gla::UnsupportedFunctionality("double conversion");
        break;
    }

    if (castOp == 0)

        return 0;

    llvm::Value* result = llvmBuilder.CreateCast(castOp, operand, destType);
    glaBuilder->setInstructionPrecision(result, precision);

    return result;
}

llvm::Value* TGlslangToTopTraverser::createUnaryIntrinsic(TOperator op, gla::EMdPrecision precision, llvm::Value* operand)
{
    // Unary ops that require an intrinsic
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
    case EOpTrunc:
        intrinsicID = llvm::Intrinsic::gla_fRoundZero;
        break;
    case EOpRound:
        intrinsicID = llvm::Intrinsic::gla_fRoundFast;
        break;
    case EOpRoundEven:
        intrinsicID = llvm::Intrinsic::gla_fRoundEven;
        break;
    case EOpCeil:
        intrinsicID = llvm::Intrinsic::gla_fCeiling;
        break;
    case EOpFract:
        intrinsicID = llvm::Intrinsic::gla_fFraction;
        break;

    case EOpIsNan:
        intrinsicID = llvm::Intrinsic::gla_fIsNan;
        break;
    case EOpIsInf:
        intrinsicID = llvm::Intrinsic::gla_fIsInf;
        break;

    case EOpFloatBitsToInt:
    case EOpFloatBitsToUint:
        intrinsicID = llvm::Intrinsic::gla_fFloatBitsToInt;
        break;
    case EOpIntBitsToFloat:
    case EOpUintBitsToFloat:
        intrinsicID = llvm::Intrinsic::gla_fIntBitsTofloat;
        break;
    case EOpPackSnorm2x16:
        intrinsicID = llvm::Intrinsic::gla_fPackSnorm2x16;
        break;
    case EOpUnpackSnorm2x16:
        intrinsicID = llvm::Intrinsic::gla_fUnpackSnorm2x16;
        break;
    case EOpPackUnorm2x16:
        intrinsicID = llvm::Intrinsic::gla_fPackUnorm2x16;
        break;
    case EOpUnpackUnorm2x16:
        intrinsicID = llvm::Intrinsic::gla_fUnpackUnorm2x16;
        break;
    case EOpPackHalf2x16:
        intrinsicID = llvm::Intrinsic::gla_fPackHalf2x16;
        break;
    case EOpUnpackHalf2x16:
        intrinsicID = llvm::Intrinsic::gla_fUnpackHalf2x16;
        break;

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
        if (gla::GetBasicTypeID(operand) == llvm::Type::FloatTyID)
            intrinsicID = llvm::Intrinsic::gla_fAbs;
        else
            intrinsicID = llvm::Intrinsic::gla_abs;
        break;
    case EOpSign:
        if (gla::GetBasicTypeID(operand) == llvm::Type::FloatTyID)
            intrinsicID = llvm::Intrinsic::gla_fSign;
        else
            intrinsicID = llvm::Intrinsic::gla_sign;
        break;
    }

    if (intrinsicID != 0)
        return glaBuilder->createIntrinsicCall(precision, intrinsicID, operand);

    return 0;
}

llvm::Value* TGlslangToTopTraverser::createIntrinsic(TOperator op, gla::EMdPrecision precision, std::vector<llvm::Value*>& operands, bool isUnsigned)
{
    // Binary ops that require an intrinsic
    llvm::Value* result = 0;
    llvm::Intrinsic::ID intrinsicID = llvm::Intrinsic::ID(0);

    switch (op) {
    case EOpMin:
        if (gla::GetBasicTypeID(operands.front()) == llvm::Type::FloatTyID)
            intrinsicID = llvm::Intrinsic::gla_fMin;
        else if (isUnsigned)
            intrinsicID = llvm::Intrinsic::gla_uMin;
        else
            intrinsicID = llvm::Intrinsic::gla_sMin;
        break;
    case EOpMax:
        if (gla::GetBasicTypeID(operands.front()) == llvm::Type::FloatTyID)
            intrinsicID = llvm::Intrinsic::gla_fMax;
        else if (isUnsigned)
            intrinsicID = llvm::Intrinsic::gla_uMax;
        else
            intrinsicID = llvm::Intrinsic::gla_sMax;
        break;
    case EOpPow:
        if (gla::GetBasicTypeID(operands.front()) == llvm::Type::FloatTyID)
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
        if (gla::GetBasicTypeID(operands.front()) == llvm::Type::FloatTyID)
            intrinsicID = llvm::Intrinsic::gla_fClamp;
        else if (isUnsigned)
            intrinsicID = llvm::Intrinsic::gla_uClamp;
        else
            intrinsicID = llvm::Intrinsic::gla_sClamp;
        break;
    case EOpMix:
        if (gla::GetBasicTypeID(operands.back()) == llvm::Type::IntegerTyID)
            intrinsicID = llvm::Intrinsic::gla_fbMix;
        else
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
    case EOpModf:
        intrinsicID = llvm::Intrinsic::gla_fModF;
        break;
    }

    // If intrinsic was assigned, then call the function and return
    if (intrinsicID != 0) {
        switch (operands.size()) {
        case 0:
            result = glaBuilder->createIntrinsicCall(precision, intrinsicID);
            break;
        case 1:
            // should all be handled by createUnaryIntrinsic
            assert(0);
            break;
        case 2:
            result = glaBuilder->createIntrinsicCall(precision, intrinsicID, operands[0], operands[1]);
            break;
        case 3:
            result = glaBuilder->createIntrinsicCall(precision, intrinsicID, operands[0], operands[1], operands[2]);
            break;
        default:
            // These do not exist yet
            assert(0 && "intrinsic with more than 3 operands");
        }
    }

    return result;
}

void TGlslangToTopTraverser::createPipelineRead(TIntermSymbol* node, llvm::Value* storage, int firstSlot, llvm::MDNode* md)
{
    gla::EInterpolationMethod method = gla::EIMNone;
    if (node->getType().getQualifier().nopersp)
        method = gla::EIMNoperspective;
    else if (node->getType().getQualifier().smooth)
        method = gla::EIMSmooth;

    gla::EInterpolationLocation location = gla::EILFragment;
    if (node->getType().getQualifier().sample)
        location = gla::EILSample;
    else if (node->getType().getQualifier().centroid)
        location = gla::EILCentroid;

    // For pipeline inputs, and we will generate a fresh pipeline read at each reference,
    // which gets optimized later.
    std::string name(node->getSymbol().c_str());
    llvm::Type* readType;
    llvm::Value* pipeRead;

    if (node->getType().getStruct())
        gla::UnsupportedFunctionality("pipeline structure input");
    else if (node->getType().isArray() || node->getType().isMatrix()) {

        // Could be a matrix, an array, or an array of matrices.
        // The whole thing will be read, one slot at a time.

        int arraySize = 1;
        int numSlots = 1;
        if (node->getType().isArray())
            arraySize = node->getType().getArraySize();
        if (arraySize == 0) {
            // TODO: linker functionality: make sure front end knows size before calling here, see
            // comment in convertGlslangToGlaType
            arraySize = UnknownArraySize;
        }

        int numColumns = 1;
        if (node->getType().isMatrix())
            numColumns = node->getType().getMatrixCols();            
        
        // Get down to what slice of this type will be held 
        // in a single slot.
        TType slotType(node->getType());
        if (node->getType().isArray())
            slotType.dereference();
        if (node->getType().isMatrix())
            slotType.dereference();
        readType = convertGlslangToGlaType(slotType);

        // fill in the whole aggregate shadow, slot by slot
        std::vector<llvm::Value*> gepChain;
        gepChain.push_back(gla::MakeIntConstant(context, 0));
        int slot = firstSlot;
        for (int element = 0; element < arraySize; ++element) {
            if (node->getType().isArray())
                gepChain.push_back(gla::MakeIntConstant(context, element));

            for (int column = 0; column < numColumns; ++column, ++slot) {
                if (node->getType().isMatrix())
                    gepChain.push_back(gla::MakeIntConstant(context, slot - firstSlot));
                
                pipeRead = glaBuilder->readPipeline(getMdPrecision(node->getType()), readType, name, slot, md, -1 /*mask*/, method, location);
                llvmBuilder.CreateStore(pipeRead, glaBuilder->createGEP(storage, gepChain));
                
                if (node->getType().isMatrix())
                    gepChain.pop_back();
            }

            if (node->getType().isArray())
                gepChain.pop_back();
        }
    } else {
        readType = convertGlslangToGlaType(node->getType());
        llvm::Value* pipeRead = glaBuilder->readPipeline(getMdPrecision(node->getType()), readType, name, firstSlot, md, -1 /*mask*/, method, location);
        llvmBuilder.CreateStore(pipeRead, storage);
    }
}

int TGlslangToTopTraverser::assignSlot(TIntermSymbol* node, bool input)
{
    int numSlots = 1;
    if (node->getType().isArray()) {
        numSlots = node->getType().getArraySize();
        if (numSlots == 0)
            numSlots = UnknownArraySize;
    }

    // Get the index for this interpolant, or create a new unique one
    int slot;
    if (node->getQualifier().hasLocation()) {
        slot = node->getQualifier().layoutSlotLocation;
        
        return slot;
    }

    // Not found in the symbol, see if we've assigned one before

    std::map<std::string, int>::iterator iter;
    const char* name = node->getSymbol().c_str();
    iter = slotMap.find(name);

    if (slotMap.end() == iter) {
        slotMap[name] = nextSlot;
        nextSlot += numSlots;
    }

    return slotMap[name];
}

llvm::Value* TGlslangToTopTraverser::createLLVMConstant(const TType& glslangType, constUnion *consts, int& nextConst)
{
    // vector of constants for LLVM
    std::vector<llvm::Constant*> llvmConsts;

    // Type is used for struct and array constants
    llvm::Type* type = convertGlslangToGlaType(glslangType);

    if (glslangType.isArray()) {
        TType elementType(glslangType);
        elementType.dereference();
        for (int i = 0; i < glslangType.getArraySize(); ++i)
            llvmConsts.push_back(llvm::dyn_cast<llvm::Constant>(createLLVMConstant(elementType, consts, nextConst)));
    } else if (glslangType.isMatrix()) {
        TType vectorType(glslangType);
        vectorType.dereference();
        for (int col = 0; col < glslangType.getMatrixCols(); ++col)
            llvmConsts.push_back(llvm::dyn_cast<llvm::Constant>(createLLVMConstant(vectorType, consts, nextConst)));
    } else if (glslangType.getStruct()) {
        TVector<TTypeLine>::iterator iter;
        for (iter = glslangType.getStruct()->begin(); iter != glslangType.getStruct()->end(); ++iter)
            llvmConsts.push_back(llvm::dyn_cast<llvm::Constant>(createLLVMConstant(*iter->type, consts, nextConst)));
    } else {
        // a vector or scalar, both will work the same way
        // this is where we actually consume the constants, rather than walk a tree

        for (unsigned int i = 0; i < glslangType.getVectorSize(); ++i) {
            switch(consts[nextConst].getType()) {
            case EbtInt:
                llvmConsts.push_back(gla::MakeIntConstant(context, consts[nextConst].getIConst()));
                break;
            case EbtUint:
                llvmConsts.push_back(gla::MakeUnsignedConstant(context, consts[nextConst].getUConst()));
                break;
            case EbtFloat:
                llvmConsts.push_back(gla::MakeFloatConstant(context, consts[nextConst].getFConst()));
                break;
            case EbtDouble:
                llvmConsts.push_back(gla::MakeFloatConstant(context, consts[nextConst].getDConst()));
                break;
            case EbtBool:
                llvmConsts.push_back(gla::MakeBoolConstant(context, consts[nextConst].getBConst()));
                break;
            default:
                gla::UnsupportedFunctionality("scalar or vector element type");
            }
            ++nextConst;
        }
    }

    return glaBuilder->getConstant(llvmConsts, type);
}

void TGlslangToTopTraverser::setAccessChainMetadata(TIntermSymbol* node, llvm::Value* typeProxy)
{
    llvm::MDNode* md;
    const std::string name = node->getSymbol().c_str();
    md = uniformMdMap[name];

    gla::EMdInputOutput ioType = getMdQualifier(node);
    switch (ioType) {
    case gla::EMioDefaultUniform:
        if (md == 0) {
            md = declareMdDefaultUniform(node, typeProxy);
            uniformMdMap[name] = md;
        }
        break;
    case gla::EMioUniformBlockMember:
    case gla::EMioBufferBlockMember:
        if (md == 0) {
            md = declareMdUniformBlock(ioType, node, typeProxy);
            uniformMdMap[name] = md;
        }
        break;
    default:
        break;
    }

    if (md)
        glaBuilder->setAccessChainMetadata(gla::UniformMdName, md);
}

// Make a !gla.uniform node, as per metadata.h, for a default uniform
llvm::MDNode* TGlslangToTopTraverser::declareMdDefaultUniform(TIntermSymbol* node, llvm::Value* typeProxy)
{
    const TType& type = node->getType();
    llvm::MDNode* samplerMd = makeMdSampler(type, typeProxy);

    // Create hierarchical type information if it's an aggregate
    gla::EMdTypeLayout layout = getMdTypeLayout(type);
    llvm::MDNode* structure = 0;
    if (layout == gla::EMtlAggregate)
        structure = declareMdType(type);

    // Make the main node
    return metadata.makeMdInputOutput(node->getSymbol().c_str(), gla::UniformListMdName, gla::EMioDefaultUniform, 
                                      makePermanentTypeProxy(typeProxy),
                                      layout, getMdPrecision(type), 0, samplerMd, structure);
}

llvm::MDNode* TGlslangToTopTraverser::makeMdSampler(const TType& type, llvm::Value* typeProxy)
{
    // Figure out sampler information, if it's a sampler
    if (type.getBasicType() == EbtSampler) {
        if (! typeProxy) {    
            // TODO: memory: who/how owns tracking and deleting this allocation?
            typeProxy = new llvm::GlobalVariable(convertGlslangToGlaType(type), true, llvm::GlobalVariable::ExternalLinkage, 0, "sampler_typeProxy");
        }

        return metadata.makeMdSampler(getMdSampler(type), typeProxy, getMdSamplerDim(type), type.getSampler().arrayed,
                                      type.getSampler().shadow, getMdSamplerBaseType(type.getSampler().type));
    } else
        return 0;
}

// Make a !gla.uniform node, as per metadata.h, for a uniform block or buffer block (depending on ioType)
llvm::MDNode* TGlslangToTopTraverser::declareMdUniformBlock(gla::EMdInputOutput ioType, const TIntermSymbol* node, llvm::Value* typeProxy)
{
    const TType& type = node->getType();
    const char* name;
    if (node->getSymbol().substr(0,6) == "__anon")
        name = "";
    else
        name = node->getSymbol().c_str();

    // Make hierachical type information
    llvm::MDNode* block = declareMdType(type);

    // Make the main node
    return metadata.makeMdInputOutput(name, gla::UniformListMdName, ioType, typeProxy,
                                      getMdTypeLayout(type), getMdPrecision(type), 0, 0, block);
}

// Make a !type node as per metadata.h, recursively
llvm::MDNode* TGlslangToTopTraverser::declareMdType(const TType& type)
{
    // Figure out sampler information if it's a sampler
    llvm::MDNode* samplerMd = makeMdSampler(type, 0);

    std::vector<llvm::Value*> mdArgs;

    // name of aggregate, if an aggregate (struct or block)
    if (type.getStruct())
        mdArgs.push_back(llvm::MDString::get(context, type.getTypeName().c_str()));
    else
        mdArgs.push_back(llvm::MDString::get(context, ""));

    // !typeLayout
    mdArgs.push_back(metadata.makeMdTypeLayout(getMdTypeLayout(type), getMdPrecision(type), type.getQualifier().layoutSlotLocation, samplerMd));

    const TTypeList* typeList = type.getStruct();
    if (typeList) {
        for (int t = 0; t < typeList->size(); ++t) {
            // name of member
            const TType* fieldType = (*typeList)[t].type;
            mdArgs.push_back(llvm::MDString::get(context, fieldType->getFieldName().c_str()));
            
            // type of member
            llvm::MDNode* mdType = declareMdType(*fieldType);
            mdArgs.push_back(mdType);
        }
    }

    return llvm::MDNode::get(context, mdArgs);
}

void TGlslangToTopTraverser::setOutputMetadata(TIntermSymbol* node, llvm::Value* storage, int slot)
{    
    llvm::MDNode* md = metadata.makeMdInputOutput(node->getSymbol().c_str(), gla::OutputListMdName, getMdQualifier(node), 
                                                  makePermanentTypeProxy(storage), getMdTypeLayout(node->getType()), getMdPrecision(node->getType()), slot);
    glaBuilder->setOutputMetadata(storage, md, slot);
}

llvm::MDNode* TGlslangToTopTraverser::makeInputMetadata(TIntermSymbol* node, llvm::Value* typeProxy, int slot)
{    
    return metadata.makeMdInputOutput(node->getSymbol().c_str(), gla::InputListMdName, getMdQualifier(node), 
                                      makePermanentTypeProxy(typeProxy), getMdTypeLayout(node->getType()),
                                      getMdPrecision(node->getType()), slot);
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
    it.visitSwitch = TranslateSwitch;
    it.visitSymbol = TranslateSymbol;
    it.visitUnary = TranslateUnary;
    it.visitLoop = TranslateLoop;
    it.visitBranch = TranslateBranch;

    root->traverse(&it);
}
