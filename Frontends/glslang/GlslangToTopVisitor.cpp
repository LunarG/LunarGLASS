//===- GlslangToTop.cpp - Translate GLSL IR to LunarGLASS Top IR ---------===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (C) 2010-2014 LunarG, Inc.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 
//     Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.
// 
//     Redistributions in binary form must reproduce the above
//     copyright notice, this list of conditions and the following
//     disclaimer in the documentation and/or other materials provided
//     with the distribution.
// 
//     Neither the name of LunarG Inc. nor the names of its
//     contributors may be used to endorse or promote products derived
//     from this software without specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
// LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
// ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
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
#pragma warning(push, 1)
#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Transforms/Scalar.h"
#pragma warning(pop)

#include <string>
#include <map>
#include <list>
#include <vector>
#include <stack>

// Adapter includes
#include "GlslangToTopVisitor.h"

//
// Use this class to carry along data from node to node in the traversal
//
class TGlslangToTopTraverser : public glslang::TIntermTraverser {
public:
    TGlslangToTopTraverser(gla::Manager*);
    virtual ~TGlslangToTopTraverser();

    bool visitAggregate(glslang::TVisit, glslang::TIntermAggregate*);
    bool visitBinary(glslang::TVisit, glslang::TIntermBinary*);
    void visitConstantUnion(glslang::TIntermConstantUnion*);
    bool visitSelection(glslang::TVisit, glslang::TIntermSelection*);
    bool visitSwitch(glslang::TVisit, glslang::TIntermSwitch*);
    void visitSymbol(glslang::TIntermSymbol* symbol);
    bool visitUnary(glslang::TVisit, glslang::TIntermUnary*);
    bool visitLoop(glslang::TVisit, glslang::TIntermLoop*);
    bool visitBranch(glslang::TVisit visit, glslang::TIntermBranch*);

protected:
    llvm::Value* createLLVMVariable(const glslang::TIntermSymbol*);
    llvm::Type* convertGlslangToGlaType(const glslang::TType& type);

    bool isShaderEntrypoint(const glslang::TIntermAggregate* node);
    void makeFunctions(const glslang::TIntermSequence&);
    void handleFunctionEntry(const glslang::TIntermAggregate* node);
    void translateArguments(const glslang::TIntermSequence& glslangArguments, std::vector<llvm::Value*>& arguments);
    llvm::Value* handleBuiltinFunctionCall(const glslang::TIntermAggregate*);
    llvm::Value* handleUserFunctionCall(const glslang::TIntermAggregate*);

    llvm::Value* createBinaryOperation(glslang::TOperator op, gla::EMdPrecision, llvm::Value* left, llvm::Value* right, bool isUnsigned, bool reduceComparison = true);
    llvm::Value* createUnaryOperation(glslang::TOperator op, gla::EMdPrecision, llvm::Value* operand);
    llvm::Value* createConversion(glslang::TOperator op, gla::EMdPrecision, llvm::Type*, llvm::Value* operand);
    llvm::Value* createUnaryIntrinsic(glslang::TOperator op, gla::EMdPrecision, llvm::Value* operand);
    llvm::Value* createIntrinsic(glslang::TOperator op, gla::EMdPrecision, std::vector<llvm::Value*>& operands, bool isUnsigned);
    void createPipelineRead(glslang::TIntermSymbol*, llvm::Value* storage, int slot, llvm::MDNode*);
    void createPipelineSubread(const glslang::TType& glaType, llvm::Value* storage, std::vector<llvm::Value*>& gepChain, int& slot, llvm::MDNode* md,
                               std::string& name, gla::EInterpolationMethod, gla::EInterpolationLocation);
    int assignSlot(glslang::TIntermSymbol* node, bool input);
    llvm::Value* getSymbolStorage(const glslang::TIntermSymbol* node, bool& firstTime);
    llvm::Value* createLLVMConstant(const glslang::TType& type, const glslang::TConstUnionArray&, int& nextConst);
    llvm::MDNode* declareUniformMetadata(glslang::TIntermSymbol* node, llvm::Value*);
    llvm::MDNode* declareMdDefaultUniform(glslang::TIntermSymbol*, llvm::Value*);
    llvm::MDNode* makeMdSampler(const glslang::TType&, llvm::Value*);
    llvm::MDNode* declareMdUniformBlock(gla::EMdInputOutput ioType, const glslang::TIntermSymbol* node, llvm::Value*);
    llvm::MDNode* declareMdType(const glslang::TType&);
    llvm::MDNode* makeInputOutputMetadata(glslang::TIntermSymbol* node, llvm::Value*, int slot, const char* kind);
    void setOutputMetadata(glslang::TIntermSymbol* node, llvm::Value*, int slot);
    llvm::MDNode* makeInputMetadata(glslang::TIntermSymbol* node, llvm::Value*, int slot);

    llvm::LLVMContext &context;
    llvm::BasicBlock* shaderEntry;
    llvm::IRBuilder<> llvmBuilder;
    llvm::Module* module;
    gla::Metadata metadata;

    gla::Builder* glaBuilder;
    int nextSlot;                // non-user set interpolations slots, virtual space, so inputs and outputs can both share it
    bool inMain;
    bool mainTerminated;
    bool linkageOnly;

    std::map<int, llvm::Value*> symbolValues;
    std::map<std::string, llvm::Function*> functionMap;
    std::map<std::string, int> slotMap;
    std::map<int, llvm::MDNode*> inputMdMap;
    std::map<std::string, llvm::MDNode*> uniformMdMap;
    std::map<glslang::TTypeList*, llvm::StructType*> structMap;
    std::stack<bool> breakForLoop;  // false means break for switch
};

namespace {

// Helper functions for translating glslang to metadata, so that information
// not representable in LLVM does not get lost.

gla::EMdInputOutput GetMdQualifier(glslang::TIntermSymbol* node)
{
    gla::EMdInputOutput mdQualifier;
    switch (node->getQualifier().storage) {

    // inputs
    case glslang::EvqVertexId:   mdQualifier = gla::EMioVertexId;        break;
    case glslang::EvqInstanceId: mdQualifier = gla::EMioInstanceId;      break;
    case glslang::EvqFace:       mdQualifier = gla::EMioFragmentFace;    break;
    case glslang::EvqPointCoord: mdQualifier = gla::EMioPointCoord;      break;
    case glslang::EvqFragCoord:  mdQualifier = gla::EMioFragmentCoord;   break;
    case glslang::EvqVaryingIn:  mdQualifier = gla::EMioPipeIn;          break;

    // outputs
    case glslang::EvqPosition:   mdQualifier = gla::EMioVertexPosition;    break;
    case glslang::EvqPointSize:  mdQualifier = gla::EMioPointSize;         break;
    case glslang::EvqClipVertex: mdQualifier = gla::EMioClipVertex;        break;
    case glslang::EvqVaryingOut: mdQualifier = gla::EMioPipeOut;           break;
    case glslang::EvqFragColor:  mdQualifier = gla::EMioPipeOut;           break;
    case glslang::EvqFragDepth:  mdQualifier = gla::EMioFragmentDepth;     break;

    // uniforms
    case glslang::EvqBuffer:     mdQualifier = gla::EMioBufferBlockMember; break;
    case glslang::EvqUniform:    
                    if (node->getType().getBasicType() == glslang::EbtBlock)
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

gla::EMdTypeLayout GetMdTypeLayout(const glslang::TType& type)
{
    gla::EMdTypeLayout mdType;

    if (type.isMatrix()) {
        switch (type.getQualifier().layoutMatrix) {
        case glslang::ElmRowMajor: mdType = gla::EMtlRowMajorMatrix;   break;
        default:          mdType = gla::EMtlColMajorMatrix;   break;
        }
    } else {
        switch (type.getBasicType()) {
        case glslang::EbtSampler:  mdType = gla::EMtlSampler;    break;
        case glslang::EbtStruct:   mdType = gla::EMtlAggregate;  break;
        case glslang::EbtUint:     mdType = gla::EMtlUnsigned;   break;
        case glslang::EbtBlock:
            switch (type.getQualifier().layoutPacking) {
            case glslang::ElpShared:  return gla::EMtlShared;
            case glslang::ElpStd140:  return gla::EMtlStd140;
            case glslang::ElpStd430:  return gla::EMtlStd430;
            case glslang::ElpPacked:  return gla::EMtlPacked;
            default:
                gla::UnsupportedFunctionality("block layout", gla::EATContinue);
                return gla::EMtlShared;
            }

        default:          mdType = gla::EMtlNone;       break;
        }
    }

    return mdType;
}

gla::EMdSampler GetMdSampler(const glslang::TType& type)
{
    if (type.getSampler().image)
        return gla::EMsImage;
    else
        return gla::EMsTexture;
}

gla::EMdSamplerDim GetMdSamplerDim(const glslang::TType& type)
{
    switch (type.getSampler().dim) {
    case glslang::Esd1D:     return gla::EMsd1D;
    case glslang::Esd2D:     return gla::EMsd2D;
    case glslang::Esd3D:     return gla::EMsd3D;
    case glslang::EsdCube:   return gla::EMsdCube;
    case glslang::EsdRect:   return gla::EMsdRect;
    case glslang::EsdBuffer: return gla::EMsdBuffer;
    default:
        gla::UnsupportedFunctionality("unknown sampler dimension", gla::EATContinue);
        return gla::EMsd2D;
    }
}

gla::EMdSamplerBaseType GetMdSamplerBaseType(glslang::TBasicType type)
{
    switch (type) {
    case glslang::EbtFloat:    return gla::EMsbFloat;
    case glslang::EbtInt:      return gla::EMsbInt;
    case glslang::EbtUint:     return gla::EMsbUint;
    default:
        gla::UnsupportedFunctionality("base type of sampler return type", gla::EATContinue);
        return gla::EMsbFloat;
    }
}

int GetMdSlotLocation(const glslang::TType& type)
{
    if (type.getQualifier().layoutLocation == glslang::TQualifier::layoutLocationEnd)
        return gla::MaxUserLayoutLocation;
    else
        return type.getQualifier().layoutLocation;
}

gla::EMdPrecision GetMdPrecision(const glslang::TType& type)
{
    switch (type.getQualifier().precision) {
    case glslang::EpqNone:    return gla::EMpNone;
    case glslang::EpqLow:     return gla::EMpLow;
    case glslang::EpqMedium:  return gla::EMpMedium;
    case glslang::EpqHigh:    return gla::EMpHigh;
    default:         return gla::EMpNone;
    }
}

llvm::Value* MakePermanentTypeProxy(llvm::Value* value)
{
    // Make a type proxy that won't be optimized away (we still want the real llvm::Value to get optimized away when it can)
    llvm::Type* type = value->getType();
    while (type->getTypeID() == llvm::Type::PointerTyID)
        type = llvm::dyn_cast<llvm::PointerType>(type)->getContainedType(0);

    // TODO: memory: who/how owns tracking and deleting this allocation?
    return new llvm::GlobalVariable(type, true, llvm::GlobalVariable::ExternalLinkage, 0, value->getName() + "_typeProxy");
}

void GetInterpolationLocationMethod(const glslang::TType& type, gla::EInterpolationMethod& method, gla::EInterpolationLocation& location)
{
    method = gla::EIMNone;
    if (type.getQualifier().nopersp)
        method = gla::EIMNoperspective;
    else if (type.getQualifier().smooth)
        method = gla::EIMSmooth;

    location = gla::EILFragment;
    if (type.getQualifier().sample)
        location = gla::EILSample;
    else if (type.getQualifier().centroid)
        location = gla::EILCentroid;
}

};  // end anonymous namespace


// A fully functioning front end will know all array sizes,
// this is just a back up size.
const int UnknownArraySize = 8;

TGlslangToTopTraverser::TGlslangToTopTraverser(gla::Manager* manager)
    : TIntermTraverser(true, false, true),
      context(llvm::getGlobalContext()), shaderEntry(0), llvmBuilder(context),
      module(manager->getModule()), metadata(context, module),
      nextSlot(gla::MaxUserLayoutLocation), inMain(false), mainTerminated(false), linkageOnly(false)
{
    // do this after the builder knows the module
    glaBuilder = new gla::Builder(llvmBuilder, manager, metadata);
    glaBuilder->clearAccessChain();
    glaBuilder->setAccessChainDirectionRightToLeft(false);

    shaderEntry = glaBuilder->makeMain();
    llvmBuilder.SetInsertPoint(shaderEntry);
}

TGlslangToTopTraverser::~TGlslangToTopTraverser()
{
    if (! mainTerminated) {            
        llvm::BasicBlock* lastMainBlock = &shaderEntry->getParent()->getBasicBlockList().back();
        llvmBuilder.SetInsertPoint(lastMainBlock);
        glaBuilder->leaveFunction(true);
    }

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
// Uniforms, inputs, and outputs also declare metadata for future linker consumption.
//
// Sort out what the deal is...
//
void TGlslangToTopTraverser::visitSymbol(glslang::TIntermSymbol* symbol)
{
    bool input = symbol->getType().getQualifier().isPipeInput();
    bool output = symbol->getType().getQualifier().isPipeOutput();
    bool uniform = symbol->getType().getQualifier().isUniform();

    // Normal symbols and uniforms need a variable allocated to them,
    // we will shadow inputs by reading them in whole into a global variables, 
    // and outputs are shadowed for read/write optimizations before being written out,
    // so everything gets a variable allocated; see if we've cached it.
    bool firstTime;
    llvm::Value* storage = getSymbolStorage(symbol, firstTime);
    if (firstTime && output) {
        // set up output metadata once for all future pipeline intrinsic writes
        int slot = assignSlot(symbol, input);
        setOutputMetadata(symbol, storage, slot);
    }
    
    // set up uniform metadata
    llvm::MDNode* mdNode = 0;
    if (uniform)
        mdNode = declareUniformMetadata(symbol, storage);

    if (! linkageOnly) {
        // Prepare to generate code for the access

        // L-value chains will be computed purely left to right, so now is "clear" time
        // (since we are on the symbol; the base of the expression, which is left-most)
        glaBuilder->clearAccessChain();

        // Track the current value
        glaBuilder->setAccessChainLValue(storage);

        // Set up metadata for uniform/sampler inputs
        if (uniform && mdNode)
            glaBuilder->setAccessChainMetadata(gla::UniformMdName, mdNode);

        // If it's an arrayed output, we also want to know which indices
        // are live.
        if (symbol->isArray() && output)
            glaBuilder->accessChainTrackOutputIndex();
    }

    if (input) {
        int slot = assignSlot(symbol, input);
        mdNode = makeInputMetadata(symbol, storage, slot);

        if (! linkageOnly) {
            // do the actual read
            createPipelineRead(symbol, storage, slot, mdNode);
        }
    }
}

bool TGlslangToTopTraverser::visitBinary(glslang::TVisit /* visit */, glslang::TIntermBinary* node)
{
    // First, handle special cases
    switch (node->getOp()) {
    case glslang::EOpAssign:
    case glslang::EOpAddAssign:
    case glslang::EOpSubAssign:
    case glslang::EOpMulAssign:
    case glslang::EOpVectorTimesMatrixAssign:
    case glslang::EOpVectorTimesScalarAssign:
    case glslang::EOpMatrixTimesScalarAssign:
    case glslang::EOpMatrixTimesMatrixAssign:
    case glslang::EOpDivAssign:
    case glslang::EOpModAssign:
    case glslang::EOpAndAssign:
    case glslang::EOpInclusiveOrAssign:
    case glslang::EOpExclusiveOrAssign:
    case glslang::EOpLeftShiftAssign:
    case glslang::EOpRightShiftAssign:
        // A bin-op assign "a += b" means the same thing as "a = a + b"
        // where a is evaluated before b. For a simple assignment, GLSL
        // says to evaluate the left before the right.  So, always, left
        // node then right node.
        {
            // get the left l-value, save it away
            glaBuilder->clearAccessChain();
            node->getLeft()->traverse(this);
            gla::Builder::AccessChain lValue = glaBuilder->getAccessChain();

            // evaluate the right
            glaBuilder->clearAccessChain();
            node->getRight()->traverse(this);
            llvm::Value* rValue = glaBuilder->accessChainLoad(GetMdPrecision(node->getRight()->getType()));

            if (node->getOp() != glslang::EOpAssign) {
                // the left is also an r-value
                glaBuilder->setAccessChain(lValue);
                llvm::Value* leftRValue = glaBuilder->accessChainLoad(GetMdPrecision(node->getLeft()->getType()));

                // do the operation
                rValue = createBinaryOperation(node->getOp(), GetMdPrecision(node->getType()), leftRValue, rValue, node->getType().getBasicType() == glslang::EbtUint);

                // these all need their counterparts in createBinaryOperation()
                assert(rValue);
            }

            // store the result
            glaBuilder->setAccessChain(lValue);
            glaBuilder->accessChainStore(rValue);

            // assignments are expressions having an rValue after they are evaluated...
            glaBuilder->clearAccessChain();
            glaBuilder->setAccessChainRValue(rValue);
        }
        return false;
    case glslang::EOpIndexDirect:
    case glslang::EOpIndexIndirect:
    case glslang::EOpIndexDirectStruct:
        {
            // this adapter is building access chains left to right
            // set up the access chain to the left
            node->getLeft()->traverse(this);

            if (! node->getLeft()->getType().isArray() &&
                  node->getLeft()->getType().isVector() &&
                  node->getOp() == glslang::EOpIndexDirect) {
                // this is essentially a hard-coded vector swizzle of size 1,
                // so short circuit the GEP stuff with a swizzle
                std::vector<int> swizzle;
                swizzle.push_back(node->getRight()->getAsConstantUnion()->getConstArray()[0].getIConst());
                glaBuilder->accessChainPushSwizzleRight(swizzle, convertGlslangToGlaType(node->getType()),
                                                             node->getLeft()->getVectorSize());
            } else {
                // struct or array or indirection into a vector; will use native LLVM gep
                // matrices are arrays of vectors, so will also work for a matrix

                // save it so that computing the right side doesn't trash it
                gla::Builder::AccessChain partial = glaBuilder->getAccessChain();

                // compute the next index
                glaBuilder->clearAccessChain();
                node->getRight()->traverse(this);
                llvm::Value* index = glaBuilder->accessChainLoad(GetMdPrecision(node->getRight()->getType()));

                // make the new access chain to date
                glaBuilder->setAccessChain(partial);
                glaBuilder->accessChainPushLeft(index);
            }
        }
        return false;
    case glslang::EOpVectorSwizzle:
        {
            node->getLeft()->traverse(this);
            glslang::TIntermSequence& swizzleSequence = node->getRight()->getAsAggregate()->getSequence();
            std::vector<int> swizzle;
            for (int i = 0; i < (int)swizzleSequence.size(); ++i)
                swizzle.push_back(swizzleSequence[i]->getAsConstantUnion()->getConstArray()[0].getIConst());
            glaBuilder->accessChainPushSwizzleRight(swizzle, convertGlslangToGlaType(node->getType()),
                                                         node->getLeft()->getVectorSize());
        }
        return false;
    }

    // Assume generic binary op...

    // Get the operands
    glaBuilder->clearAccessChain();
    node->getLeft()->traverse(this);
    llvm::Value* left = glaBuilder->accessChainLoad(GetMdPrecision(node->getLeft()->getType()));

    glaBuilder->clearAccessChain();
    node->getRight()->traverse(this);
    llvm::Value* right = glaBuilder->accessChainLoad(GetMdPrecision(node->getRight()->getType()));

    llvm::Value* result;
    gla::EMdPrecision precision = GetMdPrecision(node->getType());

    switch (node->getOp()) {
    case glslang::EOpVectorTimesMatrix:
    case glslang::EOpMatrixTimesVector:
    case glslang::EOpMatrixTimesScalar:
    case glslang::EOpMatrixTimesMatrix:
        result = glaBuilder->createMatrixMultiply(precision, left, right);
        break;
    default:
        result = createBinaryOperation(node->getOp(), precision, left, right, node->getType().getBasicType() == glslang::EbtUint);
    }

    if (! result) {
        gla::UnsupportedFunctionality("glslang binary operation", gla::EATContinue);
    } else {
        glaBuilder->clearAccessChain();
        glaBuilder->setAccessChainRValue(result);

        return false;
    }

    return true;
}

bool TGlslangToTopTraverser::visitUnary(glslang::TVisit /* visit */, glslang::TIntermUnary* node)
{
    glaBuilder->clearAccessChain();
    node->getOperand()->traverse(this);
    llvm::Value* operand = glaBuilder->accessChainLoad(GetMdPrecision(node->getOperand()->getType()));

    gla::EMdPrecision precision = GetMdPrecision(node->getType());

    // it could be a conversion
    llvm::Value* result = createConversion(node->getOp(), precision, convertGlslangToGlaType(node->getType()), operand);

    // if not, then possibly an operation
    if (! result)
        result = createUnaryOperation(node->getOp(), precision, operand);

    // if not, then possibly a LunarGLASS intrinsic
    if (! result)
        result = createUnaryIntrinsic(node->getOp(), precision, operand);

    if (result) {
        glaBuilder->clearAccessChain();
        glaBuilder->setAccessChainRValue(result);

        return false; // done with this node
    }

    // it must be a special case, check...
    switch (node->getOp()) {
    case glslang::EOpPostIncrement:
    case glslang::EOpPostDecrement:
    case glslang::EOpPreIncrement:
    case glslang::EOpPreDecrement:
        {
            // we need the integer value "1" or the floating point "1.0" to add/subtract
            llvm::Value* one = gla::GetBasicTypeID(operand) == llvm::Type::FloatTyID ?
                                     gla::MakeFloatConstant(context, 1.0) :
                                     gla::MakeIntConstant(context, 1);
            glslang::TOperator op;
            if (node->getOp() == glslang::EOpPreIncrement ||
                node->getOp() == glslang::EOpPostIncrement)
                op = glslang::EOpAdd;
            else
                op = glslang::EOpSub;

            llvm::Value* result = createBinaryOperation(op, GetMdPrecision(node->getType()), operand, one, node->getType().getBasicType() == glslang::EbtUint);

            // The result of operation is always stored, but conditionally the
            // consumed result.  The consumed result is always an r-value.
            glaBuilder->accessChainStore(result);
            glaBuilder->clearAccessChain();
            if (node->getOp() == glslang::EOpPreIncrement ||
                node->getOp() == glslang::EOpPreDecrement)
                glaBuilder->setAccessChainRValue(result);
            else
                glaBuilder->setAccessChainRValue(operand);
        }
        return false;
    default:
        gla::UnsupportedFunctionality("glslang unary", gla::EATContinue);
    }

    return true;
}

bool TGlslangToTopTraverser::visitAggregate(glslang::TVisit visit, glslang::TIntermAggregate* node)
{
    llvm::Value* result;
    glslang::TOperator binOp = glslang::EOpNull;
    bool reduceComparison = true;
    bool isMatrix = false;

    assert(node->getOp());

    gla::EMdPrecision precision = GetMdPrecision(node->getType());

    switch (node->getOp()) {
    case glslang::EOpSequence:
        {
            // If this is the parent node of all the functions, we want to see them
            // early, so all call points have actual LLVM functions to reference.  
            // In all cases, still let the traverser visit the children for us.
            if (visit == glslang::EvPreVisit)
                makeFunctions(node->getAsAggregate()->getSequence());
        }

        return true;
    case glslang::EOpLinkerObjects:
        {
            if (visit == glslang::EvPreVisit)
                linkageOnly = true;
            else
                linkageOnly = false;
        }

        return true;
    case glslang::EOpComma:
        {
            // processing from left to right naturally leaves the right-most
            // lying around in the access chain
            glslang::TIntermSequence& glslangOperands = node->getSequence();
            for (int i = 0; i < (int)glslangOperands.size(); ++i)
                glslangOperands[i]->traverse(this);
        }

        return false;
    case glslang::EOpFunction:
        if (visit == glslang::EvPreVisit) {
            if (isShaderEntrypoint(node)) {
                inMain = true;
                llvmBuilder.SetInsertPoint(shaderEntry);
                metadata.addMdEntrypoint("main");
            } else {
                handleFunctionEntry(node);
            }
        } else {
            if (inMain)
                mainTerminated = true;
            glaBuilder->leaveFunction(inMain);
            inMain = false;

            // earlier code between functions could have had flow control, so bump up shader entry
            // to the end of that code, ready for the next one
            llvm::BasicBlock* lastMainBlock = &shaderEntry->getParent()->getBasicBlockList().back();
            llvmBuilder.SetInsertPoint(lastMainBlock);
        }

        return true;
    case glslang::EOpParameters:
        // Parameters will have been consumed by EOpFunction processing, but not
        // the body, so we still visited the function node's children, making this
        // child redundant.
        return false;
    case glslang::EOpFunctionCall:
        {
            if (node->isUserDefined())
                result = handleUserFunctionCall(node);
            else
                result = handleBuiltinFunctionCall(node);

            if (! result)
                gla::UnsupportedFunctionality("glslang function call", gla::EATContinue);
            else {
                glaBuilder->clearAccessChain();
                glaBuilder->setAccessChainRValue(result);

                return false;
            }
        }

        return true;
    case glslang::EOpConstructMat2x2:
    case glslang::EOpConstructMat2x3:
    case glslang::EOpConstructMat2x4:
    case glslang::EOpConstructMat3x2:
    case glslang::EOpConstructMat3x3:
    case glslang::EOpConstructMat3x4:
    case glslang::EOpConstructMat4x2:
    case glslang::EOpConstructMat4x3:
    case glslang::EOpConstructMat4x4:
    case glslang::EOpConstructDMat2x2:
    case glslang::EOpConstructDMat2x3:
    case glslang::EOpConstructDMat2x4:
    case glslang::EOpConstructDMat3x2:
    case glslang::EOpConstructDMat3x3:
    case glslang::EOpConstructDMat3x4:
    case glslang::EOpConstructDMat4x2:
    case glslang::EOpConstructDMat4x3:
    case glslang::EOpConstructDMat4x4:
        isMatrix = true;
        // fall through
    case glslang::EOpConstructFloat:
    case glslang::EOpConstructVec2:
    case glslang::EOpConstructVec3:
    case glslang::EOpConstructVec4:
    case glslang::EOpConstructDouble:
    case glslang::EOpConstructDVec2:
    case glslang::EOpConstructDVec3:
    case glslang::EOpConstructDVec4:
    case glslang::EOpConstructBool:
    case glslang::EOpConstructBVec2:
    case glslang::EOpConstructBVec3:
    case glslang::EOpConstructBVec4:
    case glslang::EOpConstructInt:
    case glslang::EOpConstructIVec2:
    case glslang::EOpConstructIVec3:
    case glslang::EOpConstructIVec4:
    case glslang::EOpConstructUint:
    case glslang::EOpConstructUVec2:
    case glslang::EOpConstructUVec3:
    case glslang::EOpConstructUVec4:
    case glslang::EOpConstructStruct:
        {
            std::vector<llvm::Value*> arguments;
            translateArguments(node->getSequence(), arguments);
            llvm::Value* constructed = glaBuilder->createVariable(gla::Builder::ESQLocal, 0,
                                                                        convertGlslangToGlaType(node->getType()),
                                                                        0, 0, "constructed");
            if (node->getOp() == glslang::EOpConstructStruct || node->getType().isArray()) {
                //TODO: clean up: is there a more direct way to set a whole LLVM structure?
                //                if not, move this inside Top Builder; too many indirections

                std::vector<llvm::Value*> gepChain;
                gepChain.push_back(gla::MakeIntConstant(context, 0));
                for (int field = 0; field < (int)arguments.size(); ++field) {
                    gepChain.push_back(gla::MakeIntConstant(context, field));
                    llvmBuilder.CreateStore(arguments[field], glaBuilder->createGEP(constructed, gepChain));
                    gepChain.pop_back();
                }
                glaBuilder->clearAccessChain();
                glaBuilder->setAccessChainLValue(constructed);
            } else {
                constructed = glaBuilder->createLoad(constructed);
                if (isMatrix)
                    constructed = glaBuilder->createMatrixConstructor(precision, arguments, constructed);
                else
                    constructed = glaBuilder->createConstructor(precision, arguments, constructed);
                glaBuilder->clearAccessChain();
                glaBuilder->setAccessChainRValue(constructed);
            }

            return false;
        }

    // These six are component-wise compares with component-wise results.
    // Forward on to createBinaryOperation(), requesting a vector result.
    case glslang::EOpLessThan:
    case glslang::EOpGreaterThan:
    case glslang::EOpLessThanEqual:
    case glslang::EOpGreaterThanEqual:
    case glslang::EOpVectorEqual:
    case glslang::EOpVectorNotEqual:
        {
            // Map the operation to a binary
            binOp = node->getOp();
            reduceComparison = false;
            switch (node->getOp()) {
            case glslang::EOpVectorEqual:     binOp = glslang::EOpEqual;      break;
            case glslang::EOpVectorNotEqual:  binOp = glslang::EOpNotEqual;   break;
            default:                 binOp = node->getOp(); break;
            }
        }
        break;

    //case glslang::EOpRecip:
    //    return glaBuilder->createRecip(operand);

    case glslang::EOpMul:
        // compontent-wise matrix multiply      
        binOp = glslang::EOpMul;
        break;
    case glslang::EOpOuterProduct:
        // two vectors multiplied to make a matrix
        binOp = glslang::EOpOuterProduct;
        break;
    case glslang::EOpDot:
        {
            // for scalar dot product, use multiply        
            glslang::TIntermSequence& glslangOperands = node->getSequence();
            if (! glslangOperands[0]->getAsTyped()->isVector())
                binOp = glslang::EOpMul;
            break;
        }
    case glslang::EOpMod:
        // when an aggregate, this is the floating-point mod built-in function,
        // which can be emitted by the one it createBinaryOperation()
        binOp = glslang::EOpMod;
        break;
    case glslang::EOpArrayLength:
        {
            glslang::TIntermTyped* typedNode = node->getSequence()[0]->getAsTyped();
            assert(typedNode);
            llvm::Value* length = gla::MakeIntConstant(context, typedNode->getType().getArraySize());

            glaBuilder->clearAccessChain();
            glaBuilder->setAccessChainRValue(length);
        }

        return false;
    }

    //
    // See if it maps to a regular operation or intrinsic.
    //

    if (binOp != glslang::EOpNull) {
        glaBuilder->clearAccessChain();
        node->getSequence()[0]->traverse(this);
        llvm::Value* left = glaBuilder->accessChainLoad(GetMdPrecision(node->getSequence()[0]->getAsTyped()->getType()));

        glaBuilder->clearAccessChain();
        node->getSequence()[1]->traverse(this);
        llvm::Value* right = glaBuilder->accessChainLoad(GetMdPrecision(node->getSequence()[1]->getAsTyped()->getType()));

        if (binOp == glslang::EOpOuterProduct)
            result = glaBuilder->createMatrixMultiply(precision, left, right);
        else if (gla::IsAggregate(left) && binOp == glslang::EOpMul)
            result = glaBuilder->createMatrixOp(precision, llvm::Instruction::FMul, left, right);
        else
            result = createBinaryOperation(binOp, precision, left, right, node->getType().getBasicType() == glslang::EbtUint, reduceComparison);

        // code above should only make binOp that exists in createBinaryOperation
        assert(result);

        glaBuilder->clearAccessChain();
        glaBuilder->setAccessChainRValue(result);

        return false;
    }

    glslang::TIntermSequence& glslangOperands = node->getSequence();
    std::vector<llvm::Value*> operands;
    for (int i = 0; i < (int)glslangOperands.size(); ++i) {
        glaBuilder->clearAccessChain();
        glslangOperands[i]->traverse(this);
        operands.push_back(glaBuilder->accessChainLoad(GetMdPrecision(glslangOperands[i]->getAsTyped()->getType())));
    }
    if (glslangOperands.size() == 1)
        result = createUnaryIntrinsic(node->getOp(), precision, operands.front());
    else
        result = createIntrinsic(node->getOp(), precision, operands, glslangOperands.front()->getAsTyped()->getBasicType() == glslang::EbtUint);

    if (! result)
        gla::UnsupportedFunctionality("glslang aggregate", gla::EATContinue);
    else {
        glaBuilder->clearAccessChain();
        glaBuilder->setAccessChainRValue(result);

        return false;
    }

    return true;
}

bool TGlslangToTopTraverser::visitSelection(glslang::TVisit /* visit */, glslang::TIntermSelection* node)
{
    // This path handles both if-then-else and ?:
    // The if-then-else has a node type of void, while
    // ?: has a non-void node type
    llvm::Value* result = 0;
    if (node->getBasicType() != glslang::EbtVoid) {
        // don't handle this as just on-the-fly temporaries, because there will be two names
        // and better to leave SSA to LLVM passes
        result = glaBuilder->createVariable(gla::Builder::ESQLocal, 0, convertGlslangToGlaType(node->getType()),
                                                 0, 0, "ternary");
    }

    // emit the condition before doing anything with selection
    node->getCondition()->traverse(this);

    // make an "if" based on the value created by the condition
    gla::Builder::If ifBuilder(glaBuilder->accessChainLoad(gla::EMpNone), glaBuilder);

    if (node->getTrueBlock()) {
        // emit the "then" statement
		node->getTrueBlock()->traverse(this);
        if (result)
            glaBuilder->createStore(glaBuilder->accessChainLoad(GetMdPrecision(node->getTrueBlock()->getAsTyped()->getType())), result);
	}

    if (node->getFalseBlock()) {
        ifBuilder.makeBeginElse();
        // emit the "else" statement
        node->getFalseBlock()->traverse(this);
        if (result)
            glaBuilder->createStore(glaBuilder->accessChainLoad(GetMdPrecision(node->getFalseBlock()->getAsTyped()->getType())), result);
    }

    ifBuilder.makeEndIf();

    if (result) {
        // GLSL only has r-values as the result of a :?, but
        // if we have an l-value, that can be more efficient if it will
        // become the base of a complex r-value expression, because the
        // next layer copies r-values into memory to use the GEP mechanism
        glaBuilder->clearAccessChain();
        glaBuilder->setAccessChainLValue(result);
    }

    return false;
}

bool TGlslangToTopTraverser::visitSwitch(glslang::TVisit /* visit */, glslang::TIntermSwitch* node)
{
    // emit and get the condition before doing anything with switch
    node->getCondition()->traverse(this);
    llvm::Value* condition = glaBuilder->accessChainLoad(GetMdPrecision(node->getCondition()->getAsTyped()->getType()));

    // browse the children to sort out code segments
    int defaultSegment = -1;
    std::vector<TIntermNode*> codeSegments;
    glslang::TIntermSequence& sequence = node->getBody()->getSequence();
    std::vector<llvm::ConstantInt*> caseValues;
    std::vector<int> valueToSegment(sequence.size());  // note: probably not all are used, it is an overestimate
    for (glslang::TIntermSequence::iterator c = sequence.begin(); c != sequence.end(); ++c) {
        TIntermNode* child = *c;
        if (child->getAsBranchNode() && child->getAsBranchNode()->getFlowOp() == glslang::EOpDefault)
            defaultSegment = codeSegments.size();
        else if (child->getAsBranchNode() && child->getAsBranchNode()->getFlowOp() == glslang::EOpCase) {
            valueToSegment[caseValues.size()] = codeSegments.size();
            caseValues.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 
                                                        child->getAsBranchNode()->getExpression()->getAsConstantUnion()->getConstArray()[0].getIConst(), 
                                                        false));
        } else
            codeSegments.push_back(child);
    }

    // make the switch statement
    std::vector<llvm::BasicBlock*> segmentBB;
    glaBuilder->makeSwitch(condition, codeSegments.size(), caseValues, valueToSegment, defaultSegment, segmentBB);

    // emit all the code in the segments
    breakForLoop.push(false);
    for (unsigned int s = 0; s < codeSegments.size(); ++s) {
        glaBuilder->nextSwitchSegment(segmentBB, s);
        codeSegments[s]->traverse(this);
    }
    breakForLoop.pop();

    glaBuilder->endSwitch(segmentBB);

    return false;
}

void TGlslangToTopTraverser::visitConstantUnion(glslang::TIntermConstantUnion* node)
{
    int nextConst = 0;
    llvm::Value* c = createLLVMConstant(node->getType(), node->getConstArray(), nextConst);
    glaBuilder->clearAccessChain();
    glaBuilder->setAccessChainRValue(c);
}

bool TGlslangToTopTraverser::visitLoop(glslang::TVisit /* visit */, glslang::TIntermLoop* node)
{
    bool bodyOut = false;

    glaBuilder->makeNewLoop();

    if (! node->testFirst()) {
        if (node->getBody()) {
            breakForLoop.push(true);
            node->getBody()->traverse(this);
            breakForLoop.pop();
        }
        bodyOut = true;
    }

    if (node->getTest()) {
        node->getTest()->traverse(this);
        // the AST only contained the test, not the branch, we have to add it

        // make the following
        //     if (! condition from test traversal)
        //         break;
        llvm::Value* condition = glaBuilder->accessChainLoad(GetMdPrecision(node->getTest()->getType()));
        condition = llvmBuilder.CreateNot(condition);
        gla::Builder::If ifBuilder(condition, glaBuilder);
        glaBuilder->makeLoopExit();
        ifBuilder.makeEndIf();
    }

    if (! bodyOut && node->getBody()) {
        breakForLoop.push(true);
        node->getBody()->traverse(this);
        breakForLoop.pop();
    }

    if (node->getTerminal())
        node->getTerminal()->traverse(this);

    glaBuilder->makeLoopBackEdge();
    glaBuilder->closeLoop();

    return false;
}

bool TGlslangToTopTraverser::visitBranch(glslang::TVisit /* visit */, glslang::TIntermBranch* node)
{
    if (node->getExpression())
        node->getExpression()->traverse(this);

    switch (node->getFlowOp()) {
    case glslang::EOpKill:
        glaBuilder->makeDiscard(inMain);
        break;
    case glslang::EOpBreak:
        if (breakForLoop.top())
            glaBuilder->makeLoopExit();
        else
            glaBuilder->addSwitchBreak();
        break;
    case glslang::EOpContinue:
        glaBuilder->makeLoopBackEdge();
        break;
    case glslang::EOpReturn:
        if (inMain)
            glaBuilder->makeMainReturn();
        else if (node->getExpression()) {
            glaBuilder->makeReturn(false, glaBuilder->accessChainLoad(GetMdPrecision(node->getExpression()->getType())));
        } else
            glaBuilder->makeReturn();

        glaBuilder->clearAccessChain();
        break;

    default:
        gla::UnsupportedFunctionality("branch type");
    }

    return false;
}

llvm::Value* TGlslangToTopTraverser::createLLVMVariable(const glslang::TIntermSymbol* node)
{
    llvm::Constant* initializer = 0;
    gla::Builder::EStorageQualifier storageQualifier;
    int constantBuffer = 0;

    switch (node->getQualifier().storage) {
    case glslang::EvqTemporary:
    case glslang::EvqConstReadOnly:
    case glslang::EvqConst:
        storageQualifier = gla::Builder::ESQLocal;
        break;
    case glslang::EvqGlobal:
        storageQualifier = gla::Builder::ESQGlobal;
        break;
    case glslang::EvqVaryingIn:
    case glslang::EvqFragCoord:
    case glslang::EvqPointCoord:
    case glslang::EvqFace:
    case glslang::EvqVertexId:
    case glslang::EvqInstanceId:
        // Pipeline reads: If we are here, it must be to create a shadow which
        // will shadow the actual pipeline reads, which must still be done elsewhere.
        // The top builder will make a global shadow for ESQInput.
        storageQualifier = gla::Builder::ESQInput;
        break;
    case glslang::EvqVaryingOut:
    case glslang::EvqPosition:
    case glslang::EvqPointSize:
    case glslang::EvqClipVertex:
    case glslang::EvqFragColor:
    case glslang::EvqFragDepth:
        storageQualifier = gla::Builder::ESQOutput;
        break;
    case glslang::EvqUniform:
    case glslang::EvqBuffer:
        storageQualifier = gla::Builder::ESQUniform;
        // TODO: linker functionality: uniform buffers? need to generalize to N objects (constant buffers) for higher shader models
        constantBuffer = 0;
        break;
    case glslang::EvqIn:
    case glslang::EvqOut:
    case glslang::EvqInOut:
        // parameter qualifiers should not come through here
    default:
        gla::UnsupportedFunctionality("glslang qualifier", gla::EATContinue);
        storageQualifier = gla::Builder::ESQLocal;
    }

    if (node->getBasicType() == glslang::EbtSampler) {
        storageQualifier = gla::Builder::ESQResource;
    }

    std::string name(node->getName().c_str());

    llvm::Type *llvmType = convertGlslangToGlaType(node->getType());

    return glaBuilder->createVariable(storageQualifier, constantBuffer, llvmType,
                                      initializer, 0, name);
}

llvm::Type* TGlslangToTopTraverser::convertGlslangToGlaType(const glslang::TType& type)
{
    llvm::Type *glaType;

    switch(type.getBasicType()) {
    case glslang::EbtVoid:
        glaType = gla::GetVoidType(context);
        break;
    case glslang::EbtFloat:
        glaType = gla::GetFloatType(context);
        break;
    case glslang::EbtDouble:
        gla::UnsupportedFunctionality("basic type: double", gla::EATContinue);
        break;
    case glslang::EbtBool:
        glaType = gla::GetBoolType(context);
        break;
    case glslang::EbtInt:
    case glslang::EbtSampler:
        glaType = gla::GetIntType(context);
        break;
    case glslang::EbtUint:
        glaType = gla::GetUintType(context);
        break;
    case glslang::EbtStruct:
    case glslang::EbtBlock:
        {
            glslang::TTypeList* glslangStruct = type.getStruct();
            std::vector<llvm::Type*> structFields;
            llvm::StructType* structType = structMap[glslangStruct];
            if (structType) {
                // If we've seen this struct type, return it
                glaType = structType;
            } else {
                // Create a vector of struct types for LLVM to consume
                for (int i = 0; i < (int)glslangStruct->size(); i++)
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

bool TGlslangToTopTraverser::isShaderEntrypoint(const glslang::TIntermAggregate* node)
{
    return node->getName() == "main(";
}

void TGlslangToTopTraverser::makeFunctions(const glslang::TIntermSequence& glslFunctions)
{
    for (int f = 0; f < (int)glslFunctions.size(); ++f) {
        glslang::TIntermAggregate* glslFunction = glslFunctions[f]->getAsAggregate();

        // TODO: compile-time performance: find a way to skip this loop if we aren't
        // a child of the root node of the compilation unit, which should be the only
        // one holding a list of functions.
        if (! glslFunction || glslFunction->getOp() != glslang::EOpFunction || isShaderEntrypoint(glslFunction))
            continue;

        std::vector<llvm::Type*> paramTypes;
        glslang::TIntermSequence& parameters = glslFunction->getSequence()[0]->getAsAggregate()->getSequence();

        // At call time, space should be allocated for all the arguments,
        // and pointers to that space passed to the function as the formal parameters.
        for (int i = 0; i < (int)parameters.size(); ++i) {
            llvm::Type* type = convertGlslangToGlaType(parameters[i]->getAsTyped()->getType());
            paramTypes.push_back(llvm::PointerType::get(type, gla::GlobalAddressSpace));
        }

        llvm::BasicBlock* functionBlock;
        llvm::Function *function = glaBuilder->makeFunctionEntry(convertGlslangToGlaType(glslFunction->getType()), glslFunction->getName().c_str(),
                                                                 paramTypes, &functionBlock);
        function->addFnAttr(llvm::Attribute::AlwaysInline);

        // Visit parameter list again to create mappings to local variables and set attributes.
        llvm::Function::arg_iterator arg = function->arg_begin();
        for (int i = 0; i < (int)parameters.size(); ++i, ++arg)
            symbolValues[parameters[i]->getAsSymbolNode()->getId()] = &(*arg);

        // Track function to emit/call later
        functionMap[glslFunction->getName().c_str()] = function;
    }
}

void TGlslangToTopTraverser::handleFunctionEntry(const glslang::TIntermAggregate* node)
{
    // LLVM functions should already be in the functionMap from the prepass 
    // that called makeFunctions.
    llvm::Function* function = functionMap[node->getName().c_str()];
    llvm::BasicBlock& functionBlock = function->getEntryBlock();
    llvmBuilder.SetInsertPoint(&functionBlock);
}

void TGlslangToTopTraverser::translateArguments(const glslang::TIntermSequence& glslangArguments, std::vector<llvm::Value*>& arguments)
{
    for (int i = 0; i < (int)glslangArguments.size(); ++i) {
        glaBuilder->clearAccessChain();
        glslangArguments[i]->traverse(this);
        arguments.push_back(glaBuilder->accessChainLoad(GetMdPrecision(glslangArguments[i]->getAsTyped()->getType())));
    }
}

llvm::Value* TGlslangToTopTraverser::handleBuiltinFunctionCall(const glslang::TIntermAggregate* node)
{
    std::vector<llvm::Value*> arguments;
    translateArguments(node->getSequence(), arguments);

    gla::EMdPrecision precision = GetMdPrecision(node->getType());

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
        case glslang::Esd1D:       samplerType = gla::ESampler1D;      break;
        case glslang::Esd2D:       samplerType = gla::ESampler2D;      break;
        case glslang::Esd3D:       samplerType = gla::ESampler3D;      break;
        case glslang::EsdCube:     samplerType = gla::ESamplerCube;    break;
        case glslang::EsdRect:     samplerType = gla::ESampler2DRect;  break;
        case glslang::EsdBuffer:   samplerType = gla::ESamplerBuffer;  break;
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

            if ((int)arguments.size() > nonBiasArgCount) {
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

llvm::Value* TGlslangToTopTraverser::handleUserFunctionCall(const glslang::TIntermAggregate* node)
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
    const glslang::TIntermSequence& glslangArgs = node->getSequence();
    const glslang::TQualifierList& qualifiers = node->getQualifierList();
    llvm::SmallVector<gla::Builder::AccessChain, 2> lValuesOut;
    for (int i = 0; i < (int)glslangArgs.size(); ++i) {
        // build l-value
        glaBuilder->clearAccessChain();
        glslangArgs[i]->traverse(this);
        if (qualifiers[i] == glslang::EvqOut || qualifiers[i] == glslang::EvqInOut) {
            // save l-value
            lValuesOut.push_back(glaBuilder->getAccessChain());
        }
        if (qualifiers[i] == glslang::EvqIn || qualifiers[i] == glslang::EvqConstReadOnly || qualifiers[i] == glslang::EvqInOut) {
            // process r-value
            glaBuilder->createStore(glaBuilder->accessChainLoad(GetMdPrecision(glslangArgs[i]->getAsTyped()->getType())), llvmArgs[i]);
        }
    }

    llvm::Value* result = llvmBuilder.Insert(llvm::CallInst::Create(function, llvmArgs));

    // Copy-out time...
    // Convert outputs to correct type before storing into the l-value
    llvm::SmallVector<gla::Builder::AccessChain, 2>::iterator savedIt = lValuesOut.begin();
    for (int i = 0; i < (int)glslangArgs.size(); ++i) {
        if (qualifiers[i] == glslang::EvqOut || qualifiers[i] == glslang::EvqInOut) {
            glaBuilder->setAccessChain(*savedIt);
            llvm::Value* output = glaBuilder->createLoad(llvmArgs[i]);
            llvm::Type* destType = convertGlslangToGlaType(glslangArgs[i]->getAsTyped()->getType());
            if (destType != output->getType()) {
                // TODO: non-ES testing: test this after the front-end can support it
                glslang::TOperator op = glslang::EOpNull;
                if (gla::GetBasicTypeID(destType) == llvm::Type::FloatTyID &&
                    gla::GetBasicTypeID(output->getType())) {
                    op = glslang::EOpConvIntToFloat;
                } // TODO: desktop functionality: more cases will go here for future versions

                if (op != glslang::EOpNull) {
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

llvm::Value* TGlslangToTopTraverser::createBinaryOperation(glslang::TOperator op, gla::EMdPrecision precision, llvm::Value* left, llvm::Value* right, bool isUnsigned, bool reduceComparison)
{
    llvm::Instruction::BinaryOps binOp = llvm::Instruction::BinaryOps(0);
    bool needsPromotion = true;
    bool leftIsFloat = (gla::GetBasicTypeID(left) == llvm::Type::FloatTyID);
    bool comparison = false;

    switch(op) {
    case glslang::EOpAdd:
    case glslang::EOpAddAssign:
        if (leftIsFloat)
            binOp = llvm::Instruction::FAdd;
        else
            binOp = llvm::Instruction::Add;
        break;
    case glslang::EOpSub:
    case glslang::EOpSubAssign:
        if (leftIsFloat)
            binOp = llvm::Instruction::FSub;
        else
            binOp = llvm::Instruction::Sub;
        break;
    case glslang::EOpMul:
    case glslang::EOpMulAssign:
    case glslang::EOpVectorTimesScalar:
    case glslang::EOpVectorTimesScalarAssign:
    case glslang::EOpVectorTimesMatrixAssign:
    case glslang::EOpMatrixTimesScalarAssign:
    case glslang::EOpMatrixTimesMatrixAssign:
        if (leftIsFloat)
            binOp = llvm::Instruction::FMul;
        else
            binOp = llvm::Instruction::Mul;
        break;
    case glslang::EOpDiv:
    case glslang::EOpDivAssign:
        if (leftIsFloat)
            binOp = llvm::Instruction::FDiv;
        else if (isUnsigned)
            binOp = llvm::Instruction::UDiv;
        else
            binOp = llvm::Instruction::SDiv;
        break;
    case glslang::EOpMod:
    case glslang::EOpModAssign:
        if (leftIsFloat)
            binOp = llvm::Instruction::FRem;
        else if (isUnsigned)
            binOp = llvm::Instruction::URem;
        else
            binOp = llvm::Instruction::SRem;
        break;
    case glslang::EOpRightShift:
    case glslang::EOpRightShiftAssign:
        if (isUnsigned)
            binOp = llvm::Instruction::LShr;
        else
            binOp = llvm::Instruction::AShr;
        break;
    case glslang::EOpLeftShift:
    case glslang::EOpLeftShiftAssign:
        binOp = llvm::Instruction::Shl;
        break;
    case glslang::EOpAnd:
    case glslang::EOpAndAssign:
        binOp = llvm::Instruction::And;
        break;
    case glslang::EOpInclusiveOr:
    case glslang::EOpInclusiveOrAssign:
    case glslang::EOpLogicalOr:
        binOp = llvm::Instruction::Or;
        break;
    case glslang::EOpExclusiveOr:
    case glslang::EOpExclusiveOrAssign:
    case glslang::EOpLogicalXor:
        binOp = llvm::Instruction::Xor;
        break;
    case glslang::EOpLogicalAnd:
        assert(gla::IsBoolean(left->getType()) && gla::IsScalar(left->getType()));
        assert(gla::IsBoolean(right->getType()) && gla::IsScalar(right->getType()));
        needsPromotion = false;
        binOp = llvm::Instruction::And;
        break;

    case glslang::EOpLessThan:
    case glslang::EOpGreaterThan:
    case glslang::EOpLessThanEqual:
    case glslang::EOpGreaterThanEqual:
    case glslang::EOpEqual:
    case glslang::EOpNotEqual:
        comparison = true;
        break;
    }

    if (binOp != 0) {
        if (gla::IsAggregate(left) || gla::IsAggregate(right)) {
            switch(op) {
            case glslang::EOpVectorTimesMatrixAssign:
            case glslang::EOpMatrixTimesScalarAssign:
            case glslang::EOpMatrixTimesMatrixAssign:
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
        assert(op == glslang::EOpEqual || op == glslang::EOpNotEqual);

        return glaBuilder->createCompare(precision, left, right, op == glslang::EOpEqual);
    }

    if (leftIsFloat) {
        llvm::FCmpInst::Predicate pred = llvm::FCmpInst::Predicate(0);
        switch (op) {
        case glslang::EOpLessThan:
            pred = llvm::FCmpInst::FCMP_OLT;
            break;
        case glslang::EOpGreaterThan:
            pred = llvm::FCmpInst::FCMP_OGT;
            break;
        case glslang::EOpLessThanEqual:
            pred = llvm::FCmpInst::FCMP_OLE;
            break;
        case glslang::EOpGreaterThanEqual:
            pred = llvm::FCmpInst::FCMP_OGE;
            break;
        case glslang::EOpEqual:
            pred = llvm::FCmpInst::FCMP_OEQ;
            break;
        case glslang::EOpNotEqual:
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
            case glslang::EOpLessThan:
                pred = llvm::ICmpInst::ICMP_ULT;
                break;
            case glslang::EOpGreaterThan:
                pred = llvm::ICmpInst::ICMP_UGT;
                break;
            case glslang::EOpLessThanEqual:
                pred = llvm::ICmpInst::ICMP_ULE;
                break;
            case glslang::EOpGreaterThanEqual:
                pred = llvm::ICmpInst::ICMP_UGE;
                break;
            case glslang::EOpEqual:
                pred = llvm::ICmpInst::ICMP_EQ;
                break;
            case glslang::EOpNotEqual:
                pred = llvm::ICmpInst::ICMP_NE;
                break;
            }
        } else {
            switch (op) {
            case glslang::EOpLessThan:
                pred = llvm::ICmpInst::ICMP_SLT;
                break;
            case glslang::EOpGreaterThan:
                pred = llvm::ICmpInst::ICMP_SGT;
                break;
            case glslang::EOpLessThanEqual:
                pred = llvm::ICmpInst::ICMP_SLE;
                break;
            case glslang::EOpGreaterThanEqual:
                pred = llvm::ICmpInst::ICMP_SGE;
                break;
            case glslang::EOpEqual:
                pred = llvm::ICmpInst::ICMP_EQ;
                break;
            case glslang::EOpNotEqual:
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

llvm::Value* TGlslangToTopTraverser::createUnaryOperation(glslang::TOperator op, gla::EMdPrecision precision, llvm::Value* operand)
{
    // Unary ops that map to llvm operations
    switch (op) {
    case glslang::EOpNegative:
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

    case glslang::EOpLogicalNot:
    case glslang::EOpVectorLogicalNot:
    case glslang::EOpBitwiseNot:
        return llvmBuilder.CreateNot(operand);
    
    case glslang::EOpDeterminant:
        return glaBuilder->createMatrixDeterminant(precision, operand);
    case glslang::EOpMatrixInverse:
        return glaBuilder->createMatrixInverse(precision, operand);
    case glslang::EOpTranspose:
        return glaBuilder->createMatrixTranspose(precision, operand);
    }

    return 0;
}

llvm::Value* TGlslangToTopTraverser::createConversion(glslang::TOperator op, gla::EMdPrecision precision, llvm::Type* destType, llvm::Value* operand)
{
    llvm::Instruction::CastOps castOp = llvm::Instruction::CastOps(0);
    switch(op) {
    case glslang::EOpConvIntToBool:
    case glslang::EOpConvUintToBool:
    case glslang::EOpConvFloatToBool:
        {
            // any non-zero should return true
            llvm::Value* zero;
            if (op == glslang::EOpConvFloatToBool)
                zero = gla::MakeFloatConstant(context, 0.0f);
            else
                zero = gla::MakeIntConstant(context, 0);

            if (gla::GetComponentCount(operand) > 1)
                zero = glaBuilder->smearScalar(gla::EMpNone, zero, operand->getType());

            return createBinaryOperation(glslang::EOpNotEqual, precision, operand, zero, false, false);
        }

    case glslang::EOpConvIntToFloat:
        castOp = llvm::Instruction::SIToFP;
        break;
    case glslang::EOpConvBoolToFloat:
        castOp = llvm::Instruction::UIToFP;
        break;
    case glslang::EOpConvUintToFloat:
        castOp = llvm::Instruction::UIToFP;
        break;

    case glslang::EOpConvFloatToInt:
        castOp = llvm::Instruction::FPToSI;
        break;
    case glslang::EOpConvBoolToInt:
        // GLSL says true is converted to 1
        castOp = llvm::Instruction::ZExt;
        break;
    case glslang::EOpConvUintToInt:

        return operand;

    case glslang::EOpConvBoolToUint:
        // GLSL says true is converted to 1
        castOp = llvm::Instruction::ZExt;
        break;
    case glslang::EOpConvFloatToUint:
        castOp = llvm::Instruction::FPToUI;
        break;
    case glslang::EOpConvIntToUint:

        return operand;

    case glslang::EOpConvDoubleToInt:
    case glslang::EOpConvDoubleToBool:
    case glslang::EOpConvDoubleToFloat:
    case glslang::EOpConvDoubleToUint:
    case glslang::EOpConvIntToDouble:
    case glslang::EOpConvUintToDouble:
    case glslang::EOpConvFloatToDouble:
    case glslang::EOpConvBoolToDouble:
        gla::UnsupportedFunctionality("double conversion");
        break;
    }

    if (castOp == 0)

        return 0;

    llvm::Value* result = llvmBuilder.CreateCast(castOp, operand, destType);
    glaBuilder->setInstructionPrecision(result, precision);

    return result;
}

llvm::Value* TGlslangToTopTraverser::createUnaryIntrinsic(glslang::TOperator op, gla::EMdPrecision precision, llvm::Value* operand)
{
    // Unary ops that require an intrinsic
    llvm::Intrinsic::ID intrinsicID = llvm::Intrinsic::ID(0);

    switch(op) {
    case glslang::EOpRadians:
        intrinsicID = llvm::Intrinsic::gla_fRadians;
        break;
    case glslang::EOpDegrees:
        intrinsicID = llvm::Intrinsic::gla_fDegrees;
        break;

    case glslang::EOpSin:
        intrinsicID = llvm::Intrinsic::gla_fSin;
        break;
    case glslang::EOpCos:
        intrinsicID = llvm::Intrinsic::gla_fCos;
        break;
    case glslang::EOpTan:
        intrinsicID = llvm::Intrinsic::gla_fTan;
        break;
    case glslang::EOpAcos:
        intrinsicID = llvm::Intrinsic::gla_fAcos;
        break;
    case glslang::EOpAsin:
        intrinsicID = llvm::Intrinsic::gla_fAsin;
        break;
    case glslang::EOpAtan:
        intrinsicID = llvm::Intrinsic::gla_fAtan;
        break;

    case glslang::EOpAcosh:
        intrinsicID = llvm::Intrinsic::gla_fAcosh;
        break;
    case glslang::EOpAsinh:
        intrinsicID = llvm::Intrinsic::gla_fAsinh;
        break;
    case glslang::EOpAtanh:
        intrinsicID = llvm::Intrinsic::gla_fAtanh;
        break;
    case glslang::EOpTanh:
        intrinsicID = llvm::Intrinsic::gla_fTanh;
        break;
    case glslang::EOpCosh:
        intrinsicID = llvm::Intrinsic::gla_fCosh;
        break;
    case glslang::EOpSinh:
        intrinsicID = llvm::Intrinsic::gla_fSinh;
        break;

    case glslang::EOpLength:
        intrinsicID = llvm::Intrinsic::gla_fLength;
        break;
    case glslang::EOpNormalize:
        intrinsicID = llvm::Intrinsic::gla_fNormalize;
        break;

    case glslang::EOpExp:
        intrinsicID = llvm::Intrinsic::gla_fExp;
        break;
    case glslang::EOpLog:
        intrinsicID = llvm::Intrinsic::gla_fLog;
        break;
    case glslang::EOpExp2:
        intrinsicID = llvm::Intrinsic::gla_fExp2;
        break;
    case glslang::EOpLog2:
        intrinsicID = llvm::Intrinsic::gla_fLog2;
        break;
    case glslang::EOpSqrt:
        intrinsicID = llvm::Intrinsic::gla_fSqrt;
        break;
    case glslang::EOpInverseSqrt:
        intrinsicID = llvm::Intrinsic::gla_fInverseSqrt;
        break;

    case glslang::EOpFloor:
        intrinsicID = llvm::Intrinsic::gla_fFloor;
        break;
    case glslang::EOpTrunc:
        intrinsicID = llvm::Intrinsic::gla_fRoundZero;
        break;
    case glslang::EOpRound:
        intrinsicID = llvm::Intrinsic::gla_fRoundFast;
        break;
    case glslang::EOpRoundEven:
        intrinsicID = llvm::Intrinsic::gla_fRoundEven;
        break;
    case glslang::EOpCeil:
        intrinsicID = llvm::Intrinsic::gla_fCeiling;
        break;
    case glslang::EOpFract:
        intrinsicID = llvm::Intrinsic::gla_fFraction;
        break;

    case glslang::EOpIsNan:
        intrinsicID = llvm::Intrinsic::gla_fIsNan;
        break;
    case glslang::EOpIsInf:
        intrinsicID = llvm::Intrinsic::gla_fIsInf;
        break;

    case glslang::EOpFloatBitsToInt:
    case glslang::EOpFloatBitsToUint:
        intrinsicID = llvm::Intrinsic::gla_fFloatBitsToInt;
        break;
    case glslang::EOpIntBitsToFloat:
    case glslang::EOpUintBitsToFloat:
        intrinsicID = llvm::Intrinsic::gla_fIntBitsTofloat;
        break;
    case glslang::EOpPackSnorm2x16:
        intrinsicID = llvm::Intrinsic::gla_fPackSnorm2x16;
        break;
    case glslang::EOpUnpackSnorm2x16:
        intrinsicID = llvm::Intrinsic::gla_fUnpackSnorm2x16;
        break;
    case glslang::EOpPackUnorm2x16:
        intrinsicID = llvm::Intrinsic::gla_fPackUnorm2x16;
        break;
    case glslang::EOpUnpackUnorm2x16:
        intrinsicID = llvm::Intrinsic::gla_fUnpackUnorm2x16;
        break;
    case glslang::EOpPackHalf2x16:
        intrinsicID = llvm::Intrinsic::gla_fPackHalf2x16;
        break;
    case glslang::EOpUnpackHalf2x16:
        intrinsicID = llvm::Intrinsic::gla_fUnpackHalf2x16;
        break;

    case glslang::EOpDPdx:
        intrinsicID = llvm::Intrinsic::gla_fDFdx;
        break;
    case glslang::EOpDPdy:
        intrinsicID = llvm::Intrinsic::gla_fDFdy;
        break;
    case glslang::EOpFwidth:
        intrinsicID = llvm::Intrinsic::gla_fFilterWidth;
        break;

    case glslang::EOpAny:
        intrinsicID = llvm::Intrinsic::gla_any;
        break;
    case glslang::EOpAll:
        intrinsicID = llvm::Intrinsic::gla_all;
        break;

    case glslang::EOpAbs:
        if (gla::GetBasicTypeID(operand) == llvm::Type::FloatTyID)
            intrinsicID = llvm::Intrinsic::gla_fAbs;
        else
            intrinsicID = llvm::Intrinsic::gla_abs;
        break;
    case glslang::EOpSign:
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

llvm::Value* TGlslangToTopTraverser::createIntrinsic(glslang::TOperator op, gla::EMdPrecision precision, std::vector<llvm::Value*>& operands, bool isUnsigned)
{
    // Binary ops that require an intrinsic
    llvm::Value* result = 0;
    llvm::Intrinsic::ID intrinsicID = llvm::Intrinsic::ID(0);

    switch (op) {
    case glslang::EOpMin:
        if (gla::GetBasicTypeID(operands.front()) == llvm::Type::FloatTyID)
            intrinsicID = llvm::Intrinsic::gla_fMin;
        else if (isUnsigned)
            intrinsicID = llvm::Intrinsic::gla_uMin;
        else
            intrinsicID = llvm::Intrinsic::gla_sMin;
        break;
    case glslang::EOpMax:
        if (gla::GetBasicTypeID(operands.front()) == llvm::Type::FloatTyID)
            intrinsicID = llvm::Intrinsic::gla_fMax;
        else if (isUnsigned)
            intrinsicID = llvm::Intrinsic::gla_uMax;
        else
            intrinsicID = llvm::Intrinsic::gla_sMax;
        break;
    case glslang::EOpPow:
        if (gla::GetBasicTypeID(operands.front()) == llvm::Type::FloatTyID)
            intrinsicID = llvm::Intrinsic::gla_fPow;
        else
            intrinsicID = llvm::Intrinsic::gla_fPowi;
        break;
    case glslang::EOpDot:
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
    case glslang::EOpAtan:
        intrinsicID = llvm::Intrinsic::gla_fAtan2;
        break;

    case glslang::EOpClamp:
        if (gla::GetBasicTypeID(operands.front()) == llvm::Type::FloatTyID)
            intrinsicID = llvm::Intrinsic::gla_fClamp;
        else if (isUnsigned)
            intrinsicID = llvm::Intrinsic::gla_uClamp;
        else
            intrinsicID = llvm::Intrinsic::gla_sClamp;
        break;
    case glslang::EOpMix:
        if (gla::GetBasicTypeID(operands.back()) == llvm::Type::IntegerTyID)
            intrinsicID = llvm::Intrinsic::gla_fbMix;
        else
            intrinsicID = llvm::Intrinsic::gla_fMix;
        break;
    case glslang::EOpStep:
        intrinsicID = llvm::Intrinsic::gla_fStep;
        break;
    case glslang::EOpSmoothStep:
        intrinsicID = llvm::Intrinsic::gla_fSmoothStep;
        break;

    case glslang::EOpDistance:
        intrinsicID = llvm::Intrinsic::gla_fDistance;
        break;
    case glslang::EOpCross:
        intrinsicID = llvm::Intrinsic::gla_fCross;
        break;
    case glslang::EOpFaceForward:
        intrinsicID = llvm::Intrinsic::gla_fFaceForward;
        break;
    case glslang::EOpReflect:
        intrinsicID = llvm::Intrinsic::gla_fReflect;
        break;
    case glslang::EOpRefract:
        intrinsicID = llvm::Intrinsic::gla_fRefract;
        break;
    case glslang::EOpModf:
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

// Set up to recursively traverse the structure to read, while flattening it into slots
void TGlslangToTopTraverser::createPipelineRead(glslang::TIntermSymbol* node, llvm::Value* storage, int firstSlot, llvm::MDNode* md)
{
    gla::EInterpolationMethod method;
    gla::EInterpolationLocation location;
    GetInterpolationLocationMethod(node->getType(), method, location);
    // For pipeline inputs, and we will generate a fresh pipeline read at each reference,
    // which gets optimized later.
    std::string name(node->getName().c_str());

    std::vector<llvm::Value*> gepChain;
    createPipelineSubread(node->getType(), storage, gepChain, firstSlot, md, name, method, location);
}

// Recursively read the input structure
void TGlslangToTopTraverser::createPipelineSubread(const glslang::TType& glaType, llvm::Value* storage, std::vector<llvm::Value*>& gepChain, int& slot, llvm::MDNode* md, 
                                                   std::string& name, gla::EInterpolationMethod method, gla::EInterpolationLocation location)
{
    // gla types can be both arrays and matrices or arrays and structures at the same time;
    // make sure to process arrayness first, so it is stripped to get to elements

    if (glaType.isArray()) {
        // read the array elements, recursively

        int arraySize = glaType.getArraySize();
        if (arraySize == 0) {
            // TODO: desktop linker functionality: make sure front end knows size before calling here, see
            // comment in convertGlslangToGlaType
            arraySize = UnknownArraySize;
        }

        glslang::TType elementType;
        elementType.shallowCopy(glaType);  // TODO: desktop arrays will need a deeper copy to avoid modifying the original
        elementType.dereference();

        if (gepChain.size() == 0)
            gepChain.push_back(gla::MakeIntConstant(context, 0));
        for (int element = 0; element < arraySize; ++element) {
            gepChain.push_back(gla::MakeIntConstant(context, element));
            createPipelineSubread(elementType, storage, gepChain, slot, md, name, method, location);
            gepChain.pop_back();
        }
        if (gepChain.size() == 1)
            gepChain.pop_back();
    } else if (const glslang::TTypeList* typeList = glaType.getStruct()) {
        if (gepChain.size() == 0)
            gepChain.push_back(gla::MakeIntConstant(context, 0));
        for (int field = 0; field < (int)typeList->size(); ++field) {
            gepChain.push_back(gla::MakeIntConstant(context, field));            
            createPipelineSubread(*(*typeList)[field].type, storage, gepChain, slot, md, name, method, location);
            gepChain.pop_back();
        }
        if (gepChain.size() == 1)
            gepChain.pop_back();
        
    } else if (glaType.isMatrix()) {
        // Read the whole matrix now, one slot at a time.

        int numColumns = glaType.getMatrixCols();            
        
        glslang::TType columnType;
        columnType.shallowCopy(glaType);
        columnType.dereference();
        llvm::Type* readType = convertGlslangToGlaType(columnType);

        // fill in the whole aggregate shadow, slot by slot
        if (gepChain.size() == 0)
            gepChain.push_back(gla::MakeIntConstant(context, 0));
        for (int column = 0; column < numColumns; ++column, ++slot) {
            gepChain.push_back(gla::MakeIntConstant(context, column));               
            llvm::Value* pipeRead = glaBuilder->readPipeline(GetMdPrecision(glaType), readType, name, slot, md, -1 /*mask*/, method, location);
            llvmBuilder.CreateStore(pipeRead, glaBuilder->createGEP(storage, gepChain));                
            gepChain.pop_back();
        }
        if (gepChain.size() == 1)
            gepChain.pop_back();
    } else {
        llvm::Type* readType = convertGlslangToGlaType(glaType);
        llvm::Value* pipeRead = glaBuilder->readPipeline(GetMdPrecision(glaType), readType, name, slot, md, -1 /*mask*/, method, location);
        ++slot;
        if (gepChain.size() > 0)
            llvmBuilder.CreateStore(pipeRead, glaBuilder->createGEP(storage, gepChain));
        else
            llvmBuilder.CreateStore(pipeRead, storage);
    }
}

int TGlslangToTopTraverser::assignSlot(glslang::TIntermSymbol* node, bool input)
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
        slot = node->getQualifier().layoutLocation;
        
        return slot;
    }

    // Not found in the symbol, see if we've assigned one before

    std::map<std::string, int>::iterator iter;
    const char* name = node->getName().c_str();
    iter = slotMap.find(name);

    if (slotMap.end() == iter) {
        slotMap[name] = nextSlot;
        nextSlot += numSlots;
    }

    return slotMap[name];
}

llvm::Value* TGlslangToTopTraverser::getSymbolStorage(const glslang::TIntermSymbol* symbol, bool& firstTime)
{
    std::map<int, llvm::Value*>::iterator iter;
    iter = symbolValues.find(symbol->getId());
    llvm::Value* storage;
    if (symbolValues.end() == iter) {
        // it was not found, create it
        firstTime = true;
        storage = createLLVMVariable(symbol);
        symbolValues[symbol->getId()] = storage;
    } else {
        firstTime = false;
        storage = iter->second;
    }

    return storage;
}

llvm::Value* TGlslangToTopTraverser::createLLVMConstant(const glslang::TType& glslangType, const glslang::TConstUnionArray& consts, int& nextConst)
{
    // vector of constants for LLVM
    std::vector<llvm::Constant*> llvmConsts;

    // Type is used for struct and array constants
    llvm::Type* type = convertGlslangToGlaType(glslangType);

    if (glslangType.isArray()) {
        glslang::TType elementType;
        elementType.shallowCopy(glslangType);   // TODO: desktop arrays will need a deeper copy to avoid modifying the original
        elementType.dereference();
        for (int i = 0; i < glslangType.getArraySize(); ++i)
            llvmConsts.push_back(llvm::dyn_cast<llvm::Constant>(createLLVMConstant(elementType, consts, nextConst)));
    } else if (glslangType.isMatrix()) {
        glslang::TType vectorType;
        vectorType.shallowCopy(glslangType);
        vectorType.dereference();
        for (int col = 0; col < glslangType.getMatrixCols(); ++col)
            llvmConsts.push_back(llvm::dyn_cast<llvm::Constant>(createLLVMConstant(vectorType, consts, nextConst)));
    } else if (glslangType.getStruct()) {
        glslang::TVector<glslang::TTypeLoc>::iterator iter;
        for (iter = glslangType.getStruct()->begin(); iter != glslangType.getStruct()->end(); ++iter)
            llvmConsts.push_back(llvm::dyn_cast<llvm::Constant>(createLLVMConstant(*iter->type, consts, nextConst)));
    } else {
        // a vector or scalar, both will work the same way
        // this is where we actually consume the constants, rather than walk a tree

        for (unsigned int i = 0; i < (unsigned int)glslangType.getVectorSize(); ++i) {
            switch(consts[nextConst].getType()) {
            case glslang::EbtInt:
                llvmConsts.push_back(gla::MakeIntConstant(context, consts[nextConst].getIConst()));
                break;
            case glslang::EbtUint:
                llvmConsts.push_back(gla::MakeUnsignedConstant(context, consts[nextConst].getUConst()));
                break;
            case glslang::EbtDouble:
                llvmConsts.push_back(gla::MakeFloatConstant(context, (float)consts[nextConst].getDConst()));
                break;
            case glslang::EbtBool:
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

llvm::MDNode* TGlslangToTopTraverser::declareUniformMetadata(glslang::TIntermSymbol* node, llvm::Value* value)
{
    llvm::MDNode* md;
    const std::string name = node->getName().c_str();
    md = uniformMdMap[name];
    if (md)
        return md;

    gla::EMdInputOutput ioType = GetMdQualifier(node);
    switch (ioType) {
    case gla::EMioDefaultUniform:
        md = declareMdDefaultUniform(node, value);
        uniformMdMap[name] = md;
        break;
    case gla::EMioUniformBlockMember:
    case gla::EMioBufferBlockMember:
        md = declareMdUniformBlock(ioType, node, value);
        uniformMdMap[name] = md;
        break;
    default:
        break;
    }

    if (linkageOnly)
        metadata.addNoStaticUse(md);

    return md;
}

// Make a !gla.uniform node, as per metadata.h, for a default uniform
llvm::MDNode* TGlslangToTopTraverser::declareMdDefaultUniform(glslang::TIntermSymbol* node, llvm::Value* value)
{
    const glslang::TType& type = node->getType();
    llvm::MDNode* samplerMd = makeMdSampler(type, value);

    // Create hierarchical type information if it's an aggregate
    gla::EMdTypeLayout layout = GetMdTypeLayout(type);
    llvm::MDNode* structure = 0;
    if (layout == gla::EMtlAggregate)
        structure = declareMdType(type);

    // Make the main node
    return metadata.makeMdInputOutput(node->getName().c_str(), gla::UniformListMdName, gla::EMioDefaultUniform, 
                                      MakePermanentTypeProxy(value),
                                      layout, GetMdPrecision(type), gla::MaxUserLayoutLocation, samplerMd, structure);
}

llvm::MDNode* TGlslangToTopTraverser::makeMdSampler(const glslang::TType& type, llvm::Value* value)
{
    // Figure out sampler information, if it's a sampler
    if (type.getBasicType() == glslang::EbtSampler) {
        llvm::Value* typeProxy = 0;
        if (! value) {
            // TODO: memory: who/how owns tracking and deleting this allocation?
            typeProxy = new llvm::GlobalVariable(convertGlslangToGlaType(type), true, llvm::GlobalVariable::ExternalLinkage, 0, "sampler_typeProxy");
        } else
            typeProxy = MakePermanentTypeProxy(value);

        return metadata.makeMdSampler(GetMdSampler(type), typeProxy, GetMdSamplerDim(type), type.getSampler().arrayed,
                                      type.getSampler().shadow, GetMdSamplerBaseType(type.getSampler().type));
    } else
        return 0;
}

// Make a !gla.uniform node, as per metadata.h, for a uniform block or buffer block (depending on ioType)
llvm::MDNode* TGlslangToTopTraverser::declareMdUniformBlock(gla::EMdInputOutput ioType, const glslang::TIntermSymbol* node, llvm::Value* value)
{
    const glslang::TType& type = node->getType();
    const char* name;
    if (node->getName().substr(0,6) == "__anon")
        name = "";
    else
        name = node->getName().c_str();

    // Make hierachical type information
    llvm::MDNode* block = declareMdType(type);

    // Make the main node
    return metadata.makeMdInputOutput(name, gla::UniformListMdName, ioType, MakePermanentTypeProxy(value),
                                      GetMdTypeLayout(type), GetMdPrecision(type), gla::MaxUserLayoutLocation, 0, block);
}

// Make a !type node as per metadata.h, recursively
llvm::MDNode* TGlslangToTopTraverser::declareMdType(const glslang::TType& type)
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
    mdArgs.push_back(metadata.makeMdTypeLayout(GetMdTypeLayout(type), GetMdPrecision(type), GetMdSlotLocation(type), samplerMd));

    const glslang::TTypeList* typeList = type.getStruct();
    if (typeList) {
        for (int t = 0; t < (int)typeList->size(); ++t) {
            // name of member
            const glslang::TType* fieldType = (*typeList)[t].type;
            mdArgs.push_back(llvm::MDString::get(context, fieldType->getFieldName().c_str()));
            
            // type of member
            llvm::MDNode* mdType = declareMdType(*fieldType);
            mdArgs.push_back(mdType);
        }
    }

    return llvm::MDNode::get(context, mdArgs);
}

llvm::MDNode* TGlslangToTopTraverser::makeInputOutputMetadata(glslang::TIntermSymbol* node, llvm::Value* value, int slot, const char* kind)
{    
    llvm::MDNode* aggregate = 0;
    if (node->getBasicType() == glslang::EbtStruct || node->getBasicType() == glslang::EbtBlock) {
        // Make hierarchical type information
        aggregate = declareMdType(node->getType());
    }

    gla::EInterpolationMethod interpMethod;
    gla::EInterpolationLocation interpLocation;
    GetInterpolationLocationMethod(node->getType(), interpMethod, interpLocation);

    return metadata.makeMdInputOutput(node->getName().c_str(), kind, GetMdQualifier(node), MakePermanentTypeProxy(value), 
                                      GetMdTypeLayout(node->getType()), GetMdPrecision(node->getType()), slot, 0, aggregate,
                                      gla::MakeInterpolationMode(interpMethod, interpLocation));
}

void TGlslangToTopTraverser::setOutputMetadata(glslang::TIntermSymbol* node, llvm::Value* storage, int slot)
{
    llvm::MDNode* md = makeInputOutputMetadata(node, storage, slot, gla::OutputListMdName);

    if (node->getQualifier().invariant)
        module->getOrInsertNamedMetadata(gla::InvariantListMdName)->addOperand(md);

    if (linkageOnly)
        metadata.addNoStaticUse(md);

    glaBuilder->setOutputMetadata(storage, md, slot);
}

llvm::MDNode* TGlslangToTopTraverser::makeInputMetadata(glslang::TIntermSymbol* node, llvm::Value* value, int slot)
{

    llvm::MDNode* mdNode = inputMdMap[slot];
    if (mdNode == 0) {
        // set up metadata for pipeline intrinsic read
        mdNode = makeInputOutputMetadata(node, value, slot, gla::InputListMdName);
        inputMdMap[slot] = mdNode;
        if (linkageOnly)
            metadata.addNoStaticUse(mdNode);
    }

    return mdNode;
}

//
// Set up the glslang traversal
//
void GlslangToTop(TIntermNode* root, gla::Manager* manager)
{
    if (root == 0)
        return;

    TGlslangToTopTraverser it(manager);

    root->traverse(&it);
}
