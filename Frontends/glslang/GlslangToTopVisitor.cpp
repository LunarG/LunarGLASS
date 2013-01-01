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

    gla::Builder::SuperValue createLLVMVariable(TIntermSymbol* node, bool shadow);
    const llvm::Type* convertGlslangToGlaType(const TType& type);

    void handleFunctionEntry(TIntermAggregate* node);
    void translateArguments(TIntermSequence& glslangArguments, std::vector<gla::Builder::SuperValue>& arguments);
    gla::Builder::SuperValue handleBuiltinFunctionCall(TIntermAggregate*);
    gla::Builder::SuperValue handleUserFunctionCall(TIntermAggregate*);

    gla::Builder::SuperValue createBinaryOperation(TOperator op, gla::Builder::SuperValue left, gla::Builder::SuperValue right, bool reduceComparison = true);
    gla::Builder::SuperValue createUnaryOperation(TOperator op, const TType& destType, gla::Builder::SuperValue operand);
    gla::Builder::SuperValue createUnaryIntrinsic(TOperator op, gla::Builder::SuperValue operand, TBasicType);
    gla::Builder::SuperValue createIntrinsic(TOperator op, std::vector<gla::Builder::SuperValue>& operands, TBasicType);
    void createPipelineRead(TIntermSymbol*, gla::Builder::SuperValue storage, int slot);
    int getNextInterpIndex(const std::string& name, int numSlots);
    gla::Builder::SuperValue createLLVMConstant(const TType& type, constUnion *consts, int& nextConst);

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

// A fully functionaling front end will know all array sizes,
// this is just a back up size.
const int UnknownArraySize = 8;

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

    // we will shadow inputs in global variables, so everything gets a variable 
    // allocated, see if we've cached it

    std::map<int, gla::Builder::SuperValue>::iterator iter;
    iter = oit->namedValues.find(symbolNode->getId());
    gla::Builder::SuperValue storage;

    if (oit->namedValues.end() == iter) {
        // it was not found, create it
        storage = oit->createLLVMVariable(symbolNode, input);
        oit->namedValues[symbolNode->getId()] = storage;
    } else
        storage = oit->namedValues[symbolNode->getId()];

    // Track the current value
    oit->glaBuilder->setAccessChainLValue(storage);

    if (input) {
        // TODO: get correct slot numbers from somewhere
        int size = 1;
        if (symbolNode->getType().isArray()) {
            size = symbolNode->getType().getArraySize();
            if (size == 0)
                size = UnknownArraySize;
        }
        oit->createPipelineRead(symbolNode, storage, oit->getNextInterpIndex(symbolNode->getSymbol().c_str(), size));
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
            gla::Builder::SuperValue rValue = oit->glaBuilder->accessChainLoad();

            if (node->getOp() != EOpAssign) {
                // the left is also an r-value
                oit->glaBuilder->setAccessChain(lValue);
                gla::Builder::SuperValue leftRValue = oit->glaBuilder->accessChainLoad();

                // do the operation
                rValue = oit->createBinaryOperation(node->getOp(), leftRValue, rValue);
                if (rValue.isClear())
                    gla::UnsupportedFunctionality("unknown op-assign");
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
            } else {
                // struct or array or indirection into a vector; will use native LLVM gep
                // matrices are arrays of vectors, so will also work for a matrix

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
    }

    // Assume generic binary op...

    // Get the operands
    oit->glaBuilder->clearAccessChain();
    node->getLeft()->traverse(oit);
    gla::Builder::SuperValue left = oit->glaBuilder->accessChainLoad();
    
    oit->glaBuilder->clearAccessChain();
    node->getRight()->traverse(oit);
    gla::Builder::SuperValue right = oit->glaBuilder->accessChainLoad();

    gla::Builder::SuperValue result;

    switch (node->getOp()) {
    case EOpVectorTimesMatrix:
    case EOpMatrixTimesVector:
    case EOpMatrixTimesScalar:
    case EOpMatrixTimesMatrix:
    //case EOpOuterProduct:
        result = oit->glaBuilder->createMatrixMultiply(left, right);
        break;
    default:
        result = oit->createBinaryOperation(node->getOp(), left, right);
        break;
    }

    if (result.isClear()) {
        gla::UnsupportedFunctionality("glslang binary operation", gla::EATContinue);

        return true;
    } else {
        oit->glaBuilder->clearAccessChain();
        oit->glaBuilder->setAccessChainRValue(result);

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

    // must be a special case, check...
    switch (node->getOp()) {
    case EOpPostIncrement:
    case EOpPostDecrement:
    case EOpPreIncrement:
    case EOpPreDecrement: {
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

        gla::Builder::SuperValue result = oit->createBinaryOperation(op, operand, one);

        // The result of operation is always stored, but conditionally the 
        // consumed result.  The consumed result is always an r-value.
        oit->glaBuilder->accessChainStore(result);
        oit->glaBuilder->clearAccessChain();
        if (node->getOp() == EOpPreIncrement ||
            node->getOp() == EOpPreDecrement)
            oit->glaBuilder->setAccessChainRValue(result);
        else
            oit->glaBuilder->setAccessChainRValue(operand);

        return false;
    }
    default:
        gla::UnsupportedFunctionality("glslang unary", gla::EATContinue);
        return false;
    }

    return true;
}

bool TranslateAggregate(bool preVisit, TIntermAggregate* node, TIntermTraverser* it)
{
    TGlslangToTopTraverser* oit = static_cast<TGlslangToTopTraverser*>(it);
    gla::Builder::SuperValue result;
    TOperator binOp = EOpNull;
    bool reduceComparison = true;
    bool isMatrix = false;

    if (node->getOp() == EOpNull) {
        gla::UnsupportedFunctionality("glslang aggregate: EOpNull", gla::EATContinue);
        return false;
    }

    switch (node->getOp()) {
    case EOpSequence:
        return true;
    case EOpComma: {
        // processing from left to right naturally leaves the right-most
        // lying around in the access chain
        TIntermSequence& glslangOperands = node->getSequence();
        for (int i = 0; i < glslangOperands.size(); ++i)
            glslangOperands[i]->traverse(oit);
        
        return false;
    }
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
        if (node->isUserDefined())
            result = oit->handleUserFunctionCall(node);
        else
            result = oit->handleBuiltinFunctionCall(node);

        if (result.isClear())
            gla::UnsupportedFunctionality("glslang function call", gla::EATContinue);
        else {
            oit->glaBuilder->clearAccessChain();
            oit->glaBuilder->setAccessChainRValue(result);
        }

        return false;
    }

    case EOpConstructMat2:
    case EOpConstructMat3:
    case EOpConstructMat4:
        isMatrix = true;
        // fall through
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
        gla::Builder::SuperValue constructed = oit->glaBuilder->createVariable(gla::Builder::ESQLocal, 0,
                                                                    oit->convertGlslangToGlaType(node->getType()),
                                                                    isMatrix, 0, 0, "constructed");
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
            oit->glaBuilder->clearAccessChain();
            oit->glaBuilder->setAccessChainLValue(constructed);
        } else {
            constructed = oit->glaBuilder->createLoad(constructed);
            if (isMatrix)
                constructed = oit->glaBuilder->createMatrixConstructor(arguments, constructed);
            else
                constructed = oit->glaBuilder->createConstructor(arguments, constructed);
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
        
        break;
    }

    //case EOpRecip:
    //    return glaBuilder->createRecip(operand);

    case EOpMul:
        // this is the case for compontent-wise matrix multiply
        binOp = EOpMul;
        break;

    case EOpMod:
    {
        // when an aggregate, this is the floating-point mod built-in function,
        // which can be emitted by the one it createBinaryOperation()
        binOp = EOpMod;
        break;
    }
    case EOpArrayLength:
        TIntermTyped* typedNode = node->getSequence()[0]->getAsTyped();
        assert(typedNode);
        gla::Builder::SuperValue length = gla::MakeIntConstant(oit->context, typedNode->getType().getArraySize());

        oit->glaBuilder->clearAccessChain();
        oit->glaBuilder->setAccessChainRValue(length);

        return false;
    }

    //
    // See if it maps to a regular operation or intrinsic.
    //

    if (binOp != EOpNull) {
        oit->glaBuilder->clearAccessChain();
        node->getSequence()[0]->traverse(oit);
        gla::Builder::SuperValue left = oit->glaBuilder->accessChainLoad();

        oit->glaBuilder->clearAccessChain();
        node->getSequence()[1]->traverse(oit);
        gla::Builder::SuperValue right = oit->glaBuilder->accessChainLoad();

        if (left.isMatrix() && binOp == EOpMul)
            result = oit->glaBuilder->createMatrixOp(llvm::Instruction::FMul, left, right);
        else
            result = oit->createBinaryOperation(binOp, left, right, reduceComparison);
        
        oit->glaBuilder->clearAccessChain();
        oit->glaBuilder->setAccessChainRValue(result);

        return false;
    }

    TIntermSequence& glslangOperands = node->getSequence();
    std::vector<gla::Builder::SuperValue> operands;
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
        gla::UnsupportedFunctionality("glslang aggregate");
    else {
        oit->glaBuilder->clearAccessChain();
        oit->glaBuilder->setAccessChainRValue(result);
    }

    return false;
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
                                                 node->getType().isMatrix(), 0, 0, "ternary");
    }

    // emit the condition before doing anything with selection
    node->getCondition()->traverse(it);

    // make an "if" based on the value created by the condition
    gla::Builder::If ifBuilder(oit->glaBuilder->accessChainLoad(), oit->glaBuilder);

    if (node->getTrueBlock()) {
        // emit the "then" statement
		node->getTrueBlock()->traverse(it);
        if (result)
            oit->glaBuilder->createStore(oit->glaBuilder->accessChainLoad(), result);
	}

    if (node->getFalseBlock()) {
        ifBuilder.makeBeginElse();
        // emit the "else" statement
        node->getFalseBlock()->traverse(it);
        if (result)
            oit->glaBuilder->createStore(oit->glaBuilder->accessChainLoad(), result);
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

void TranslateConstantUnion(TIntermConstantUnion* node, TIntermTraverser* it)
{
    TGlslangToTopTraverser* oit = static_cast<TGlslangToTopTraverser*>(it);

    int size = node->getType().getObjectSize();

    int nextConst = 0;
    gla::Builder::SuperValue c = oit->createLLVMConstant(node->getType(), node->getUnionArrayPointer(), nextConst);
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
        if (node->getBody())
            node->getBody()->traverse(it);
        bodyOut = true;
    }

    if (node->getTest()) {
        node->getTest()->traverse(it);
        // the AST only contained the test, not the branch, we have to add it

        // make the following
        //     if (! condition from test traversal)
        //         break;
        llvm::Value* condition = oit->glaBuilder->accessChainLoad();
        condition = oit->llvmBuilder.CreateNot(condition);
        gla::Builder::If ifBuilder(condition, oit->glaBuilder);
        oit->glaBuilder->makeLoopExit();
        ifBuilder.makeEndIf();
    }

    if (! bodyOut && node->getBody())
        node->getBody()->traverse(it);

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
        oit->glaBuilder->makeLoopExit();
        break;
    case EOpContinue:
        oit->glaBuilder->makeLoopBackEdge();
        break;
    case EOpReturn:
        if (oit->inMain)
            oit->glaBuilder->makeMainReturn();
        else if (node->getExpression()) {
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

gla::Builder::SuperValue TGlslangToTopTraverser::createLLVMVariable(TIntermSymbol* node, bool shadow)
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
        // Pipeline reads: If we are here, it must be to create a shadow which
        // will shadow the actual pipeline reads, which must still be done elsewhere.
        assert(shadow);
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
        // TODO: front-end needs to be fixed to tease apart the difference between 
        //       an input to a shader and an input to a function, etc.
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

    std::string name(node->getSymbol().c_str());
    if (shadow) {
        storageQualifier = gla::Builder::ESQGlobal;
        name.append("_shadow");
    }

    const llvm::Type *llvmType = convertGlslangToGlaType(node->getType());

    return glaBuilder->createVariable(storageQualifier, constantBuffer, llvmType, node->getType().isMatrix(),
                                      initializer, annotationAddr, name);
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
        llvm::Value* space = glaBuilder->createVariable(gla::Builder::ESQLocal, 0, param->getType()->getContainedType(0), false, 0, 0, "param");
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

gla::Builder::SuperValue TGlslangToTopTraverser::createBinaryOperation(TOperator op, gla::Builder::SuperValue left, gla::Builder::SuperValue right, bool reduceComparison)
{
    gla::Builder::SuperValue result;
    llvm::Instruction::BinaryOps binOp = llvm::Instruction::BinaryOps(0);
    bool needsPromotion = true;
    bool leftIsFloat = (gla::GetBasicTypeID(left) == llvm::Type::FloatTyID);
    bool isSigned = true;
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
        if (left.isMatrix() || right.isMatrix()) {
            switch(op) {
            case EOpVectorTimesMatrixAssign:
            case EOpMatrixTimesScalarAssign:
            case EOpMatrixTimesMatrixAssign:
                return glaBuilder->createMatrixMultiply(left, right);
            default:
                return glaBuilder->createMatrixOp(binOp, left, right);
            }
        }

        if (needsPromotion)
            glaBuilder->promoteScalar(left, right);

        return llvmBuilder.CreateBinOp(binOp, left, right);
    }

    if (! comparison)
        return result;

    // Comparison instructions

    if (reduceComparison && (left.isMatrix() || gla::IsVector(left) || gla::IsAggregate(left))) {
        assert(op == EOpEqual || op == EOpNotEqual);

        return glaBuilder->createCompare(left, right, op == EOpEqual);
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
        if (operand.isMatrix()) {
            // emulate by subtracting from 0.0
            llvm::Value* zero = gla::MakeFloatConstant(context, 0.0);

            return glaBuilder->createMatrixOp(llvm::Instruction::FSub, zero, operand);
        }

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
        // GLSL says true is converted to 1
        castOp = llvm::Instruction::ZExt;
        break;
    //case EOpConvBoolToUint:
    //    // GLSL says true is converted to 1
    //    castOp = llvm::Instruction::ZExt;
    //    break;
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

// TODO: remove basicType?
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
    //case EOpModF:
    //    intrinsicID = llvm::Intrinsic::gla_fModF;
    //    break;
    }

    // If intrinsic was assigned, then call the function and return
    if (intrinsicID != 0) {
        switch (operands.size()) {
        case 0:
            result = glaBuilder->createIntrinsicCall(intrinsicID);
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
            // These do not exist yet
            gla::UnsupportedFunctionality("intrinsic with more than 3 operands");
        }
    }

    return result;
}

void TGlslangToTopTraverser::createPipelineRead(TIntermSymbol* node, gla::Builder::SuperValue storage, int slot)
{
    gla::EInterpolationMethod method = gla::EIMSmooth;
    // TODO: set interpolation types
            //method = gla::EIMSmooth;
            //method = gla::EIMNoperspective;
            //method = gla::EIMNone;

    // For pipeline inputs, and we will generate a fresh pipeline read at each reference,
    // which gets optimized later.
    std::string name(node->getSymbol().c_str());
    const llvm::Type* readType;
    int endSlot = slot + 1;
    llvm::Value* pipeRead;

    if (node->getType().isArray()) {
        int arraySize = node->getType().getArraySize();
        if (arraySize == 0) {
            // TODO: make sure front end knows size before calling here, see 
            // comment in convertGlslangToGlaType
            arraySize = UnknownArraySize;
        }
        endSlot = slot + arraySize;
        TType nonArrayType = node->getType();
        nonArrayType.clearArrayness();
        readType = convertGlslangToGlaType(nonArrayType);

        // fill in the whole array
        std::vector<llvm::Value*> gepChain;
        gepChain.push_back(gla::MakeIntConstant(context, 0));
        for (int s = slot; s < endSlot; ++s) {
            std::string indexedName = name;
            // TODO: do less string manipulation if there is no GLSL back end
            gla::AppendArraySizeToName(indexedName, arraySize);
            gla::AppendArrayIndexToName(indexedName, s-slot);
            gepChain.push_back(gla::MakeIntConstant(context, s-slot));
            pipeRead = glaBuilder->readPipeline(readType, indexedName, s, -1 /*mask*/, method);
            llvmBuilder.CreateStore(pipeRead, glaBuilder->createGEP(storage, gepChain));
            gepChain.pop_back();
        }
    } else {
        readType = convertGlslangToGlaType(node->getType());
        llvm::Value* pipeRead = glaBuilder->readPipeline(readType, name, slot, -1 /*mask*/, method);
        llvmBuilder.CreateStore(pipeRead, storage);
    }
}

int TGlslangToTopTraverser::getNextInterpIndex(const std::string& name, int numSlots)
{
    // Get the index for this interpolant, or create a new unique one
    std::map<std::string, int>::iterator iter;
    iter = interpMap.find(name);

    if (interpMap.end() == iter) {
        interpMap[name] = interpIndex;
        interpIndex += numSlots;
    }

    return interpMap[name];
}

gla::Builder::SuperValue TGlslangToTopTraverser::createLLVMConstant(const TType& glslangType, constUnion *consts, int& nextConst)
{
    // vector of constants for LLVM
    std::vector<llvm::Constant*> llvmConsts;

    // Type is used for struct and array constants
    const llvm::Type* type = convertGlslangToGlaType(glslangType);

    if (glslangType.isArray()) {
        TType nonArrayType = glslangType;
        nonArrayType.clearArrayness();
        for (int i = 0; i < glslangType.getArraySize(); ++i)
            llvmConsts.push_back(llvm::dyn_cast<llvm::Constant>(createLLVMConstant(nonArrayType, consts, nextConst).getValue()));
    } else if (glslangType.isMatrix()) {
        TType vectorType = TType(glslangType.getBasicType(), EvqTemporary, glslangType.getNominalSize());
        for (int col = 0; col < glslangType.getNominalSize(); ++col)
            llvmConsts.push_back(llvm::dyn_cast<llvm::Constant>(createLLVMConstant(vectorType, consts, nextConst).getValue()));
    } else if (glslangType.getStruct()) {
        TVector<TTypeLine>::iterator iter;
        for (iter = glslangType.getStruct()->begin(); iter != glslangType.getStruct()->end(); ++iter)
            llvmConsts.push_back(llvm::dyn_cast<llvm::Constant>(createLLVMConstant(*iter->type, consts, nextConst).getValue()));
    } else {
        // a vector or scalar, both will work the same way
        // this is where we actually consume the constants, rather than walk a tree

        for (unsigned int i = 0; i < glslangType.getNominalSize(); ++i) {
            switch(consts[nextConst].getType()) {
            case EbtInt:
                llvmConsts.push_back(gla::MakeUnsignedConstant(context, consts[nextConst].getIConst()));
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
                break;
            }
            ++nextConst;
        }
    }

    if (glslangType.isMatrix())
        return glaBuilder->newMatrix(glaBuilder->getConstant(llvmConsts, type));
    else
        return glaBuilder->getConstant(llvmConsts, type);
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
