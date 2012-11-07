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

// LLVM includes
#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"

// Adapter includes
#include "GlslangToTopVisitor.h"

//
// Use this class to carry along data from node to node in
// the traversal
//
class TGlaToTopTraverser : public TIntermTraverser {
public:
    TGlaToTopTraverser() { }
};

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
    TGlaToTopTraverser* oit = static_cast<TGlaToTopTraverser*>(it);

}

bool TranslateBinary(bool /* preVisit */, TIntermBinary* node, TIntermTraverser* it)
{
    TGlaToTopTraverser* oit = static_cast<TGlaToTopTraverser*>(it);

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

    case EOpIndexDirect:
    case EOpIndexIndirect:
    case EOpIndexDirectStruct:
    case EOpVectorSwizzle:

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
    case EOpEqual:
    case EOpNotEqual:
    case EOpLessThan:
    case EOpGreaterThan:
    case EOpLessThanEqual:
    case EOpGreaterThanEqual:

    case EOpVectorTimesScalar:
    case EOpVectorTimesMatrix:
    case EOpMatrixTimesVector:
    case EOpMatrixTimesScalar:
    case EOpMatrixTimesMatrix:

    case EOpLogicalOr:
    case EOpLogicalXor:
    case EOpLogicalAnd:
    default:
        ;
    }

    return true;
}

bool TranslateUnary(bool /* preVisit */, TIntermUnary* node, TIntermTraverser* it)
{
    TGlaToTopTraverser* oit = static_cast<TGlaToTopTraverser*>(it);

    switch (node->getOp()) {
    case EOpNegative:       break;
    case EOpVectorLogicalNot:
    case EOpLogicalNot:     break;
    case EOpBitwiseNot:     break;

    case EOpPostIncrement:  break;
    case EOpPostDecrement:  break;
    case EOpPreIncrement:   break;
    case EOpPreDecrement:   break;

    case EOpConvIntToBool:  break;
    case EOpConvFloatToBool:break;
    case EOpConvBoolToFloat:break;
    case EOpConvIntToFloat: break;
    case EOpConvFloatToInt: break;
    case EOpConvBoolToInt:  break;

    case EOpRadians:        break;
    case EOpDegrees:        break;
    case EOpSin:            break;
    case EOpCos:            break;
    case EOpTan:            break;
    case EOpAsin:           break;
    case EOpAcos:           break;
    case EOpAtan:           break;

    case EOpExp:            break;
    case EOpLog:            break;
    case EOpExp2:           break;
    case EOpLog2:           break;
    case EOpSqrt:           break;
    case EOpInverseSqrt:    break;

    case EOpAbs:            break;
    case EOpSign:           break;
    case EOpFloor:          break;
    case EOpCeil:           break;
    case EOpFract:          break;

    case EOpLength:         break;
    case EOpNormalize:      break;
    case EOpDPdx:           break;
    case EOpDPdy:           break;
    case EOpFwidth:         break;

    case EOpAny:            break;
    case EOpAll:            break;

    default:
        ;
    }

    return true;
}

bool TranslateAggregate(bool /* preVisit */, TIntermAggregate* node, TIntermTraverser* it)
{
    TGlaToTopTraverser* oit = static_cast<TGlaToTopTraverser*>(it);

    if (node->getOp() == EOpNull) {
        //??out.debug.message(EPrefixError, "node is still EOpNull!");
        return true;
    }

    switch (node->getOp()) {
    case EOpSequence:      return true;
    case EOpComma:         return true;
    case EOpFunction:      break;
    case EOpFunctionCall:  break;
    case EOpParameters:    break;

    case EOpConstructFloat: break;
    case EOpConstructVec2:  break;
    case EOpConstructVec3:  break;
    case EOpConstructVec4:  break;
    case EOpConstructBool:  break;
    case EOpConstructBVec2: break;
    case EOpConstructBVec3: break;
    case EOpConstructBVec4: break;
    case EOpConstructInt:   break;
    case EOpConstructIVec2: break;
    case EOpConstructIVec3: break;
    case EOpConstructIVec4: break;
    case EOpConstructMat2:  break;
    case EOpConstructMat3:  break;
    case EOpConstructMat4:  break;
    case EOpConstructStruct:  break;

    case EOpLessThan:         break;
    case EOpGreaterThan:      break;
    case EOpLessThanEqual:    break;
    case EOpGreaterThanEqual: break;
    case EOpVectorEqual:      break;
    case EOpVectorNotEqual:   break;

    case EOpMod:           break;
    case EOpPow:           break;

    case EOpAtan:          break;

    case EOpMin:           break;
    case EOpMax:           break;
    case EOpClamp:         break;
    case EOpMix:           break;
    case EOpStep:          break;
    case EOpSmoothStep:    break;

    case EOpDistance:      break;
    case EOpDot:           break;
    case EOpCross:         break;
    case EOpFaceForward:   break;
    case EOpReflect:       break;
    case EOpRefract:       break;
    case EOpMul:           break;

    default:
        ;
    }

    return true;
}

bool TranslateSelection(bool /* preVisit */, TIntermSelection* node, TIntermTraverser* it)
{
    TGlaToTopTraverser* oit = static_cast<TGlaToTopTraverser*>(it);

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
    TGlaToTopTraverser* oit = static_cast<TGlaToTopTraverser*>(it);

    int size = node->getType().getObjectSize();

    for (int i = 0; i < size; i++) {
        switch (node->getUnionArrayPointer()[i].getType()) {
        case EbtBool:
            if (node->getUnionArrayPointer()[i].getBConst())
                ;
            else
                ;
            break;
        case EbtFloat:
            {
                node->getUnionArrayPointer()[i].getFConst();
            }
            break;
        case EbtDouble:
            {
                node->getUnionArrayPointer()[i].getDConst();
            }
            break;
        case EbtInt:
            {
				node->getUnionArrayPointer()[i].getIConst();
            }
        default:
            break;
        }
    }
}

bool TranslateLoop(bool /* preVisit */, TIntermLoop* node, TIntermTraverser* it)
{
    TGlaToTopTraverser* oit = static_cast<TGlaToTopTraverser*>(it);

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

bool TranslateBranch(bool /* previsit*/, TIntermBranch* node, TIntermTraverser* it)
{
    TGlaToTopTraverser* oit = static_cast<TGlaToTopTraverser*>(it);

    switch (node->getFlowOp()) {
    case EOpKill:      break;
    case EOpBreak:     break;
    case EOpContinue:  break;
    case EOpReturn:    break;
    default:
        ;
    }

    if (node->getExpression()) {
        node->getExpression()->traverse(it);
    }

    return false;
}

//
// This function is the one to call externally to start the traversal.
//
void GlslangToTop(TIntermNode* root, gla::Manager* manager)
{
    if (root == 0)
        return;

    TGlaToTopTraverser it;

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
