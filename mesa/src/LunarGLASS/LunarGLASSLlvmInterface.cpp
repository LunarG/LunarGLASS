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
#include "LunarGLASSTopIR.h"

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
    if (type->getTypeID() == llvm::Type::VectorTyID)
        return llvm::dyn_cast<llvm::VectorType>(type)->getNumElements();
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

llvm::Type::TypeID Util::getBasicType(llvm::Value* value)
{
    return getBasicType(value->getType());
}

llvm::Type::TypeID Util::getBasicType(const llvm::Type* type)
{
    switch(type->getTypeID()) {
    case llvm::Type::VectorTyID:
    case llvm::Type::ArrayTyID:
        return getBasicType(type->getContainedType(0));
    }

    assert(gla::Util::isGlaScalar(type));
    return type->getTypeID();
}

}; // end gla namespace
