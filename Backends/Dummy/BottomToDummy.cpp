//===- BottomToDummy.cpp - Take bottom IR, and do nothing -----------------===//
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
// Author: Michael Ilseman, LunarG
//
// Take bottom IR, and do nothing
//
//===----------------------------------------------------------------------===//

#include <cstdio>
#include <string>
#include <sstream>
#include <map>
#include <vector>
#include <cstdio>

// LunarGLASS includes
#include "Revision.h"
#include "Exceptions.h"
#include "Util.h"
#include "BottomIR.h"
#include "Backend.h"
#include "PrivateManager.h"
#include "DummyTarget.h"
#include "Options.h"

//
// Implement the Dummy backend
//
class DummyBackEnd : public gla::BackEnd {
public:
    DummyBackEnd() { }
    virtual ~DummyBackEnd() { }

    virtual void getRegisterForm(int& outerSoA, int& innerAoS)
    {
        gla::BackEnd::getRegisterForm(outerSoA, innerAoS);
    }

    virtual void getControlFlowMode(gla::EFlowControlMode& flowControlMode,
                                    bool& breakOp, bool& continueOp,
                                    bool& earlyReturnOp, bool& discardOp)
    {
        gla::BackEnd::getControlFlowMode(flowControlMode, breakOp, continueOp,
                                         earlyReturnOp, discardOp);
    }

    virtual bool getDeclarePhiCopies()
    {
        return true;
    }

    // virtual bool decomposeOrd() const
    // {
    //     return true;
    // }

};

//
// factory for the Dummy backend
//
gla::BackEnd* gla::GetDummyBackEnd()
{
    return new DummyBackEnd();
}

void gla::ReleaseDummyBackEnd(gla::BackEnd* backEnd)
{
    delete backEnd;
}

//
// Implement the Bottom IR -> Dummy translator
//
namespace gla {
    class DummyTarget;
};

class gla::DummyTarget : public gla::BackEndTranslator {
public:
    DummyTarget(Manager* m) : BackEndTranslator(m)
    { counter = 0; }

    ~DummyTarget()
    { }

    void addStructType(llvm::StringRef, const llvm::Type*)
    { counter++; }

    void addGlobal(const llvm::GlobalVariable*)
    { counter++; }

    void startFunctionDeclaration(const llvm::Type* type, llvm::StringRef name)
    { counter++; }

    void addArgument(const llvm::Value* value, bool last)
    { counter++; }

    void endFunctionDeclaration()
    { counter++; }

    void startFunctionBody()
    { counter++; }

    void endFunctionBody()
    { counter++; }

    void addInstruction(const llvm::Instruction* llvmInstruction, bool lastBlock, bool referencedOutsideScope=false)
    {
        counter++;

        counter += llvmInstruction->getNumOperands();

        const llvm::IntrinsicInst* intr = llvm::dyn_cast<llvm::IntrinsicInst>(llvmInstruction);
        if (intr) {
            counter += intr->getIntrinsicID();
        }

    }

    void declarePhiCopy(const llvm::Value* dst)
    { counter++; }

    void addPhiCopy(const llvm::Value* dst, const llvm::Value* src)
    { counter++; }

    void addIf(const llvm::Value* cond, bool invert=false)
    { counter++; }

    void addElse()
    { counter++; }

    void addEndif()
    { counter++; }

    void beginConditionalLoop()
    { counter++; }

    void beginSimpleConditionalLoop(const llvm::CmpInst* cmp, const llvm::Value* op1, const llvm::Value* op2, bool invert=false)
    { counter++; }

    void beginForLoop(const llvm::PHINode* phi, llvm::ICmpInst::Predicate, unsigned bound, unsigned increment)
    { counter++; }

    void beginSimpleInductiveLoop(const llvm::PHINode* phi, const llvm::Value* count)
    { counter++; }

    void beginLoop()
    { counter++; }

    void endLoop()
    { counter++; }

    void addLoopExit(const llvm::Value* condition=NULL, bool invert=false)
    { counter++; }

    void addLoopBack(const llvm::Value* condition=NULL, bool invert=false)
    { counter++; }

    void addDiscard()
    { counter++; }

    void print()
    { printf("%d\n", counter); }

protected:
    int counter;
};

//
// Factory for Dummy back-end translator
//
gla::BackEndTranslator* gla::GetDummyTranslator(Manager* manager)
{
    return new gla::DummyTarget(manager);
}

void gla::ReleaseDummyTranslator(gla::BackEndTranslator* target)
{
    delete target;
}

