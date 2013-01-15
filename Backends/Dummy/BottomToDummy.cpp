//===- BottomToDummy.cpp - Take bottom IR, and do nothing -----------------===//
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

    enum EVariableQualifier {
        EVQNone,
        EVQUniform,
        EVQGlobal,
        EVQInput,
        EVQOutput,
        EVQTemporary,
        EVQConstant,
        EVQUndef
    };
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

    void add(const llvm::Instruction* llvmInstruction, bool lastBlock, bool referencedOutsideScope=false)
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

    void beginSimpleInductiveLoop(const llvm::PHINode* phi, unsigned count)
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

