//===- BottomTranslator.cpp - Translate bottom IR to Generic IR -----------===//
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
// Author: Michael Ilseman, LunarG
//
// Translate bottom IR to another IR through an LLVM module pass
//
//===----------------------------------------------------------------------===//

// LLVM includes
#include "llvm/DerivedTypes.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/Intrinsics.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/IRBuilder.h"

#include <cstdio>
#include <string>
#include <map>
#include <vector>

// LunarGLASS includes
#include "Exceptions.h"
#include "LunarGLASSBackend.h"
#include "Manager.h"

namespace {
    // Code Generation Class
    class CodeGeneration : public llvm::ModulePass {
    public:
        CodeGeneration() : ModulePass(ID)
        { }

        // Module Pass implementation
        static char ID;
        bool runOnModule(llvm::Module&);
        void print(std::ostream&, const llvm::Module*) const;
        void getAnalysisUsage(llvm::AnalysisUsage&) const;

        void setBackEndTranslator(gla::BackEndTranslator* bet) { backEndTranslator = bet; }
        void setBackEnd(gla::BackEnd* be)             { backEnd = be; }
    private:
        gla::BackEndTranslator* backEndTranslator;
        gla::BackEnd* backEnd;

    };

    class BottomTranslator {
    public:
        BottomTranslator(gla::BackEndTranslator* t) : backEndTranslator(t) { }

        ~BottomTranslator() { }

        // Translate from LLVM CFG style to structured style.  This is done
        // using a stack to keep track of what is pending.

        // Also, translate from SSA form to non-SSA form (remove phi functions).
        // This is done by looking ahead for phi funtions and adding copies in
        // the phi-predecessor blocks.

        // Currently, this is done in a fragile way, assuming no loops are
        // present, and that LLVM branches must be representing if-then-else
        // constructs.
        void addFlowControl(const llvm::Instruction*, bool);

        void declarePhiCopies(const llvm::Function*);

    protected:
        void addPhiCopies(const llvm::Instruction*);

        gla::BackEndTranslator* backEndTranslator;
        std::vector<const llvm::Value*> flowControl;
    };
} // end namespace

void BottomTranslator::addFlowControl(const llvm::Instruction* llvmInstruction, bool removePhiFunctions)
{
    // Translate from LLVM CFG style to structured style.  This is done
    // using a stack to keep track of what is pending.

    // Also, translate from SSA form to non-SSA form (remove phi functions).
    // This is done by looking ahead for phi funtions and adding copies in
    // the phi-predecessor blocks.

    // Currently, this is done in a fragile way, assuming no loops are
    // present, and that LLVM branches must be representing if-then-else
    // constructs.

    if (removePhiFunctions) {
        // All branches that branch to a block having phi instructions for that
        // branch need copies inserted.
        addPhiCopies(llvmInstruction);
    }

    switch (llvmInstruction->getNumOperands()) {
    case 1:
        // We are doing an unconditional branch
        // Assume it is to the merge of the if-then-else or the if-then
        if (flowControl.back() == llvmInstruction->getOperand(0)) {
            // This must be the end of the if block
            backEndTranslator->addEndif();
            flowControl.pop_back();
        } else {
            // This must be the end of a then that has as else
            backEndTranslator->addElse();
            flowControl.pop_back();
            flowControl.push_back(llvmInstruction->getOperand(0));
        }
        break;
    case 3:
        // We are splitting into two children.
        // Assume we are entering an if-then-else statement or if-then statement.
        flowControl.push_back(llvmInstruction->getOperand(1));
        backEndTranslator->addIf(llvmInstruction->getOperand(0));
        break;
    default:
        gla::UnsupportedFunctionality("Flow Control in Bottom IR");
    }
}

void BottomTranslator::declarePhiCopies(const llvm::Function* function)
{
    // basic blocks
    for (llvm::Function::const_iterator bb = function->begin(), E = function->end(); bb != E; ++bb) {

        // instructions in the basic block
        for (llvm::BasicBlock::const_iterator i = bb->begin(), e = bb->end(); i != e; ++i) {
            const llvm::Instruction* llvmInstruction = i;

            if (llvmInstruction->getOpcode() == llvm::Instruction::PHI)
                backEndTranslator->declarePhiCopy(llvmInstruction);
        }
    }
}

void BottomTranslator::addPhiCopies(const llvm::Instruction* llvmInstruction)
{
    // for each child block
    for (unsigned int op = 0; op < llvmInstruction->getNumOperands(); ++op) {

        // get the destination block (not all operands are blocks, but consider each that is)
        const llvm::BasicBlock *phiBlock = llvm::dyn_cast<llvm::BasicBlock>(llvmInstruction->getOperand(op));
        if (! phiBlock)
            continue;

        // for each llvm phi node, add a copy instruction
        for (llvm::BasicBlock::const_iterator i = phiBlock->begin(), e = phiBlock->end(); i != e; ++i) {
            const llvm::Instruction* destInstruction = i;
            const llvm::PHINode *phiNode = llvm::dyn_cast<llvm::PHINode>(destInstruction);

            if (phiNode) {
                // find the operand whose predecessor is us
                // each phi operand takes up two normal operands,
                // so don't directly access operands; use the Index encapsulation
                int predIndex = phiNode->getBasicBlockIndex(llvmInstruction->getParent());
                if (predIndex >= 0) {
                    // then we found ourselves
                    backEndTranslator->addPhiCopy(phiNode, phiNode->getIncomingValue(predIndex));
                }
            }
        }
    }
}

bool CodeGeneration::runOnModule(llvm::Module& module)
{
    BottomTranslator translator(backEndTranslator);

    //
    // Query the back end about its flow control
    //
    bool breakOp, continueOp, earlyReturnOp, discardOp;
    gla::EFlowControlMode flowControlMode;
    backEnd->getControlFlowMode(flowControlMode, breakOp, continueOp, earlyReturnOp, discardOp);
    if (flowControlMode == gla::EFcmExplicitMasking)
        gla::UnsupportedFunctionality("explicit masking in middle end");

    //
    // Translate globals.
    //
    for (llvm::Module::const_global_iterator global = module.global_begin(), end = module.global_end(); global != end; ++global)
        backEndTranslator->addGlobal(global);

    //
    // Translate code.
    //
    llvm::Module::const_iterator function, lastFunction;
    for (function = module.begin(), lastFunction = module.end(); function != lastFunction; ++function) {
        if (function->isDeclaration()) {
            //?? do we need to handle declarations of functions, or just definitions?
        } else {
            // handle function's with bodies

            // fast HACK for LunarGOO to not emit functions, because they were all inlined, but still lying around
            if (function->getNameStr() != std::string("main"))
                continue;

            backEndTranslator->startFunctionDeclaration(function->getFunctionType(), function->getNameStr());

            // paramaters and arguments
            for (llvm::Function::const_arg_iterator arg = function->arg_begin(), endArg = function->arg_end(); arg != endArg; ++arg) {
                llvm::Function::const_arg_iterator nextArg = arg;
                ++nextArg;
                backEndTranslator->addArgument(arg, nextArg == endArg);
            }

            backEndTranslator->endFunctionDeclaration();

            backEndTranslator->startFunctionBody();

            // Phi declaration pass
            if (backEnd->getDeclarePhiCopies())
                translator.declarePhiCopies(function);

            // basic blocks
            for (llvm::Function::const_iterator bb = function->begin(), E = function->end(); bb != E; ++bb) {
                bool lastBlock = (bb->getNextNode() == E);

                // instructions in the basic block
                for (llvm::BasicBlock::const_iterator i = bb->begin(), e = bb->end(); i != e; ++i) {
                    const llvm::Instruction* llvmInstruction = i;

                    //?? what are compare llvmInstruction predicates
                    // if (const CmpInst *CI = dyn_cast<CmpInst>(&llvmInstruction))

                    if (llvmInstruction->getOpcode() == llvm::Instruction::Br && flowControlMode == gla::EFcmStructuredOpCodes)
                        translator.addFlowControl(llvmInstruction, backEnd->getRemovePhiFunctions());
                    else {
                        if (! (backEnd->getRemovePhiFunctions() && llvmInstruction->getOpcode() == llvm::Instruction::PHI))
                            backEndTranslator->add(llvmInstruction, lastBlock);
                    }
                }
            }

            backEndTranslator->endFunctionBody();
        }
    }

    //set_branchtargets(&v, currentInstructionructions, num_instructions);
    backEndTranslator->print();

    return false;
}

void CodeGeneration::getAnalysisUsage(llvm::AnalysisUsage& AU) const
{
    AU.addRequired<llvm::LoopInfo>();
    return;
}

char CodeGeneration::ID = 0;

namespace llvm {
    INITIALIZE_PASS(CodeGeneration,
                    "code-gen",
                    "LunarGLASS code generation pass",
                    true,   // Whether it preserves the CFG
                    false); // Whether it is an analysis pass
} // end namespace llvm

namespace gla {
    llvm::ModulePass* createCodeGenerationPass(BackEndTranslator* bet, BackEnd* be)
    {
        CodeGeneration* cgp = new CodeGeneration();
        cgp->setBackEndTranslator(bet);
        cgp->setBackEnd(be);
        return cgp;
    }
} // end gla namespace

void gla::PrivateManager::translateBottomToTarget()
{
    llvm::PassManager passManager;
    passManager.add(gla::createCodeGenerationPass(backEndTranslator, backEnd));
    passManager.run(*module);
}
