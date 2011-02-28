//===- BottomConverter.cpp - Translate bottom IR to Tgsi ------------------===//
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
//
// Translate bottom IR to another IR by doing a manual traversal of the LLVM.
//
// Note:  Modeled after LLVM/llvm-2.8/lib/VMCore/AsmWriter.cpp
//
//===----------------------------------------------------------------------===//

// LLVM includes
#include "llvm/DerivedTypes.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/Intrinsics.h"

#include <cstdio>
#include <string>
#include <map>
#include <vector>

// LunarGLASS includes
#include "LunarGLASSBackend.h"
#include "Manager.h"

namespace gla {
    class BottomTranslator;
};

class gla::BottomTranslator {
public:
    BottomTranslator(gla::BackEndTranslator* t) : backEndTranslator(t) { }

    ~BottomTranslator() { }

    void addFlowControl(const llvm::Instruction* llvmInstruction, bool removePhiFunctions)
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
        case 2:
            assert (! "Unexpected number of operands for LLVM branch");
            break;
        case 3:
            // We are splitting into two children.
            // Assume we are entering an if-then-else statement or if-then statement.
            flowControl.push_back(llvmInstruction->getOperand(1));
            backEndTranslator->addIf(llvmInstruction->getOperand(0));
            break;
        default:
            printf ("UNSUPPORTED llvm flow control number of operands\n");
        }
    }

    void declarePhiCopies(const llvm::Function* function)
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

protected:

    void addPhiCopies(const llvm::Instruction* llvmInstruction)
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

    gla::BackEndTranslator* backEndTranslator;
    std::vector<const llvm::Value*> flowControl;
};

void gla::PrivateManager::translateBottomToTarget()
{
    // Initial creation of target.

    // In the real driver, this is is done through a call through the function
    // pointer ctx->Driver.NewProgram(...), so directly call a funtion here
    // that does the same thing, and could later be plugged into that pointer.

    gla::BottomTranslator translator(backEndTranslator);
    bool breakOp, continueOp, earlyReturnOp, discardOp;
    gla::EFlowControlMode flowControlMode;

    backEnd->getControlFlowMode(flowControlMode, breakOp, continueOp, earlyReturnOp, discardOp);
    assert(flowControlMode != gla::EFcmExplicitMasking);

    //
    // Translate globals.
    //
    for (llvm::Module::const_global_iterator global = module->global_begin(), end = module->global_end(); global != end; ++global)
        backEndTranslator->addGlobal(global);

    //
    // Translate code.
    //
    llvm::Module::const_iterator function, lastFunction;
    for (function = module->begin(), lastFunction = module->end(); function != lastFunction; ++function) {
        if (function->isDeclaration()) {
            //?? do we need to handle declarations of functions, or just definitions?
        } else {
            // handle function's with bodies
            backEndTranslator->startFunction();

            // paramaters and arguments
            for (llvm::Function::const_arg_iterator P = function->arg_begin(), E = function->arg_end(); P != E; ++P) {
                assert (! "function arguments not supported");
                //?? argument is Attrs.getParamAttributes(Idx));  // Idx has to count as you go through the loop
            }

            // Phi declaration pass
            if (backEnd->getDeclarePhiCopies())
                translator.declarePhiCopies(function);

            // basic blocks
            for (llvm::Function::const_iterator bb = function->begin(), E = function->end(); bb != E; ++bb) {

                // instructions in the basic block
                for (llvm::BasicBlock::const_iterator i = bb->begin(), e = bb->end(); i != e; ++i) {
                    const llvm::Instruction* llvmInstruction = i;

                    //?? what are compare llvmInstruction predicates
                    // if (const CmpInst *CI = dyn_cast<CmpInst>(&llvmInstruction))

                    if (llvmInstruction->getOpcode() == llvm::Instruction::Br && flowControlMode == EFcmStructuredOpCodes)
                        translator.addFlowControl(llvmInstruction, backEnd->getRemovePhiFunctions());
                    else {
                        if (! (backEnd->getRemovePhiFunctions() && llvmInstruction->getOpcode() == llvm::Instruction::PHI))
                            backEndTranslator->add(llvmInstruction);
                    }
                }
            }

            backEndTranslator->endFunction();
        }
    }

    //set_branchtargets(&v, currentInstructionructions, num_instructions);
    backEndTranslator->print();
}
