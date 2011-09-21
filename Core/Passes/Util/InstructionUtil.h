//===- InstructionUtil.h - Utility functions for instructions -------------===//
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
// Provides utility functions for Instructions
//
//===----------------------------------------------------------------------===//

#ifndef GLA_INSTRUCTIONUTIL_H
#define GLA_INSTRUCTIONUTIL_H

#include "llvm/Instructions.h"
#include "llvm/IntrinsicInst.h"

namespace gla_llvm {
    using namespace llvm;

    // Whether the given instruction is a pipeline output
    inline bool IsOutputInstruction(const Instruction* inst)
    {
        if (const IntrinsicInst* intr = dyn_cast<IntrinsicInst>(inst)) {
            switch (intr->getIntrinsicID()) {
            case Intrinsic::gla_fWriteData:
            case Intrinsic::gla_fWriteInterpolant:
            case Intrinsic::gla_writeData:
                return true;
            }
        }

        return false;
    }

    inline bool IsInputInstruction(const Instruction* inst)
    {
        if (const IntrinsicInst* intr = dyn_cast<IntrinsicInst>(inst)) {
            switch (intr->getIntrinsicID()) {
            case Intrinsic::gla_fReadInterpolant:
            case Intrinsic::gla_fReadInterpolantOffset:
            case Intrinsic::gla_readData:
            case Intrinsic::gla_fReadData:
                return true;
            }
        }

        return false;
    }

    inline bool IsLoad(const Instruction* inst)
    {
        return inst->getOpcode() == Instruction::Load;
    }

    inline bool IsDiscardConditional(const Instruction* inst)
    {
        const IntrinsicInst* intrInst = dyn_cast<IntrinsicInst>(inst);
        return intrInst && intrInst->getIntrinsicID() == llvm::Intrinsic::gla_discardConditional;
    }

    inline bool IsDiscard(const Instruction* inst, bool ignoreDiscardConditionals=true)
    {
        const IntrinsicInst* intrInst = dyn_cast<IntrinsicInst>(inst);
        return intrInst && (intrInst->getIntrinsicID() == llvm::Intrinsic::gla_discard
                            || (! ignoreDiscardConditionals && IsDiscardConditional(inst)));
    }

    inline bool HasDiscard(const BasicBlock* bb)
    {
        for (BasicBlock::const_iterator instI = bb->begin(), e = bb->end(); instI != e; ++instI) {
            if (IsDiscard(instI)) {
                return true;
            }
        }

        return false;
    }

    // If the given instruction is an intrinsic that can be constant folded,
    // returns the constant result. Returns 0 if it can't do anything.
    Constant* ConstantFoldIntrinsic(const Instruction*);

} // end namespace gla_llvm

#endif /* GLA_INSTRUCTIONUTIL_H */
