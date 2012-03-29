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

// LunarGLASS helpers
#include "LunarGLASSTopIR.h"
#include "Util.h"

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

    inline bool IsStore(const Instruction* inst)
    {
        return inst->getOpcode() == Instruction::Store;
    }

    inline bool IsGEP(const Instruction* inst)
    {
        return inst->getOpcode() == Instruction::GetElementPtr;
    }

    inline bool IsAbs(const Instruction* inst)
    {
        const IntrinsicInst* intrInst = dyn_cast<const IntrinsicInst>(inst);

        return intrInst && (intrInst->getIntrinsicID() == Intrinsic::gla_fAbs
                            || intrInst->getIntrinsicID() == Intrinsic::gla_abs);
    }

    inline bool IsMultiInsert(const Instruction* inst)
    {
        const IntrinsicInst* miInst = dyn_cast<const IntrinsicInst>(inst);

        return miInst && (miInst->getIntrinsicID() == Intrinsic::gla_multiInsert
                          || miInst->getIntrinsicID() == Intrinsic::gla_fMultiInsert);
    }

    inline bool IsGlaSwizzle(const Instruction* inst)
    {
        const IntrinsicInst* swInst = dyn_cast<const IntrinsicInst>(inst);

        return swInst && (swInst->getIntrinsicID() == Intrinsic::gla_swizzle
                          || swInst->getIntrinsicID() == Intrinsic::gla_fSwizzle);
    }

    inline bool IsDiscardConditional(const Instruction* inst)
    {
        const IntrinsicInst* intrInst = dyn_cast<IntrinsicInst>(inst);
        return intrInst && intrInst->getIntrinsicID() == Intrinsic::gla_discardConditional;
    }

    inline bool IsDiscard(const Instruction* inst, bool ignoreDiscardConditionals=true)
    {
        const IntrinsicInst* intrInst = dyn_cast<IntrinsicInst>(inst);
        return intrInst && (intrInst->getIntrinsicID() == Intrinsic::gla_discard
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

    // Whether the multi-insert's write mask specifies that component is written to
    inline bool MultiInsertWritesComponent(unsigned int mask, unsigned int component)
    {
        return 0 != (mask & (1 << component));
    }
    inline bool MultiInsertWritesComponent(const Instruction* miInst, unsigned int component)
    {
        return MultiInsertWritesComponent(gla::GetConstantInt(miInst->getOperand(1)), component);
    }

    bool IsNegation(const Instruction* inst);

    // Given a MultiInsert, returns the channel selects designated by its
    // writemask.
    void GetMultiInsertSelects(const Instruction* miInst, SmallVectorImpl<Constant*>& channelSelects);

    // Given a MultiInsert, if all the sources designated by its writemask are
    // the same, return the source, else returns 0
    Value* GetMultiInsertUniqueSource(const Instruction* miInst);

    // Whether the given instruction really represents a swizzle. ShuffleVectors
    // over an undef and single-source MultiInserts into an undef represent swizles.
    bool RepresentsSwizzle(const Instruction* inst);
} // end namespace gla_llvm

#endif /* GLA_INSTRUCTIONUTIL_H */
