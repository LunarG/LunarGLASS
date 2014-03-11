//===- InstructionUtil.h - Utility functions for instructions -------------===//
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
// Provides utility functions for Instructions
//
//===----------------------------------------------------------------------===//

#ifndef GLA_INSTRUCTIONUTIL_H
#define GLA_INSTRUCTIONUTIL_H

#pragma warning(push, 1)
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#pragma warning(pop)

// LunarGLASS helpers
#include "LunarGLASSTopIR.h"
#include "Util.h"

namespace gla_llvm {
    using namespace llvm;

    // Whether the given instruction is a pipeline output
    inline bool IsOutputInstruction(const IntrinsicInst* intr)
    {
        switch (intr->getIntrinsicID()) {
        case Intrinsic::gla_fWriteData:
        case Intrinsic::gla_fWriteInterpolant:
        case Intrinsic::gla_writeData:
        case Intrinsic::gla_fWriteComponent:
        case Intrinsic::gla_fWriteInterpolantComponent:
        case Intrinsic::gla_writeComponent:
            return true;
        }

        return false;
    }
    inline bool IsOutputInstruction(const Instruction* inst)
    {
        const IntrinsicInst* intr = dyn_cast<IntrinsicInst>(inst);
        return intr && IsOutputInstruction(intr);
    }

    inline bool IsInputInstruction(const IntrinsicInst* intr)
    {
        switch (intr->getIntrinsicID()) {
        case Intrinsic::gla_fReadInterpolant:
        case Intrinsic::gla_fReadInterpolantSample:
        case Intrinsic::gla_fReadInterpolantOffset:
        case Intrinsic::gla_readData:
        case Intrinsic::gla_fReadData:
        case Intrinsic::gla_fReadInterpolantComponent:
        case Intrinsic::gla_fReadInterpolantSampleComponent:
        case Intrinsic::gla_fReadInterpolantOffsetComponent:
        case Intrinsic::gla_fSamplePosition:
        case Intrinsic::gla_readComponent:
        case Intrinsic::gla_fReadComponent:
        case Intrinsic::gla_loadComponent:
        case Intrinsic::gla_fLoadComponent:
            return true;
        }

        return false;
    }
    inline bool IsInputInstruction(const Instruction* inst)
    {
        const IntrinsicInst* intr = dyn_cast<IntrinsicInst>(inst);
        return intr && IsInputInstruction(intr);
    }

    bool IsTextureInstruction(const IntrinsicInst* intr);
    inline bool IsTextureInstruction(const Instruction* inst)
    {
        const IntrinsicInst* intr = dyn_cast<IntrinsicInst>(inst);
        return intr && IsTextureInstruction(intr);
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
