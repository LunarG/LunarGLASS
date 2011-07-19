//===- InstructionUtil.h - Utility functions for basic blocks -------------===//
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

namespace gla_llvm {
    using namespace llvm;

    // Whether the given instruction is a pipeline output
    inline bool IsOutputInstruction(const Instruction* inst)
    {
        if (const IntrinsicInst* intr = dyn_cast<IntrinsicInst>(inst)) {
            switch (intr->getIntrinsicID()) {
            case llvm::Intrinsic::gla_fWriteData:
            case llvm::Intrinsic::gla_fWriteInterpolant:
            case llvm::Intrinsic::gla_writeData:
                return true;
            }
        }

        return false;
    }

} // end namespace gla_llvm

#endif /* GLA_INSTRUCTIONUTIL_H */
