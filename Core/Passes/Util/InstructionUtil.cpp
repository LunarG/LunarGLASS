//===- InstructionUtil.cpp - Utility functions for instructions -----------===//
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

#include "Passes/Util/InstructionUtil.h"

#include <cmath>

namespace gla_llvm {
using namespace llvm;

// Calculate and return the absolute value of c.
static Constant* CalculateAbs(const Constant* c)
{
    const Type* ty = c->getType();

    // Aggregate Zero is it's own abs
    if (isa<ConstantAggregateZero>(c))
        return ConstantAggregateZero::get(ty);

    // Fold a scalar float
    if (const ConstantFP* cFloat = dyn_cast<const ConstantFP>(c))
        return ConstantFP::get(ty, abs(cFloat->getValueAPF().convertToFloat()));

    // Fold a scalar int
    if (const ConstantInt* cInt = dyn_cast<const ConstantInt>(c))
        return ConstantInt::get(ty, cInt->getValue().abs());

    // Fold a constant vector
    if (const ConstantVector* cVec = dyn_cast<const ConstantVector>(c)) {
        SmallVector<Constant*, 8> elts;
        cVec->getVectorElements(elts);

        SmallVector<Constant*, 8> newElts;
        for (SmallVector<Constant*,8>::iterator i = elts.begin(), e = elts.end(); i != e; ++i) {
            newElts.push_back(CalculateAbs(*i));
        }

        return ConstantVector::get(newElts);
    }

    assert(0 && "Unknown constant type");
    return 0;
}

Constant* ConstantFoldIntrinsic(const Instruction* inst)
{
    const IntrinsicInst* intr = dyn_cast<const IntrinsicInst>(inst);
    if (! intr)
        return 0;

    // Gather all the operands, return 0 if any are not constants
    SmallVector<Constant*, 8> ops;
    for (Instruction::const_op_iterator i = intr->op_begin(), e = intr->op_end(); i != e; ++i) {
        Constant *cOp = dyn_cast<Constant>(*i);
        if (!cOp)
            return 0;

        ops.push_back(cOp);
    }

    switch (intr->getIntrinsicID()) {
    case Intrinsic::gla_abs:
    case Intrinsic::gla_fAbs:
        return CalculateAbs(ops[0]);

    default:
        return 0;

    } // end of switch (intr->getIntrinsicID())
}

} // end namespace gla_llvm


