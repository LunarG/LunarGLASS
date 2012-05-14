//===- ConstantUtil.h - Utility functions for constants -------------------===//
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
// Provides utility functions for Constants
//
//===----------------------------------------------------------------------===//

#ifndef GLA_CONSTANTUTIL_H
#define GLA_CONSTANTUTIL_H

#include "llvm/Constants.h"

// LunarGLASS helpers
#include "LunarGLASSTopIR.h"
#include "Util.h"

namespace gla_llvm {
    using namespace llvm;

    inline bool IsOne(Constant* c)
    {
        if (ConstantFP* cFP = dyn_cast<ConstantFP>(c))
            return cFP->isExactlyValue(1.0);

        if (ConstantVector* cVector = dyn_cast<ConstantVector>(c)) {
            Constant* splat = cVector->getSplatValue();
            return splat && IsOne(splat);
        }

        if (ConstantInt* cInt = dyn_cast<ConstantInt>(c))
            return cInt->equalsInt(1);

        return false;
    }

    // Get the given constant's component, e.g. a scalar undef for any component
    // of a UndefValue vector. Returns 0 if unable to get a component
    inline Constant* GetComponentFromConstant(Constant* c, int component)
    {
        if (ConstantVector* cv = dyn_cast<ConstantVector>(c)) {
            return cv->getOperand(component);
        } else if (isa<ConstantAggregateZero>(c)) {
            return Constant::getNullValue(gla::GetBasicType(c));
        } else if (isa<UndefValue>(c)) {
            return UndefValue::get(gla::GetBasicType(c));
        }

        return 0;
    }

} // end namespace gla_llvm

#endif /* GLA_CONSTANTUTIL_H */
