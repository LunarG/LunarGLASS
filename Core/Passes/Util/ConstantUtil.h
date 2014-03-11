//===- ConstantUtil.h - Utility functions for constants -------------------===//
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
// Provides utility functions for Constants
//
//===----------------------------------------------------------------------===//

#ifndef GLA_CONSTANTUTIL_H
#define GLA_CONSTANTUTIL_H

#pragma warning(push, 1)
#include "llvm/IR/Constants.h"
#pragma warning(pop)

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

        if (ConstantDataVector* cVector = dyn_cast<ConstantDataVector>(c)) {
            Constant* splat = cVector->getSplatValue();
            return splat && IsOne(splat);
        }

        if (ConstantInt* cInt = dyn_cast<ConstantInt>(c))
            return cInt->equalsInt(1);

        return false;
    }

    // Get all the elements of an aggregate data type. Will fill with zeros for
    // ConstantAggregateZero and undefs for UndefValues. Result may contain
    // NULLs from calls to getAggregateElement(). Currently only supports
    // vectors.
    inline void GetElements(const Constant* c, SmallVectorImpl<Constant*>& res)
    {
        // TODO LLVM 3.2: handle ConstantDataVector
        for (int i = 0; i < gla::GetComponentCount(c); ++i)
            res.push_back(c->getAggregateElement((unsigned)i));
    }

} // end namespace gla_llvm

#endif /* GLA_CONSTANTUTIL_H */
