//===- Passes.h - Factories prototypes for analysis/transformation passes -===//
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
//===----------------------------------------------------------------------===//
//
// Define protypes for factories that LunarGLASS analysis/transformation passes
//
//===----------------------------------------------------------------------===//

#ifndef GLA_PASSES_H
#define GLA_PASSES_H

#include "llvm/Pass.h"

namespace gla {
    class BackEnd;
} // end namespace gla

namespace gla_llvm {
    using namespace llvm;

    // Decompose instructions for LunarGLASS
    FunctionPass* createDecomposeInstsPass();

    // Coalesce insert/extracts into multiInserts
    FunctionPass* createCoalesceInsertsPass();

    // Flatten conditional assignments into select instructions. Currently only
    // simplifies instructions, deletes dead code, and then removes empty
    // conditionals
    FunctionPass* createFlattenConditionalAssignmentsPass(int threshold);

    // Canonicalize the CFG for LunarGLASS
    FunctionPass* createCanonicalizeCFGPass();

    // Canonicalize instructions for LunarGLASS
    FunctionPass* createCanonicalizeInstsPass();

    // // Alias Analysis that says nothing ever aliases.
    // ImmutablePass* createNeverAliasPass();

    // BackEnd queries
    ImmutablePass* createBackEndPointerPass(gla::BackEnd*);

    // Combine Intrinsics for LunarGLASS
    FunctionPass* createIntrinsicCombinePass();

    // Scalarize the IR
    FunctionPass* createScalarizePass();

    // Gather instructions
    FunctionPass* createGatherInstsPass();

} // end namespace gla_llvm

#endif // GLA_PASSES_H
