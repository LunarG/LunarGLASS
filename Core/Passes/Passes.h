//===- Passes.h - Factories prototypes for analysis/transformation passes -===//
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
    FunctionPass* createFlattenConditionalAssignmentsPass();

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


} // end namespace gla_llvm

#endif // GLA_PASSES_H
