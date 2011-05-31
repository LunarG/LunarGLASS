//===- BackEndPointer.cpp - Expose backend queries  -----------------------===//
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
// Expose backend queries to passes through the BackEndPointer class, which is
// an immutable pass and behaves as a pointer to a BackEnd object.
//
//===----------------------------------------------------------------------===//

#include "Passes/Immutable/BackEndPointer.h"

#include "llvm/Pass.h"

using namespace llvm;
using namespace gla_llvm;

char BackEndPointer::ID = 0;
INITIALIZE_PASS(BackEndPointer,
                "back-end-pointer",
                "Exposes backend queries",
                false, // Whether it looks only at CFG
                true) // Whether it is an analysis pass

namespace gla_llvm {
    ImmutablePass* createBackEndPointerPass(gla::BackEnd* be)
    {
        return new BackEndPointer(be);
    }

} // end namespace gla_llvm
