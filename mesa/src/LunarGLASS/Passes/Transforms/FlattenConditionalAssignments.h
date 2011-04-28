//===- FlattenConditionalAssignments.h - Flatten conditional assignments --===//
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
// Flatten conditional assignments into select instructions.
//
// if (p)
//    v = foo;
// else
//    v = bar;
//
// Becomes something like:
//
// %v = select <ty>, i1 p, <ty> foo, <ty> bar
//
// This pass does not modify the CFG, it merely removes candidate assignments
// and replaces them with a select outside the condition. Thus, empty blocks may
// be left lying around, and it may be desirable to run a pass to clean them up.
//
//===----------------------------------------------------------------------===//

// TODO: move all these .h transform files into something analogous to
// llvm/Transforms/Scalar.h

#ifndef FLATTEN_CONDITIONAL_ASSIGNMENTS_H
#define FLATTEN_CONDITIONAL_ASSIGNMENTS_H

#include "llvm/Pass.h"
namespace llvm {
    FunctionPass* createFlattenConditionalAssignmentsPass();
} // end namespace llvm

#endif // FLATTEN_CONDITIONAL_ASSIGNMENTS_H
