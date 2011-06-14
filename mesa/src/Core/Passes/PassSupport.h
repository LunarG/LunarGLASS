//===- PassSupport.h - Pass support (e.g. initialization) -----------------===//
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
// Expose pass initialization declarations
//
//===----------------------------------------------------------------------===//

// TODO: consider name change

#ifndef GLA_PASS_SUPPORT_H
#define GLA_PASS_SUPPORT_H

#include "llvm/PassSupport.h"

// Include initialization prototypes
#include "Passes/InitializePasses.h"

// Include creation prototypes
#include "Passes/Passes.h"

#endif // GLA_PASS_SUPPORT_H
