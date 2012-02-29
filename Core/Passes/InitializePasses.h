//===- InitializePasses.h - Expose pass initialization declarations -------===//
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

#ifndef GLA_INIT_PASSES_H
#define GLA_INIT_PASSES_H

// Use of llvm namespace required
namespace llvm {

    void initializeDecomposeInstsPass(PassRegistry&);

    void initializeBottomTranslatorPass(PassRegistry&);

    void initializeCanonicalizeCFGPass(PassRegistry&);

    void initializeCanonicalizeInstsPass(PassRegistry&);

    void initializeCoalesceInsertsPass(PassRegistry&);

    void initializeFlattenCondAssnPass(PassRegistry&);

    void initializeIdentifyConditionalsPass(PassRegistry&);

    void initializeIdentifyStructuresPass(PassRegistry&);

    void initializeBackEndPointerPass(PassRegistry&);

    void initializeIntrinsicCombinePass(PassRegistry&);

} // end namespace llvm

#endif // GLA_INIT_PASSES_H
