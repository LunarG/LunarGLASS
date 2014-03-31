//===- GlslangToTop.cpp - Translate GLSL IR to LunarGLASS Top IR ---------===//
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
// Author: John Kessenich, LunarG
//
// Translate glslang to LunarGLASS Top IR.
//
//===----------------------------------------------------------------------===//

// LLVM includes
#pragma warning(push, 1)
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/IR/IRBuilder.h"
#pragma warning(pop)

#include <cstdio>
#include <string>
#include <map>
#include <vector>

// Glslang includes
#include "glslang/MachineIndependent/localintermediate.h"

// Glslang deprecated includes
#include "glslang/Include/intermediate.h"

// LunarGLASS includes
#include "LunarGLASSManager.h"

// Adapter includes
#include "GlslangToTopVisitor.h"

// Glslang new C++ interface
void TranslateGlslangToTop(glslang::TIntermediate& intermediate, gla::Manager& manager)
{
    llvm::Module* topModule = new llvm::Module("Glslang", llvm::getGlobalContext());
    manager.setModule(topModule);

    GlslangToTop(intermediate, manager);

    manager.setVersion(intermediate.getVersion());
    manager.setProfile(intermediate.getProfile());
    manager.setStage(intermediate.getStage());
    manager.setRequestedExtensions(intermediate.getRequestedExtensions());
}

// Glslang deprecated interface
void TranslateGlslangToTop(TIntermNode* root, gla::Manager* manager)
{
    llvm::Module* topModule = new llvm::Module("Top", llvm::getGlobalContext());
    manager->setModule(topModule);

    GlslangToTop(root, manager);
}
