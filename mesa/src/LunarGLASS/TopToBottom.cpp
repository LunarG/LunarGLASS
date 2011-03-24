//===- TopToBottom.cpp - Translate Top IR to Bottom IR --------------------===//
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
// Author: John Kessenich, LunarG
//
// Translate Top IR to Bottom IR
//
//===----------------------------------------------------------------------===//

// LLVM includes
#include "llvm/DerivedTypes.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/IRBuilder.h"
#include <cstdio>
#include <string>
#include <map>
#include <vector>

// LunarGLASS includes
#include "Exceptions.h"
#include "LunarGLASSBackend.h"
#include "Manager.h"
#include "TgsiTarget.h"
#include "GlslTarget.h"
#include "Options.h"

// LunarGLASS Passes
#include "Passes/Transforms/CoalesceInserts/CoalesceInserts.h"

void gla::PrivateManager::translateTopToBottom()
{
#ifdef _WIN32
    unsigned int oldFormat = _set_output_format(_TWO_DIGIT_EXPONENT);
#endif

    if (gla::Options.debug)
        module->dump();

    runLLVMOptimizations1();

    if (gla::Options.debug)
        module->dump();
#ifdef _WIN32
    _set_output_format(oldFormat);
#endif

    int innerAoS, outerSoA;
    backEnd->getRegisterForm(outerSoA, innerAoS);
    if (outerSoA != 1)
        UnsupportedFunctionality("SoA in middle end: ", outerSoA);
    if (innerAoS != 4)
        UnsupportedFunctionality("AoS other than size 4 in middle end: ", innerAoS);

    // make sure we can decompose all the intrisics
    for (int d = 0; d < gla::EDiCount; ++d) {
        if (backEnd->decomposeIntrinsic(d))
            UnsupportedFunctionality("intrinsic decomposition in middle end");
    }
}

void gla::PrivateManager::runLLVMOptimizations1()
{
    // Set up the function-level optimizations we want
    llvm::FunctionPassManager passManager(module);
    if (Options.optimizations.verify)      passManager.add(llvm::createVerifierPass());
    if (Options.optimizations.mem2reg)     passManager.add(llvm::createPromoteMemoryToRegisterPass());
    if (Options.optimizations.reassociate) passManager.add(llvm::createReassociatePass());
    if (Options.optimizations.gvn)         passManager.add(llvm::createGVNPass());
    if (Options.optimizations.coalesce)    passManager.add(llvm::createCoalesceInsertsPass());
    if (Options.optimizations.adce)        passManager.add(llvm::createAggressiveDCEPass());
    //    passManager.add(llvm::createCFGSimplificationPass());
    if (Options.optimizations.verify)      passManager.add(llvm::createVerifierPass());
    llvm::Module::iterator function, lastFunction;

    // run them across all functions
    passManager.doInitialization();
    for (function = module->begin(), lastFunction = module->end(); function != lastFunction; ++function) {
        passManager.run(*function);
    }
    passManager.doFinalization();

    // Set up the module-level optimizations we want
    llvm::PassManager modulePassManager;
    modulePassManager.add(llvm::createGlobalOptimizerPass());

    // Optimize the whole module
    bool changed = modulePassManager.run(*module);

    if (changed) {
        // removing globals created stack allocations we want to eliminate
        llvm::FunctionPassManager postGlobalManager(module);
        postGlobalManager.add(llvm::createPromoteMemoryToRegisterPass());

        // run across all functions
        postGlobalManager.doInitialization();
        for (function = module->begin(), lastFunction = module->end(); function != lastFunction; ++function) {
            postGlobalManager.run(*function);
        }
        postGlobalManager.doFinalization();
    }

    if (! backEnd->preferRegistersOverMemory()) {
        llvm::FunctionPassManager memoryPassManager(module);
        memoryPassManager.add(llvm::createDemoteRegisterToMemoryPass());

        memoryPassManager.doInitialization();
        for (function = module->begin(), lastFunction = module->end(); function != lastFunction; ++function) {
            memoryPassManager.run(*function);
        }
        memoryPassManager.doFinalization();
    }
}
