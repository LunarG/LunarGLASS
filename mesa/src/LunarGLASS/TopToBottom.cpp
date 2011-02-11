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
#include "LunarGLASSBottomIR.h"
#include "Manager.h"

void gla::PrivateManager::translateTopToBottom()
{
    printf("\n===========================================\n"
           "Starting translation from Top IR to Bottom IR\n");
    
    printf("\nFirst middle-end optimization pass\n");

    runLLVMOptimizations1();

    module->dump();

    gla::BackEnd* backEnd = gla::getBackEnd();

    int innerAoS, outerSoA;
    backEnd->getRegisterForm(outerSoA, innerAoS);
    assert(outerSoA == 1);
    assert(innerAoS == 4);

    // make sure we can decompose all the intrisics
    for (int d = 0; d < gla::EDiCount; ++d)
        assert(! backEnd->decomposeIntrinsic(d));

    printf("Finishing translation from Top IR to Bottom IR\n");
}

void gla::PrivateManager::runLLVMOptimizations1()
{    
    // Set up the function-level optimizations we want
    llvm::FunctionPassManager passManager(module);
    passManager.add(llvm::createPromoteMemoryToRegisterPass());
    passManager.add(llvm::createReassociatePass());
    passManager.add(llvm::createAggressiveDCEPass());
    passManager.add(llvm::createGVNPass());
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
}
