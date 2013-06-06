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
#include "llvm/Assembly/PrintModulePass.h"
#include "llvm/DerivedTypes.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/DataLayout.h"
#include <cstdio>
#include <string>
#include <map>
#include <vector>

// LunarGLASS includes
#include "Exceptions.h"
#include "Backend.h"
#include "PrivateManager.h"
#include "Options.h"

// LunarGLASS Passes
#include "Passes/Passes.h"

void gla::PrivateManager::translateTopToBottom()
{
#ifdef _WIN32
    unsigned int oldFormat = _set_output_format(_TWO_DIGIT_EXPONENT);
#endif

    runLLVMOptimizations1();

#ifdef _WIN32
    _set_output_format(oldFormat);
#endif

    int innerAoS, outerSoA;
    backEnd->getRegisterForm(outerSoA, innerAoS);
    if (outerSoA != 1)
        UnsupportedFunctionality("SoA in middle end: ", outerSoA);
    if (innerAoS != 4 && innerAoS != 1)
        UnsupportedFunctionality("AoS other than size 4 or 1 in middle end: ", innerAoS);
}

void gla::PrivateManager::dump(const char *heading)
{
    llvm::errs() << heading;
    module->dump();
}

static inline void RunOnModule(llvm::FunctionPassManager& pm, llvm::Module* m)
{
    pm.doInitialization();
    for (llvm::Module::iterator f = m->begin(), e = m->end(); f != e; ++f)
        pm.run(*f);
    pm.doFinalization();
}

// Verify each function
static inline void VerifyModule(llvm::Module* module)
{
#ifndef NDEBUG

    llvm::FunctionPassManager verifier(module);
    verifier.add(llvm::createVerifierPass());
    RunOnModule(verifier, module);

#endif // NDEBUG
}

void gla::PrivateManager::runLLVMOptimizations1()
{
    VerifyModule(module);

    // TODO: When we have backend support for shuffles, or we canonicalize
    // shuffles into multiinserts, we can replace the InstSimplify passes with
    // InstCombine passes.

    // First, do some global (module-level) optimizations, which can free up
    // function passes to do more.
    llvm::PassManager globalPM;
    globalPM.add(llvm::createGlobalOptimizerPass());
    globalPM.add(llvm::createIPSCCPPass());
    globalPM.add(llvm::createConstantMergePass());
    globalPM.add(llvm::createInstructionSimplifierPass());
    globalPM.add(llvm::createAlwaysInlinerPass());
    globalPM.add(llvm::createPromoteMemoryToRegisterPass());
    globalPM.run(*module);

    // Next, do interprocedural passes
    // Future work: If we ever have non-inlined functions, we'll want to add some

    VerifyModule(module);

    // Set up the function-level optimizations we want
    // TODO: explore ordering of passes more
    llvm::FunctionPassManager passManager(module);


    // Add target data to unblock optimizations that require it
    // This matches default except for endianness (little) and pointer size/alignment (32)
    llvm::DataLayout* DL = new llvm::DataLayout("e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64");
    passManager.add(DL);

    // Create immutable passes once
    passManager.add(llvm::createBasicAliasAnalysisPass());
    passManager.add(llvm::createTypeBasedAliasAnalysisPass());

    // Provide the backend queries
    passManager.add(gla_llvm::createBackEndPointerPass(backEnd));

    // TODO: explore SimplifyLibCalls
    // TODO: see if we can avoid running gvn/sccp multiple times

    // Early, simple optimizations to enable others/make others more efficient
    passManager.add(llvm::createScalarReplAggregatesPass());
    passManager.add(llvm::createInstructionCombiningPass());
    passManager.add(llvm::createEarlyCSEPass());
    passManager.add(llvm::createCorrelatedValuePropagationPass());

    passManager.add(llvm::createCFGSimplificationPass());
    passManager.add(llvm::createLoopSimplifyPass());
    passManager.add(gla_llvm::createCanonicalizeCFGPass());
    passManager.add(gla_llvm::createDecomposeInstsPass());
    passManager.add(gla_llvm::createCanonicalizeCFGPass());
    passManager.add(gla_llvm::createFlattenConditionalAssignmentsPass());
    passManager.add(gla_llvm::createCanonicalizeCFGPass());

    int innerAoS, outerSoA;
    backEnd->getRegisterForm(outerSoA, innerAoS);
    if (innerAoS == 1) {
        passManager.add(gla_llvm::createScalarizePass());
    }

    passManager.add(llvm::createReassociatePass());
    passManager.add(llvm::createInstructionCombiningPass());

    passManager.add(llvm::createGVNPass());
    passManager.add(llvm::createSCCPPass());

    passManager.add(llvm::createLoopSimplifyPass());
    passManager.add(gla_llvm::createCanonicalizeCFGPass());
    passManager.add(gla_llvm::createFlattenConditionalAssignmentsPass());
    passManager.add(gla_llvm::createCanonicalizeCFGPass());

    // Make multiinsert intrinsics, and clean up afterwards
    passManager.add(llvm::createInstructionCombiningPass());
    passManager.add(gla_llvm::createCoalesceInsertsPass());
    passManager.add(llvm::createAggressiveDCEPass());
    passManager.add(llvm::createInstructionCombiningPass());

    // Loop optimizations, and clean up afterwards
    passManager.add(llvm::createLICMPass());
    passManager.add(llvm::createIndVarSimplifyPass());
    passManager.add(llvm::createLoopRotatePass());
    passManager.add(llvm::createIndVarSimplifyPass());
    passManager.add(llvm::createLoopUnrollPass(350));
    passManager.add(llvm::createLoopStrengthReducePass());
    passManager.add(llvm::createAggressiveDCEPass());

    passManager.add(llvm::createInstructionCombiningPass());
    passManager.add(llvm::createGVNPass());
    passManager.add(llvm::createSCCPPass());

    // Run intrinisic combining
    passManager.add(gla_llvm::createCanonicalizeCFGPass());
    passManager.add(llvm::createInstructionCombiningPass());
    passManager.add(gla_llvm::createIntrinsicCombinePass());
    passManager.add(gla_llvm::createCanonicalizeCFGPass());

    passManager.add(llvm::createGVNPass());
    passManager.add(llvm::createSCCPPass());

    // TODO: Consider if we really want it. For some reason StandardPasses.h
    // doesn't have it listed.
    // passManager.add(llvm::createSinkingPass());

    // Run some post-redundancy-elimination passes
    passManager.add(llvm::createScalarReplAggregatesPass());
    passManager.add(llvm::createInstructionCombiningPass());
    passManager.add(llvm::createCorrelatedValuePropagationPass());
    passManager.add(llvm::createAggressiveDCEPass());

    // LunarGLASS CFG optimizations
    passManager.add(llvm::createLoopSimplifyPass());
    passManager.add(gla_llvm::createCanonicalizeCFGPass());
    passManager.add(gla_llvm::createFlattenConditionalAssignmentsPass());
    passManager.add(gla_llvm::createCanonicalizeCFGPass());

    passManager.add(llvm::createInstructionCombiningPass());
    passManager.add(llvm::createAggressiveDCEPass());

    RunOnModule(passManager, module);

    VerifyModule(module);

    // Post Function passes cleanup
    llvm::PassManager pm;
    pm.add(llvm::createInstructionCombiningPass());
    pm.add(llvm::createDeadStoreEliminationPass());
    pm.add(llvm::createAggressiveDCEPass());
    pm.add(llvm::createStripDeadPrototypesPass());
    
    // TODO: Consider using the below in the presense of functions
    // pm.add(llvm::createGlobalDCEPass());

    pm.run(*module);

    VerifyModule(module);

    // TODO: Refactor the below use of GlobalOpt. Perhaps we want to repeat our
    // some function passes?

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
        for (llvm::Module::iterator function = module->begin(), lastFunction = module->end(); function != lastFunction; ++function) {
            postGlobalManager.run(*function);
        }
        postGlobalManager.doFinalization();
    }

    if (! backEnd->preferRegistersOverMemory()) {
        llvm::FunctionPassManager memoryPassManager(module);
        memoryPassManager.add(llvm::createDemoteRegisterToMemoryPass());

        memoryPassManager.doInitialization();
        for (llvm::Module::iterator function = module->begin(), lastFunction = module->end(); function != lastFunction; ++function) {
            memoryPassManager.run(*function);
        }
        memoryPassManager.doFinalization();
    }

    VerifyModule(module);

    // Put the IR into a canonical form for BottomTranslator.
    llvm::PassManager canonicalize;

    canonicalize.add(llvm::createIndVarSimplifyPass());
    canonicalize.add(gla_llvm::createCanonicalizeCFGPass());
    canonicalize.add(gla_llvm::createBackEndPointerPass(backEnd));
    canonicalize.add(gla_llvm::createGatherInstsPass());
    canonicalize.add(gla_llvm::createCanonicalizeInstsPass());
    canonicalize.add(llvm::createStripDeadPrototypesPass());
    canonicalize.run(*module);

    VerifyModule(module);
}
