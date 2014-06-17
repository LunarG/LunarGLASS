//===- CodeGen.cpp - Translate GLSL IR to LunarGLASS Top IR --------------===//
//
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
// Deprecated: This uses the old functional Sh* interface.  Actual execution
// uses the new C++ style interface.
//
// Stubs for glslang compiler (post-AST) step.  We use these to trigger
// execution of the adapter to translate the glslang AST to LunarGLASS Top IR.
//
//===----------------------------------------------------------------------===//

// glslang includes
#include "glslang/Include/Common.h"
#include "glslang/Include/ShHandle.h"

// LunarGLASS includes
// TODO: soon: merge glslang and LunarGLASS option handling
//#include "StandAlone/OptionParse.h"
#include "Core/Options.h"
#include "Frontends/glslang/GlslangToTop.h"

// LLVM includes
#pragma warning(push, 1)
#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
#pragma warning(pop)

extern int TargetDefinitionVersion;
extern EProfile TargetDefinitionProfile;

#ifndef USE_LUNARGLASS_CORE

    class AdapterOnlyManager : public gla::Manager {
    public:
        AdapterOnlyManager() { }
        virtual ~AdapterOnlyManager() { delete context; }

        virtual void clear() 
        {
            delete module;
            module = 0;
        }

        virtual void createContext()
        {
            context = new llvm::LLVMContext;
        }

        virtual void translateTopToBottom() { }
        virtual void translateBottomToTarget() { }
    };

#endif  // USE_LUNARGLASS_CORE

//
// Here is where real machine specific high-level data would be defined.
//
class TGenericCompiler : public TCompiler {
public:
    TGenericCompiler(EShLanguage l, int dOptions) : TCompiler(l, infoSink), debugOptions(dOptions), targetVersion(0), targetProfile(EBadProfile) { }
    virtual bool compile(TIntermNode* root, int version = 0, EProfile profile = ENoProfile);
    void setTargetVersion(int tv) { targetVersion = tv; }
    void setTargetProfile(EProfile tp) { targetProfile = tp; }
    int getTargetVersion() { return targetVersion; }
    EProfile getTargetProfile() { return targetProfile; }

protected:
    TInfoSink infoSink;
    int debugOptions;
    int targetVersion;
    EProfile targetProfile;
};

//
// This function must be provided to create the actual
// compile object used by higher level code.  It returns
// a subclass of TCompiler.
//
TCompiler* ConstructCompiler(EShLanguage language, int debugOptions)
{
    TGenericCompiler* compiler = new TGenericCompiler(language, debugOptions);
    compiler->setTargetVersion(TargetDefinitionVersion);
    compiler->setTargetProfile(TargetDefinitionProfile);

    return compiler;
}

//
// Delete the compiler made by ConstructCompiler
//
void DeleteCompiler(TCompiler* compiler)
{
    delete compiler;
}

//
//  Translate the glslang AST to LunarGLASS Top IR
//
bool TGenericCompiler::compile(TIntermNode *root, int version, EProfile profile)
{
    gla::Options.debug = true;

    haveValidObjectCode = false;

    // Pick up the target versioning from the source, if it was not specified
    if (targetProfile == EBadProfile)
        targetProfile = profile;
    if (targetVersion == 0)
        targetVersion = version;

#ifdef USE_LUNARGLASS_CORE
    gla::Manager* glaManager = gla::getManager();
#else
    gla::Manager* glaManager = new AdapterOnlyManager();
#endif
    assert(EShLangCount < 256 && targetProfile < 256);
    glaManager->setVersion(static_cast<int>(language) << 24 | static_cast<int>(targetProfile) << 16 | targetVersion);

    TranslateGlslangToTop(root, glaManager);

    if (! (debugOptions & gla::EOptionMemoryLeakMode)) {
        if (debugOptions & gla::EOptionAssembly)
            glaManager->dump("\nTop IR:\n");

#ifdef USE_LUNARGLASS_CORE
        glaManager->translateTopToBottom();

        if (debugOptions & gla::EOptionAssembly)
            glaManager->dump("\n\nBottom IR:\n");

        glaManager->translateBottomToTarget();
#endif
    }

    glaManager->clear();

    delete glaManager;

    haveValidObjectCode = true;

    return haveValidObjectCode;
}

