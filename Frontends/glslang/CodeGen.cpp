//===- CodeGen.cpp - Translate GLSL IR to LunarGLASS Top IR --------------===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (C) 2012 LunarG, Inc.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice (including the next
// paragraph) shall be included in all copies or substantial portions of the
// Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//
//===----------------------------------------------------------------------===//
//
// Author: John Kessenich, LunarG
//
// Stubs for glslang compiler (post-AST) step.  We use these to trigger
// execution of the adapter to translate the glslang AST to LunarGLASS Top IR.
//
//===----------------------------------------------------------------------===//

// glslang includes
#include "../glslang/Include/Common.h"
#include "../glslang/Include/ShHandle.h"

// LunarGLASS runtime options handling
// TODO: merge glslang and LunarGLASS option handling
//#include "StandAlone/OptionParse.h"
#include "Options.h"

// LunarGLASS includes
#include "LunarGLASSManager.h"

// LunarGLASS adapter includes
#include "GlslangToTop.h"

// LLVM includes
#include "llvm/Module.h"

// Would be good to have a way of passing a target definition through the front end, or next to it...
int TargetDefinitionVersion;
EProfile TargetDefinitionProfile;

#ifndef USE_LUNARGLASS_CORE

    class AdapterOnlyManager : public gla::Manager {
    public:
        AdapterOnlyManager() { }
        virtual ~AdapterOnlyManager() { }

        virtual void clear() 
        {
            delete module;
            module = 0;
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
    TGenericCompiler(EShLanguage l, int dOptions) : TCompiler(l, infoSink), debugOptions(dOptions), targetVersion(0), targetProfile(EProfileCount) { }
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
    if (targetProfile == EProfileCount)
        targetProfile = profile;
    if (targetVersion == 0)
        targetVersion = version;

#ifdef USE_LUNARGLASS_CORE
    gla::Manager* glaManager = gla::getManager();
#else
    gla::Manager* glaManager = new AdapterOnlyManager();
#endif
    assert(EShLangCount < 256 && EProfileCount < 256);
    glaManager->setVersion(static_cast<int>(language) << 24 | static_cast<int>(targetProfile) << 16 | targetVersion);

    TranslateGlslangToTop(root, glaManager);

    if (! (debugOptions & EDebugOpMemoryLeakMode)) {
        if (debugOptions & EDebugOpAssembly)
            glaManager->dump("\nTop IR:\n");

#ifdef USE_LUNARGLASS_CORE
        glaManager->translateTopToBottom();
    
        if (debugOptions & EDebugOpAssembly)
            glaManager->dump("\n\nBottom IR:\n");

        glaManager->translateBottomToTarget();
#endif
    }

    glaManager->clear();

    delete glaManager;

    haveValidObjectCode = true;

    return haveValidObjectCode;
}
