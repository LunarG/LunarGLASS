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
#include "StandAlone/OptionParse.h"
#include "Options.h"

// LunarGLASS includes
#include "LunarGLASSManager.h"
#include "Frontends/Glsl2/GlslToTop.h"

// LunarGLASS adapter includes
#include "GlslangToTop.h"

//
// Here is where real machine specific high-level data would be defined.
//
class TGenericCompiler : public TCompiler {
public:
    TGenericCompiler(EShLanguage l, int dOptions) : TCompiler(l, infoSink), debugOptions(dOptions) { }
    virtual bool compile(TIntermNode* root);
    TInfoSink infoSink;
    int debugOptions;
};

//
// This function must be provided to create the actual
// compile object used by higher level code.  It returns
// a subclass of TCompiler.
//
TCompiler* ConstructCompiler(EShLanguage language, int debugOptions)
{
    return new TGenericCompiler(language, debugOptions);
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
bool TGenericCompiler::compile(TIntermNode *root)
{
    haveValidObjectCode = false;

    //if (gla::Options.debug && ! gla::Options.bottomIROnly)
    //    _mesa_print_ir(Shader->ir, 0);

    gla::Manager* glaManager = gla::getManager();
    int compileCount = gla::Options.iterate ? 1000 : 1;
    for (int i = 0; i < compileCount; ++i) {
        TranslateGlslangToTop(root, glaManager);

        glaManager->translateTopToBottom();

        glaManager->translateBottomToTarget();

        glaManager->clear();
    }

    delete glaManager;

    haveValidObjectCode = false;

    return haveValidObjectCode;
}
