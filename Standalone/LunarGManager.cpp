//===- LunarGManager.cpp - LunarG's customization of PrivateManager.h -----===//
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
// LunarG's customization of gla::PrivateManager, to manage the back ends
// LunarG is working on.
//
//===----------------------------------------------------------------------===//

#include "PrivateManager.h"
#include "Exceptions.h"

#pragma warning(push, 1)
#include "llvm/IR/Module.h"
#pragma warning(pop)

#include "Backends/TGSI/TgsiTarget.h"
#include "Backends/GLSL/GlslTarget.h"
#include "Backends/Dummy/DummyTarget.h"

#include "Options.h"

namespace gla {

    class LunarGManager : public gla::PrivateManager {
    public:
        LunarGManager();
        virtual ~LunarGManager();
        virtual void clear();
    };
}

gla::Manager* gla::getManager()
{
    return new gla::LunarGManager();
}

gla::LunarGManager::LunarGManager()
{
    switch (Options.backend) {
    case TGSI:
        backEndTranslator = gla::GetTgsiTranslator(this);
        backEnd = gla::GetTgsiBackEnd();
        break;
    case GLSL:
        backEndTranslator = gla::GetGlslTranslator(this, Options.obfuscate);
        backEnd = gla::GetGlslBackEnd();
        break;
    case Dummy:
        backEndTranslator = gla::GetDummyTranslator(this);
        backEnd = gla::GetDummyBackEnd();
        break;
    default:
        UnsupportedFunctionality("Backend not supported");
        break;
    }
}

void gla::LunarGManager::clear()
{
    delete module;
    module = 0;

    //
    // Note this is likely not the best design example for a driver backend,
    // but that is between the private manager and the backend it is managing,
    // so this code is sufficient for LunarG's current standalone backends.
    // (The dummy backend does not use memory.)
    //

    switch (Options.backend) {
    case TGSI:
        gla::ReleaseTgsiTranslator(backEndTranslator);
        backEndTranslator = gla::GetTgsiTranslator(this);
        break;
    case GLSL:
        gla::ReleaseGlslTranslator(backEndTranslator);
        backEndTranslator = gla::GetGlslTranslator(this, Options.obfuscate);
        break;
    default:
        break;
    }
}

gla::LunarGManager::~LunarGManager()
{
    clear();

    switch (Options.backend) {
    case TGSI:
        gla::ReleaseTgsiTranslator(backEndTranslator);
        gla::ReleaseTgsiBackEnd(backEnd);
        break;
    case GLSL:
        gla::ReleaseGlslTranslator(backEndTranslator);
        gla::ReleaseGlslBackEnd(backEnd);
        break;
    case Dummy:
        gla::ReleaseDummyTranslator(backEndTranslator);
        gla::ReleaseDummyBackEnd(backEnd);
        break;
    default:
        UnsupportedFunctionality("Backend not supported");
        break;
    }
}
