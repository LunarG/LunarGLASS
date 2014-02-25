//===- LunarGManager.cpp - LunarG's customization of PrivateManager.h -----===//
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
// LunarG's customization of gla::PrivateManager, to manage the back ends
// LunarG is working on.
//
//===----------------------------------------------------------------------===//

#include "PrivateManager.h"
#include "Exceptions.h"

#include "llvm/IR/Module.h"

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
        backEndTranslator = gla::GetGlslTranslator(this);
        backEnd = gla::GetGlslBackEnd();
        break;
    case Dummy:
        backEndTranslator = gla::GetDummyTranslator(this);
        backEnd = gla::GetDummyBackEnd();
        break;
    default:
        UnsupportedFunctionality("Backend not supported");
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
        backEndTranslator = gla::GetGlslTranslator(this);
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
    }
}
