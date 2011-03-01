//===- LunarGLASS.cpp - Implementation of LunarGLASSBottomIR.h ------------===//
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
//===----------------------------------------------------------------------===//

#include "Manager.h"
#include "LunarGLASSBackend.h"

#include "llvm/Module.h"

#include "TgsiTarget.h"
#include "GlslTarget.h"

#include "Options.h"


gla::Manager* gla::getManager()
{
    return new gla::PrivateManager();
}

gla::PrivateManager::PrivateManager() : module(0)
{
    switch (Options.backend) {
    case TGSI:
        backEndTranslator = gla::GetTgsiTranslator();
        backEnd = gla::GetTgsiBackEnd();
        break;
    case GLSL:
        backEndTranslator = gla::GetGlslTranslator();
        backEnd = gla::GetGlslBackEnd();
        break;
    default:
        assert(!"Internal error: unknown backend");
    }
}

gla::PrivateManager::~PrivateManager()
{
    delete module;
    switch (Options.backend) {
    case TGSI:
        gla::ReleaseTgsiTranslator(backEndTranslator);
        gla::ReleaseTgsiBackEnd(backEnd);
        break;
    case GLSL:
        gla::ReleaseGlslTranslator(backEndTranslator);
        gla::ReleaseGlslBackEnd(backEnd);
        break;
    default:
        assert(!"Internal error: unknown backend");
    }

}

void gla::BackEnd::getRegisterForm(int& outerSoA, int& innerAoS)
{
    outerSoA = 1;
    innerAoS = 4;
}
