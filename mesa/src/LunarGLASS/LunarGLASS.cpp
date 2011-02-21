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
#include "LunarGLASSBottomIR.h"

#include "llvm/Module.h"

#include "TgsiTarget.h"

gla::Manager* gla::getManager()
{
    return new gla::PrivateManager();
}

gla::PrivateManager::PrivateManager() : module(0)
{
    backEndTranslator = gla::GetTgsiTarget();
}

gla::PrivateManager::~PrivateManager()
{
    delete module;
    gla::ReleaseTgsiTarget(backEndTranslator);
}

void gla::BackEnd::getRegisterForm(int& outerSoA, int& innerAoS)
{
    outerSoA = 1; 
    innerAoS = 4; 
}
