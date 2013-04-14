//===- PrivateManager.h - internal implementation of public manager -------===//
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
// Private to LunarGLASS implementation, see LunarGLASSManager.h for public
// interface.  Inherit from gla::PrivateManager to customize your own manager.
//
//===----------------------------------------------------------------------===//

#pragma once
#ifndef PrivateManager_H
#define PrivateManager_H

// LunarGLASS includes
#include "LunarGLASSManager.h"
#include "Backend.h"

namespace gla {

    class BackEnd;

    class PrivateManager : public gla::Manager {
    public:
        virtual ~PrivateManager() { }
        virtual void clear() = 0;

        virtual void translateTopToBottom();
        virtual void translateBottomToTarget();
        virtual void dump(const char* heading);

    protected:
        PrivateManager() { }

        virtual void runLLVMOptimizations1();
        gla::BackEndTranslator* backEndTranslator;
        gla::BackEnd* backEnd;
    };
}

#endif /* PrivateManager_H */
