//===- Manager.h - private implementation of public manager ---------------===//
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
// interface.
//
//===----------------------------------------------------------------------===//

#pragma once
#ifndef GlaManager_H
#define GlaManager_H

// LunarGLASS includes
#include "LunarGLASSManager.h"
#include "LunarGLASSBackend.h"

namespace gla {

    class BackEnd;

    class PrivateManager : public gla::Manager {
    public:
        PrivateManager();
        virtual ~PrivateManager();

        virtual void setModule(llvm::Module* m) { module = m; }
        virtual void translateTopToBottom();
        virtual void translateBottomToTarget();
        virtual void setTarget(gla::BackEndTranslator* t) { backEndTranslator = t; }

    protected:
        virtual void runLLVMOptimizations1();
        llvm::Module* module;
        gla::BackEndTranslator* backEndTranslator;
        gla::BackEnd* backEnd;
    };
}


#endif /* GlaManager_H */
