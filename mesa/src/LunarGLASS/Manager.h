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
// Private to LunarGLASS implementation, see LunarGLASSTopIR.h for public 
// interface.
//
//===----------------------------------------------------------------------===//

#pragma once
#ifndef GlaManager_H
#define GlaManager_H

// LLVM includes
#include "llvm/Module.h"

// LunarGLASS includes
#include "LunarGLASSTopIR.h"

namespace gla {

    class Target {
    public:
        Target() { }
        virtual ~Target() { }
        virtual void add(const llvm::Instruction* llvmInstruction) = 0;
        virtual void addIf(const llvm::Value* cond) = 0;
        virtual void addElse() = 0;
        virtual void addEndif() = 0;
        virtual void addCopy(const llvm::Value* dst, const llvm::Value* src) = 0;

        virtual void print() = 0;
    };

    class PrivateManager : public gla::Manager {
    public:
        PrivateManager();
        virtual ~PrivateManager();

        virtual void setModule(llvm::Module* m) { module = m; }
        virtual void translateTopToBottom();
        virtual void translateBottomToTarget();
        virtual void setTarget(gla::Target* t) { target = t; }

    protected:
        virtual void runLLVMOptimizations1();
        llvm::Module* module;
        gla::Target* target;
    };

}

#endif /* GlaManager_H */
