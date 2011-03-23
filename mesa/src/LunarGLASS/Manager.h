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

    class BackEndTranslator {
    public:
        BackEndTranslator() { }
        virtual ~BackEndTranslator() { }
        virtual void addGlobal(const llvm::GlobalVariable*) { }
        virtual void startFunction() = 0;
        virtual void endFunction() = 0;
        virtual void add(const llvm::Instruction*, bool lastBlock) = 0;

        //
        // The following set of functions is motivated by need to convert to
        // structured flow control and eliminate phi functions.
        //

        // This must get called soon enough that it is before the split that
        // makes the phi.  It is then referred to in addPhiCopy().
        virtual void declarePhiCopy(const llvm::Value* dst) { }

        // Called for Phi elimination.
        virtual void addPhiCopy(const llvm::Value* dst, const llvm::Value* src) = 0;

        // Called to build structured flow control.
        virtual void addIf(const llvm::Value* cond) = 0;
        virtual void addElse() = 0;
        virtual void addEndif() = 0;

        virtual void print() = 0;
    };

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
