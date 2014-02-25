//===- LunarGLASSManager.h - Public interface to using LunarGLASS ----========//
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
// Public interface to LunarGLASS.
//
// Don't include Mesa or other user's headers here.  LunarGLASS is not
// Mesa-dependent nor dependent on other uses of it.
//
// Don't include LLVM headers here.  At the highest level use of LunarGLASS,
// LLVM headers are neither needed nor desired.
//
//===----------------------------------------------------------------------===//

#pragma once
#ifndef LunarGLASSManager_H
#define LunarGLASSManager_H

#include <vector>
#include <string>

#define USE_LUNARGLASS_CORE

namespace llvm {
    class Module;
    class Value;
    class Type;
};

namespace gla {

    // Abstract class of external manager of translations within LunarGLASS.
    // Use getManager() to get a concrete manager, which should be derived
    // from gla::PrivateManager.
    class Manager {
    public:
        virtual ~Manager() { }    // the concrete class is expected to delete everything
        virtual void clear() = 0; // implement per-compile clear, so a manager object can be re-used

        virtual void setModule(llvm::Module* m) { module = m; }
        virtual llvm::Module* getModule() { return module; }

        virtual void setVersion(int v) { version = v; }
        virtual int getVersion() const { return version; }

        virtual void translateTopToBottom() = 0;
        virtual void translateBottomToTarget() = 0;
        virtual void dump(const char* heading) { }

    protected:
        Manager() : module(0), version(0) { }

        llvm::Module* module;

        int version;    // generic versioning, details defined by front end
    };

    Manager* getManager();
};

#endif /* LunarGLASSManager_H */
