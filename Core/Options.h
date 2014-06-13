//===- Options.h - Help Translate GLSL IR to LunarGLASS Top IR -===//
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
// Author: Michael Ilseman, LunarG
//
// Global run-time options
//
//===----------------------------------------------------------------------===//

#pragma once
#ifndef OPTIONS_H
#define OPTIONS_H

namespace gla {
    // Command-line options
    enum CommandOptions {
	    EOptionNone               = 0x0000,
	    EOptionIntermediate       = 0x0001,
	    EOptionSilent             = 0x0002,
	    EOptionMemoryLeakMode     = 0x0004,
        EOptionRelaxedErrors      = 0x0008,
        EOptionGiveWarnings       = 0x0010,
        EOptionsLinkProgram       = 0x0020,
        EOptionMultiThreaded      = 0x0040,
        EOptionDumpConfig         = 0x0080,
        EOptionDumpReflection     = 0x0100,
        EOptionSuppressWarnings   = 0x0200,
        EOptionDumpVersions       = 0x0400,
        EOptionAssembly           = 0x0800,
        EOptionObfuscate          = 0x1000,
        EOptionFilterInactive     = 0x2000,
    };

    enum BackendOption { GLSL      // GLSL backed
                       , TGSI      // TGSI backend
                       , Dummy     // Dummy backend
                       };

    const int DefaultBackendVersion = -1;

    // Optimizations struct
    struct Optimizations {
        bool adce;
        bool coalesce;
        bool gvn;
        bool mem2reg;
        bool reassociate;
        bool verify;
        bool crossStage;
    };

    // Options struct
    struct OptionsType {
        bool debug;
        bool iterate;
        bool obfuscate;
        bool noRevision;
        BackendOption backend;
        int backendVersion;    // what version output should the backend generate?
        bool bottomIROnly;
        Optimizations optimizations;
    };

    extern OptionsType Options;
}

#endif // OPTIONS_H
