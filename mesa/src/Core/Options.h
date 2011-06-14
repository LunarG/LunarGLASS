//===- Options.h - Help Translate GLSL IR to LunarGLASS Top IR -===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright © 2011, LunarG, Inc.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice (including the next
// paragraph) shall be included in all copies or substantial portions of the
// Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
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
