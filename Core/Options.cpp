//===- Options.cpp - Global run-time options ------------------------------===//
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

#include "Options.h"
#include <string>

namespace gla {
    // Optimizations opts = { true   // adce
    //                      , true   // coalesce
    //                      , true   // gvn
    //                      , true   // mem2reg
    //                      , true   // reassociate
    //                      , true   // verify
    //                      , false  // crossStage
    //                      };

    // Global Options
    OptionsType Options = { false   // Debug info
                          , false   // Iterate
                          , false   // Obfuscate
                          , false   // No revision
                          , GLSL    // Backend
                          , DefaultBackendVersion    // no backend version yet
                          , false   // Bottom IR Only
                          // , opts    // Optimizations
                          };
} // end namespace gla

