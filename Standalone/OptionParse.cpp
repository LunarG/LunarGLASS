//===- OptionsParse.cpp - Global run-time options -------------------------===//
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

#include "options.h"
#include "OptionParse.h"

#include <iostream>
#include <vector>
#include <cstdio>
#include <cstdlib>

const std::string Usage =
//"    back-end options:\n"
//"\n"
//"         --glsl                     Use the glsl backend (default)\n"
//"         --tgsi                     Use the TGSI backed\n"
//"         --dummy                    Use the Dummy backed\n"
//"\n"
"Optimization controls (which must appear first, before any other options or arguments):\n"
"  --disable <optimization>   Disable the optimization (see below)\n"
"  --enable  <optimization>   Enable the optimization (see below)\n"
"  Where <optimization> is one of\n"
"    adce            Aggressive dead-code elimination\n"
"    coalesce        Coalesce Inserts into MultiInserts\n"
"    gvn             Global Value Numbering\n"
"    reassociate     Reassociate/commute expressions\n"
"    cross-stage     Do cross-stage optimizations\n"
"    inline          Inline all function calls\n"
"    unroll          Unroll loops\n"
"    hoist           Hoist instructions when flattening conditionals\n"
"  Currently, all are enabled by default.\n"
;

namespace gla {

    // Is the string a transform option?  Only handling "--" options here, the others are handled by main().
    bool IsTransformOption(std::string s)
    {
        return s.compare(0, 2, "--") == 0;
    }

    // Is the string setting an option that takes an argument?
    bool IsArgumentOption(std::string s)
    {
        return s == "--enable" || s == "--disable";
    }


    // Print out description and help
    void PrintTransformOptionsHelp()
    {
        std::cout << Usage;
    }

     // Given a string and a bool, assign the option
    void AssignOptimization(std::string opt, bool val, TransformOptions& options)
    {
        if (opt == "adce") {
            options.optimizations.adce        = val;
        } else if (opt == "coalesce") {
            options.optimizations.coalesce    = val;
        } else if (opt == "gvn") {
            options.optimizations.gvn         = val;
        } else if (opt == "reassociate") {
            options.optimizations.reassociate = val;
        } else if (opt == "cross-stage") {
            options.optimizations.crossStage  = val;
        } else if (opt == "inline") {
            options.optimizations.inlineThreshold       = val ? options.optimizations.inlineThreshold       : 0;
        } else if (opt == "unroll") {
            options.optimizations.loopUnrollThreshold   = val ? options.optimizations.loopUnrollThreshold   : 0;
        } else if (opt == "hoist") {
            options.optimizations.flattenHoistThreshold = val ? options.optimizations.flattenHoistThreshold : 0;
        } else {
            std::cout << "Unkown optimization: " << opt << std::endl;
            PrintTransformOptionsHelp();
            exit(1);
        }

        return;
    }

    // Returns the index of the first non-flag argument
    // Returns -1 for an error.
    // Assumes that all option/flagged arguments come before non-flagged arguments
    int HandleTransformOptions(int argc, char **argv, TransformOptions& options)
    {
        using std::vector;
        using std::string;
        using std::iterator;

        int argIndex = 1;

        // Load up the flagged options
        vector<string> flaggedArgs;
        flaggedArgs.clear();
        for (int i = 1; i < argc; ++i) {
            if (IsTransformOption((string) argv[i])) {
                flaggedArgs.push_back(argv[i]);
                if (IsArgumentOption((string) argv[i])) {
                    ++i;
                    if (i < argc)
                        flaggedArgs.push_back(argv[i]);
                    else
                        return -1;
                }
            } else {
                argIndex = i;
                break;
            }
        }

        // Handle each option
        for (vector<string>::iterator i = flaggedArgs.begin(), e = flaggedArgs.end(); i != e; ++i) {
            if (*i == "--help") {
                return -1;
            //} else if (*i == "--glsl") {
            //    options.backend = GLSL;
            //} else if (*i == "--tgsi") {
            //    options.backend = TGSI;
            //} else if (*i == "--dummy") {
            //    options.backend = Dummy;
            } else if (*i == "--enable") {
                ++i;
                AssignOptimization(*i, true, options);
            } else if (*i == "--disable") {
                ++i;
                AssignOptimization(*i, false, options);
            } else if ((*i).compare(0, 2, "--") == 0) {
                return -1;
            }
        }

        return argIndex;
    }

} // end namespace gla

