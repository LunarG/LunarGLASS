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
#include "OptionParse.h"
#include <string>

#include <iostream>
#include <vector>
#include <cstdio>
#include <cstdlib>

// Description and usage information
const std::string Description = "\
Description: The LunarGLASS stand-alone shader compiler\n\
";

const std::string Usage = "\
Usage: ./StandAlone[.exe] [options] file1.frag ...\n\
\n\
       Options:\n\
         -h, --help                 Print out this Usage info\n\
         -d, --debug                Print out debugging info\n\
         --glsl                     Use the glsl backend (default)\n\
         --tgsi                     Use the TGSI backed\n\
         --disable <optimization>   Disable the optimization (see below)\n\
         --enable <optimization>    Enable the optimization (see below)\n\
\n\
       GLSL-backed options:\n\
         -f --obfuscate             Obfuscate the output\n\
         -n, --no-revision          Don't put the revision in the output\n\
\n\
       Optimizations/transformations that can be enabled/disabled. Default is \n\
       to enable them all\n\
         adce                       Aggressive dead code elimination\n\
         coalesce                   Coalesce Inserts into MultiInserts\n\
         gvn                        Global Value Numbering\n\
         mem2reg                    Promote memory to registers\n\
         reassociate                Reassociate/commute expressions\n\
         verify                     Verification passes\n\
         cross-stage                Do cross-stage optimizations\n\
";


namespace gla {

    Optimizations opts = { true   // adce
                         , true   // coalesce
                         , true   // gvn
                         , true   // mem2reg
                         , true   // reassociate
                         , true   // verify
                         , false  // crossStage
                         };

    // Global Options
    OptionsType Options = { false   // Debug info
                          , false   // Obfuscate
                          , false   // No revision
                          , GLSL    // Backend
                          , DefaultBackendVersion    // no backend version yet
                          , opts    // Optimizations
                          };

    // Is the string an option/flagged argument?
    bool IsOption(std::string s)
    {
        return !s.compare(0, 1, "-");
    }

    // Is the string setting an option that takes an argument?
    bool IsArgumentOption(std::string s)
    {
        return s == "--enable" || s == "--disable";
    }


    // Print out description and help
    void PrintHelp()
    {
        std::cout << Description << Usage;
    }

    // Given a string and a bool, assign the option
    void AssignOptimization(std::string opt, bool val)
    {
        if (opt == "adce") {
            Options.optimizations.adce        = val;
        } else if (opt == "coalesce") {
            Options.optimizations.coalesce    = val;
        } else if (opt == "gvn") {
            Options.optimizations.gvn         = val;
        } else if (opt == "mem2reg") {
            Options.optimizations.mem2reg     = val;
        } else if (opt == "reassociate") {
            Options.optimizations.reassociate = val;
        } else if (opt == "verify") {
            Options.optimizations.verify      = val;
        } else if (opt == "cross-stage") {
            Options.optimizations.crossStage  = val;
        } else {
            std::cout << "Unkown optimization" << opt << std::endl;
            PrintHelp();
            exit(1);
        }

        return;
    }

    // Returns the index of the first non-flag argument
    // Assumes that all option/flagged arguments come before non-flagged arguments
    int HandleArgs(int argc, char **argv)
    {
        using std::vector;
        using std::string;
        using std::iterator;

        int argIndex = 0;

        // Load up the flagged options
        vector<string> flaggedArgs;
        flaggedArgs.clear();
        for (int i = 1; i < argc; ++i) {
            if (IsOption((string) argv[i])) {
                flaggedArgs.push_back(argv[i]);
                if (IsArgumentOption((string) argv[i])) {
                    ++i;
                    flaggedArgs.push_back(argv[i]);
                }
            } else {
                argIndex = i;
                break;
            }
        }

        // Handle each option
        for (vector<string>::iterator i = flaggedArgs.begin(), e = flaggedArgs.end(); i != e; ++i) {
            if (*i == "-h" || *i == "--help") {
                PrintHelp();
                exit(0);
            } else if (*i == "-d" || *i == "--debug") {
                Options.debug = true;
            } else if (*i == "--glsl") {
                Options.backend = GLSL;
            } else if (*i == "--tgsi") {
                Options.backend = TGSI;
            } else if (*i == "-f" || *i == "--obfuscate") {
                Options.obfuscate = true;
            } else if (*i == "-n" || *i == "--no-revision") {
                Options.noRevision = true;
            } else if (*i == "--disable") {
                ++i;
                AssignOptimization(*i, false);
            } else if (*i == "--enable") {
                ++i;
                AssignOptimization(*i, true);
            } else {
                std::cout << "Unknown option: " << *i << std::endl;
                PrintHelp();
                exit(1);
            }
        }

        return argIndex;
    }

}
