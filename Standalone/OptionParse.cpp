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

#include "OptionParse.h"
#include "Options.h"

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
         -b, --bottom-ir-only       Only print out Bottom IR\n\
         --glsl                     Use the glsl backend (default)\n\
         --tgsi                     Use the TGSI backed\n\
         --dummy                    Use the Dummy backed\n\
\n\
       GLSL-backed options:\n\
         -i --iterate               Iterate LunarGLASS 1000 times\n\
         -f --obfuscate             Obfuscate the output\n\
         -n, --no-revision          Don't put the revision in the output\n\
\n\
";

/*
          --disable <optimization>   Disable the optimization (see below)\n\
          --enable <optimization>    Enable the optimization (see below)\n\
*/


/*
        Optimizations/transformations that can be enabled/disabled. Default is \n\
        to enable them all\n\
          adce                       Aggressive dead code elimination\n\
          coalesce                   Coalesce Inserts into MultiInserts\n\
          gvn                        Global Value Numbering\n\
          mem2reg                    Promote memory to registers\n\
          reassociate                Reassociate/commute expressions\n\
          verify                     Verification passes\n\
          cross-stage                Do cross-stage optimizations\n\
*/

namespace gla {

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

    // // Given a string and a bool, assign the option
    // void AssignOptimization(std::string opt, bool val)
    // {
    //     if (opt == "adce") {
    //         Options.optimizations.adce        = val;
    //     } else if (opt == "coalesce") {
    //         Options.optimizations.coalesce    = val;
    //     } else if (opt == "gvn") {
    //         Options.optimizations.gvn         = val;
    //     } else if (opt == "mem2reg") {
    //         Options.optimizations.mem2reg     = val;
    //     } else if (opt == "reassociate") {
    //         Options.optimizations.reassociate = val;
    //     } else if (opt == "verify") {
    //         Options.optimizations.verify      = val;
    //     } else if (opt == "cross-stage") {
    //         Options.optimizations.crossStage  = val;
    //     } else {
    //         std::cout << "Unkown optimization" << opt << std::endl;
    //         PrintHelp();
    //         exit(1);
    //     }

    //     return;
    // }

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
            } else if (*i == "--dummy") {
                Options.backend = Dummy;
            } else if (*i == "-i" || *i == "--iterate") {
                Options.iterate = true;
            } else if (*i == "-f" || *i == "--obfuscate") {
                Options.obfuscate = true;
            } else if (*i == "-n" || *i == "--no-revision") {
                Options.noRevision = true;
            } else if (*i == "-b" || *i == "--bottom-ir-only") {
                Options.bottomIROnly = true;
            // } else if (*i == "--disable") {
            //     ++i;
            //     AssignOptimization(*i, false);
            // } else if (*i == "--enable") {
            //     ++i;
            //     AssignOptimization(*i, true);
            } else {
                std::cout << "Unknown option: " << *i << std::endl;
                PrintHelp();
                exit(1);
            }
        }

        return argIndex;
    }

} // end namespace gla

