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

// Function prototypes, so that main.cpp can include this file safely
namespace gla {
    bool IsOption(std::string);
    void PrintHelp();
    int HandleArgs(int, char**);
}

#ifndef MAIN_CPP

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
         -h, --help      Print out this Usage info\n\
         -d, --debug     Print out debugging info\n\
         --glsl          Use the glsl backend (default)\n\
         --tgsi          Use the TGSI backed\n\
\n\
       GLSL-backed options:\n\
         -f --obfuscate  Obfuscate the output\n\
";


namespace gla {

    // Global Options
    OptionsType Options = { false   // Debug info
                          , false   // Obfuscate
                          , GLSL    // Backend
                          };

    // Is the string an option/flagged argument?
    bool IsOption(std::string s) { return !s.compare(0, 1, "-"); }


    // Print out description and help
    void PrintHelp() { std::cout << Description << Usage; }

    // Returns the index of the first non-flag argument
    // Assumes that all option/flagged arguments come before non-flagged arguments
    int HandleArgs(int argc, char **argv) {
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
            } else {
                argIndex = i;
                break;
            }
        }

        // Handle each option
        for (vector<string>::iterator i = flaggedArgs.begin(), e = flaggedArgs.end(); i != e; ++i){
            if (*i == "-h" || *i == "--help") {
                PrintHelp();
                exit(0);
            } else if (*i == "-d" || *i == "--debug") {
                Options.debug = true;
            } else if (*i == "--glsl") {
                Options.backend = GLSL;
            } else if (*i == "--tgsi") {
                Options.backend = TGSI;
            } else {
                std::cout << "Unknown option: " << *i << std::endl;
                PrintHelp();
                exit(0);
            }
        }

        return argIndex;
    }

}

#endif
