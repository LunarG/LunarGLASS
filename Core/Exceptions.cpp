//===- Exceptions.cpp - Exceptions that LunarGLASS might throw ------------===//
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
// Author: John Kessenich, LunarG
//
//===----------------------------------------------------------------------===//

#define _CRT_SECURE_NO_WARNINGS
#ifdef _WIN32
#define snprintf _snprintf
#endif

#include "Exceptions.h"

#include <iostream>
#include <cstdlib>
#include <cstdio>

const char* UFString = "***Unsupported functionality: ";

// use anonymous namespace to scope these to this file
namespace {

    // the handler; 0 means default handling
    gla::UnsupportedFunctionalityHandler Handler = 0;

    // called always, dispatches to handler if one is registered
    void ProcessMessage(const std::string& message, gla::EAbortType at)
    {
        if (Handler) {
            // let the registered handler do what it wants the built up message
            (*Handler)(message, at);
        } else {
            // Print an exception-like string to stderr
            std::cerr << std::endl << message << std::endl;
            if (at == gla::EATAbort)
                exit(1);
        }
    }
}

void gla::RegisterUnsupportedFunctionalityHandler(gla::UnsupportedFunctionalityHandler h)
{
    Handler = h;
}

// format basic string
void gla::UnsupportedFunctionality(const char* pre, EAbortType at)
{
    std::string message(UFString);

    message.append(pre);

    ProcessMessage(message, at);
}

// format, converting n into a string and appending it
void gla::UnsupportedFunctionality(const char* pre, int n, EAbortType at)
{
    std::string message(UFString);

    message.append(pre);

    char buf[20];
    snprintf(buf, 20, "%d", n);

    message.append(buf);

    ProcessMessage(message, at);
}

// format, converting n into a string and appending it
void gla::UnsupportedFunctionality(const char* pre, int n, const char* end, EAbortType at)
{
    std::string message(UFString);

    message.append(pre);

    char buf[22];
    snprintf(buf, 20, " %d ", n);

    message.append(buf);
    message.append(end);

    ProcessMessage(message, at);
}
