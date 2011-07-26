//===- Exceptions.cpp - Exceptions that LunarGLASS might throw ------------===//
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
// Author: John Kessenich, LunarG
//
//===----------------------------------------------------------------------===//

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
    void ProcessMessage(std::string message, gla::EAbortType at)
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

    char buf[20];
    snprintf(buf, 20, "%d", n);

    message.append(buf);
    message.append(end);

    ProcessMessage(message, at);
}
