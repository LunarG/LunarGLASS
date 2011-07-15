//===- Exceptions.h - Exceptions that LunarGLASS might throw --------------===//
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
//===----------------------------------------------------------------------===//

#include <string>

namespace gla {

    enum EAbortType { EATAbort, EATContinue };

    // Print an exception-like string to stderr, then exit
    void UnsupportedFunctionality(const char* pre, EAbortType at=EATAbort);

    // Print an exception-like string to stderr, converting num into a
    // string and appending it.
    void UnsupportedFunctionality(const char* pre, int n, EAbortType at=EATAbort);


    // Print an exception-like string to stderr, converting num into a
    // string and appending it, along with end.
    void UnsupportedFunctionality(const char* pre, int n, const char* end, EAbortType at=EATAbort);

    typedef void (*UnsupportedFunctionalityHandler)(std::string&, EAbortType);

    void RegisterUnsupportedFunctionalityHandler(UnsupportedFunctionalityHandler);

    /*  Examples of usage:

        gla::UnsupportedFunctionality("arrays, varType=", varType);
     => ***Unsupported functionality: arrays, varType=6

        gla::UnsupportedFunctionality("arrays <varType=", varType, ">");
     => ***Unsupported functionality: arrays <varType=6>

     // gla::UnsupportedFunctionality("", varType, ": arrays");
        ***Unsupported functionality: 6: arrays

    */

} // End gla namespace
