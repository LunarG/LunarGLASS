//===- BackEndPointer.h - Expose backend queries  -------------------------===//
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
//===----------------------------------------------------------------------===//
//
// Expose backend queries to passes through the BackEndPointer class, which is
// an immutable pass and behaves as a pointer to a BackEnd object.
//
//===----------------------------------------------------------------------===//

#ifndef GLA_BACKENDPASS_H
#define GLA_BACKENDPASS_H

#include "Passes/PassSupport.h"
#include "Backend.h"

#pragma warning(push, 1)
#include "llvm/Support/ErrorHandling.h"
#pragma warning(pop)

namespace gla_llvm {
    using namespace llvm;
    using namespace gla;

    // BackEndPointer acts like a pointer to a BackEnd object
    class BackEndPointer : public ImmutablePass  {
    public:
        typedef BackEnd& reference;
        typedef BackEnd* pointer;

        // Default ctor. This has to exist, because this is a pass, but it
        // should never be used.
        BackEndPointer() : ImmutablePass(ID)
        {
            report_fatal_error("Bad BackEndPointer ctor used.");
        }

        explicit BackEndPointer(BackEnd* be) : ImmutablePass(ID)
                                             , backEnd(be)
        {
            initializeBackEndPointerPass(*PassRegistry::getPassRegistry());
        }

        // Provide the interface to make this be just like a pointer
        operator pointer() const
        {
            return backEnd;
        }

        reference operator*() const
        {
            return *backEnd;
        }

        pointer operator->() const
        {
            return &operator*();
        }

        static char ID;

    protected:
        BackEnd* backEnd;

    private:
        BackEndPointer(const BackEndPointer&); // do not implement
        void operator=(const BackEndPointer&); // do not implement
    };

} // end namespace gla_llvm

#endif // GLA_BACKENDPASS_H

