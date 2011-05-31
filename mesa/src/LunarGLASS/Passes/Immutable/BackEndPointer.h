//===- BackEndPointer.h - Expose backend queries  -------------------------===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (C) 2010-2011 LunarG, Inc.
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; version 2 of the
// License.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301, USA.
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

#include "llvm/Support/ErrorHandling.h"

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
        { }

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

