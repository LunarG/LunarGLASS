//===- FunctionUtil.h - Utility functions for Functions -------------------===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (c) 2010-2013 LunarG, Inc.
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
// Provides utility functions for Functions
//
//===----------------------------------------------------------------------===//

#ifndef GLA_FUNTION_UTIL_H
#define GLA_FUNTION_UTIL_H

#include "llvm/Function.h"

namespace gla_llvm {
    using namespace llvm;

    // Whether the given function is the main one
    inline bool IsMain(Function& fun)
    {
        return fun.getName().equals("main");
    }

    // Get the block from the function with the passed name. Returns NULL if it
    // doesn't exist.
    inline BasicBlock* GetNamedBlock(Function& fun, llvm::StringRef name)
    {
        for (Function::iterator bb = fun.begin(), e = fun.end(); bb != e; ++bb)
            if (bb->getName().equals(name))
                return bb;

        return NULL;
    }

    // Returns the stage-exit block if the function is main and it exists, NULL
    // otherwise.
    inline BasicBlock* GetMainExit(Function& fun)
    {
        if (! IsMain(fun))
            return NULL;

        return GetNamedBlock(fun, "stage-exit");
    }

    // Returns the stage-epilogue block if the function is main and it exists,
    // NULL otherwise.
    inline BasicBlock* GetMainEpilogue(Function& fun)
    {
        if (! IsMain(fun))
            return NULL;

        return GetNamedBlock(fun, "stage-epilogue");
    }

    // Returns the (first) returning block in the Function. NULL if the function
    // does not return.
    inline BasicBlock* GetReturnBlock(Function& fun)
    {
        for (Function::iterator bb = fun.begin(), e = fun.end(); bb != e; ++bb) {
            if (isa<ReturnInst>(bb->getTerminator()))
                return bb;
        }

        return NULL;
    }

    // Returns the number of returning blocks in the function.
    inline int CountReturnBlocks(Function& fun)
    {
        int count = 0;
        for (Function::iterator bb = fun.begin(), e = fun.end(); bb != e; ++bb) {
            if (isa<ReturnInst>(bb->getTerminator()))
                ++count;
        }

        return count;
    }

} // end namespace gla_llvm


#endif // GLA_FUNTION_UTIL_H

