//===- ADT.h - Utility functions and wrappers for ADTs --------------------===//
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
// Provides utility functions and wrappers for data structures found in
// llvm/ADT
//
//===----------------------------------------------------------------------===//

#ifndef ADT_H
#define ADT_H


namespace gla_llvm {
    using namespace llvm;

    // Whether a SmallVector contains the given element
    template<typename T>
    inline bool SmallVectorContains(SmallVectorImpl<T>& vec, T val)
    {
        // We need to typedef it (with a typename) to access its iterator
        for (typename SmallVectorImpl<T>::iterator i = vec.begin(), e = vec.end(); i != e; ++i) {
            if (&**i == &*val)
                return true;
        }

        return false;
    }

} // end namespace gla_llvm

#endif // ADT_H

