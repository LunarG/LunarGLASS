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

#ifndef GLA_ADT_H
#define GLA_ADT_H

#include "llvm/ADT/ilist.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SmallPtrSet.h"

#include <vector>

namespace gla_llvm {
    using namespace llvm;

    // Whether a SmallVector contains the given element
    template<typename T>
    inline bool SmallVectorContains(const SmallVectorImpl<T>& vec, const T val)
    {
        // We need to typedef it (with a typename) to access its iterator
        for (typename SmallVectorImpl<T>::const_iterator i = vec.begin(), e = vec.end(); i != e; ++i) {
            if (&**i == &*val)
                return true;
        }

        return false;
    }

    // Whether one collection is a subset of another. Currently defined for
    // SmallPtrSet vs SmallPtrSet and iplist vs SmallPtrSet.
    // O(n*query(super)) where n is the size of sub.
    template<typename ATy, typename BTy>
    inline bool IsSubset(const ATy& sub, const BTy& super)
    {
        for (typename ATy::iterator i = sub.begin(), e = sub.end(); i != e; ++i)
            if (! super.count(*i))
                return false;

        return true;
    }

    // iplist const version
    template<typename T, unsigned S2>
    inline bool IsSubset(const iplist<T>& sub, const SmallPtrSet<const T*,S2>& super)
    {
        for (typename iplist<T>::const_iterator i = sub.begin(), e = sub.end(); i != e; ++i)
            if (! super.count(i))
                return false;

        return true;
    }


    // Set-wise intersection: stores A n B into A.
    // TODO: specialization for sets (e.g. std::set) that prefer erasing iterators.
    // O(n*query(B))
    template <typename T, unsigned S, typename BTy>
    inline void SetIntersect(SmallPtrSet<T,S>& a, BTy& b)
    {
        // Simple case: b is empty.
        if (b.empty())
            a.clear();

        // work list (erasing from a SmallPtrSet may invalidate the iterator)
        SmallVector<T,S> workList;

        // Iterate through A, removing elements not contained in B
        int times = 0;
        for (typename SmallPtrSet<T,S>::iterator i = a.begin(), e = a.end(); i != e; ++i) {
            ++times;
            if (! b.count(*i))
                workList.push_back(*i);
        }

        // Remove them all
        SetDifference(a,workList);
    }

    // Set-wise subtraction: stores A - B into A.
    // Specialization for SmallPtrSet.
    // linear in the size of B
    template <typename T, unsigned S, typename BTy>
    inline void SetDifference(SmallPtrSet<T,S>& a, BTy& b)
    {
        for (typename BTy::iterator i = b.begin(), e = b.end(); i != e; ++i)
            a.erase(*i);
    }

} // end namespace gla_llvm

#endif // GLA_ADT_H

