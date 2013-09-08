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
#include <algorithm>

namespace gla_llvm {
    using namespace llvm;

    // Whether the given predicate is true for any element
    template <class InputIterator, class Predicate>
    inline bool Any(InputIterator first, InputIterator last, Predicate pred)
    {
        return std::find_if(first, last, pred) != last;
    }

    template <class Elt, class Predicate>
    inline bool Any(ArrayRef<Elt> arr, Predicate pred)
    {
        return Any(arr.begin(), arr.end(), pred);
    }

    template <class Elt, class Predicate>
    inline bool Any(const iplist<Elt>& list, Predicate pred)
    {
        return Any(list.begin(), list.end(), pred);
    }


    // Whether the given data structure contains the given element
    template<class InputIterator, typename Elt>
    inline bool Has(InputIterator begin, InputIterator end, const Elt& val)
    {
        return end != std::find(begin, end, val);
    }

    template<typename Elt>
    inline bool Has(ArrayRef<Elt> arr, const Elt& val)
    {
        return Has(arr.begin(), arr.end(), val);
    }

    template<typename Elt>
    inline bool Has(const SmallVectorImpl<Elt>& vec, const Elt& val)
    {
        return Has(vec, val);
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

    // Set-wise subtraction: stores A - B into A.
    // Specialization for SmallPtrSet.
    // linear in the size of B
    template <typename T, unsigned S, typename BTy>
    inline void SetDifference(SmallPtrSet<T,S>& a, BTy& b)
    {
        for (typename BTy::iterator i = b.begin(), e = b.end(); i != e; ++i)
            a.erase(*i);
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

} // end namespace gla_llvm

#endif // GLA_ADT_H
