//===- BasicBlockUtil.cpp - Utility functions for basic blocks ------------===//
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
// Provides utility functions for BasicBlocks
//
//===----------------------------------------------------------------------===//

#include "Passes/Util/BasicBlockUtil.h"

#include <algorithm>
#include <vector>
#include <set>

namespace gla_llvm {
    using namespace llvm;


// Return the single merge point of the given basic blocks.  Returns null if
// there is no merge point, or if there are more than 1 merge points.  If the
// vector contains only a single element, returns that element.  If the vector
// is empty, returns NULL.  Note that the presense of backedges or exitedges may
// cause there to be multiple potential merge points.
BasicBlock* GetSingleMergePoint(SmallVectorImpl<BasicBlock*>& bbVec, DominanceFrontier& domFront)
{
    if (bbVec.size() == 0)
        return NULL;

    if (bbVec.size() == 1) {
        return bbVec[0];
    }

    BasicBlock* prev = bbVec[0];
    DominanceFrontier::DomSetType prevSet = (*domFront.find(prev)).second;

    BasicBlock* candidate = NULL;

    for (SmallVectorImpl<BasicBlock*>::iterator bbI = bbVec.begin(), bbE = bbVec.end(); /*empty*/; /*empty*/) {
        ++bbI;
        if (bbI == bbE)
            break;

        BasicBlock* next = const_cast<BasicBlock*>(*bbI); // Necessary, but safe
        DominanceFrontier::DomSetType nextSet = (*domFront.find(next)).second;

        // First, if we have a candidate already, check for it
        if (candidate) {
            if (nextSet.count(candidate))
                continue;

            return NULL;
        }

        // If next is in the dominance frontier of prev, then next is the
        // candidate, and vice versa. If they're both in each others sets, we
        // have cross-edges, so no merge point.
        if (nextSet.count(prev) && prevSet.count(next))
            return NULL;

        if (nextSet.count(prev)) {
            candidate = prev;
            continue;
        } else if (prevSet.count(next)) {
            candidate = next;
            continue;
        }

        // Otherwise take the intersection. If the intersection is empty, then
        // there is no merge point. If it contains a single element, then that's
        // the candidate. If none of the above hold, then it becomes the prevSet.
        std::vector<BasicBlock*> merges(prevSet.size() + nextSet.size());

        std::vector<BasicBlock*>::iterator end = std::set_intersection(prevSet.begin(), prevSet.end(), nextSet.begin(), nextSet.end(), merges.begin());

        // Empty intersection
        if (end == merges.begin())
            return NULL;

        // Intersection has a single member
        if (end == ++merges.begin()) {
            candidate = merges[0];
            continue;
        }

        // Intersection has multiple members
        prevSet = DominanceFrontier::DomSetType();
        for (std::vector<BasicBlock*>::iterator i = merges.begin(); i != end; ++i) {
            prevSet.insert(*i);
        }
    }

    return candidate;
}


} // end namespace gla_llvm

