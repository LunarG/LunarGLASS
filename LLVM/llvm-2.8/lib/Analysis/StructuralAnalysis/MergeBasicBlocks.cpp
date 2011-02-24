//===- MergeBlocks.cpp - Merge blocks when possible -----------------------===//
//
//                     The LLVM Compiler Infrastructure
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
// Merge basic blocks when possible
//
//===----------------------------------------------------------------------===//

#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

namespace llvm {

    class MergeBasicBlocks : public FunctionPass {
    public:
        // Rest is standard pass stuff
        static char ID;
        MergeBasicBlocks() : FunctionPass(ID) {};

        virtual bool runOnFunction(Function &);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;
    private:
        // Merge a block with its predecessor, returning true if a merge was performed.
        // Currently outputs diagnostic info
        bool mergeBlock(BasicBlock *bb);

        bool mergeBlocks(Function &);
    };
} // End llvm namespace


using namespace llvm;

bool MergeBasicBlocks::runOnFunction(Function &F) {
    errs() << "Merging basic blocks\n";
    return MergeBasicBlocks::mergeBlocks(F);
}

bool MergeBasicBlocks::mergeBlocks(Function &F) {
    bool changing = true;
    bool changed = false;
    while (changing) {
        for (Function::iterator i = F.begin(), e = F.end(); i != e; ++i) {
            changing = mergeBlock(i);
            if (changing)
                break;
            else
                continue;
        }
    }

    return changed;
}

bool MergeBasicBlocks::mergeBlock(BasicBlock* bb) {
    errs() << "  Trying " << bb->getName() << " ... ";
    bool didItHappen = MergeBlockIntoPredecessor(bb);
    errs() << (didItHappen ? "merged" : "not merged") << "\n";
    return didItHappen;
}

void MergeBasicBlocks::print(raw_ostream &OS, const Module*) const {
    return;
}

void MergeBasicBlocks::getAnalysisUsage(AnalysisUsage& AU) const {
    return;
}

char MergeBasicBlocks::ID = 0;
INITIALIZE_PASS(MergeBasicBlocks,
                "mergeblocks",
                "Merge basic blocks when possible",
                false,   // Whether it preserves the CFG
                false);  // Is an analysis pass


