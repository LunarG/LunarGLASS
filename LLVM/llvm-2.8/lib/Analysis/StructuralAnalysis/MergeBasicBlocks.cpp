//===- MergeBlocks.cpp - Merge blocks when possible ----------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
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


