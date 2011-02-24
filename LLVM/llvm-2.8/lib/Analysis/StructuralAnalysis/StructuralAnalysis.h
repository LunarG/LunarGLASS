//===- StructuralAnalysis.h - Structural analysis ----------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//

//===----------------------------------------------------------------------===//
//
// Perform Structural Analysis, as outlined in Muchnick
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_ANALYSIS_STRUCTURAL_ANALYSIS_H
#define LLVM_ANALYSIS_STRUCTURAL_ANALYSIS_H

#include "llvm/Analysis/RegionInfo.h"
#include "llvm/Analysis/LoopInfo.h"


namespace llvm {

    //===----------------------------------------------------------------------===//
    /// @brief Perform structural analysis.
    ///
    /// The StructuralAnalysis pass identifies and classifies structured control flow
    class StructuralAnalysis : public FunctionPass {
    public:

        // Structures to identify
        // Todo: expand and indentify more specific cases
        enum RegionType { IfThen              // Basic if construct
                        , IfThenElse          // Basic if else construct
                        , FallthroughSwitch   // C-style switch statements
                        , NaturalLoop         // A natural loop
                        , Improper            // An improper interval (unidentifiable, cyclic)
                        , Proper              // A proper interval (unidentifiable, cyclic)
                        , Blocks              // A series of blocks
                        , UNTAGGED            // The pass hasn't tagged this region yet
                        // , WhileLoop        // A loop that is specifically a while loop
                        // , BreakingSwitch   // Pascal-style switch statements
                        // , SelfLoop         // Single block self-loop
                        };

        // Mapping between regions and their identified types
        DenseMap<Region*, RegionType> regionToType;

        typedef DenseMapIterator<Region*, RegionType> RegionToTypeIterator;

        // Rest is standard pass stuff
        static char ID;
        StructuralAnalysis() : FunctionPass(ID) {};

        virtual bool runOnFunction(Function &);
        void print(raw_ostream&, const Module* = 0) const;
        virtual void getAnalysisUsage(AnalysisUsage&) const;

    private:
        // The results of the region analysis pass
        const RegionInfo* RI;

        // The results of the loop analysis pass
        const LoopInfo* LI;

        // Untagged regions
        SmallPtrSet<Region*, 20> untagged;

        // Untagged loops
        SmallVector<Loop*, 5> untaggedLoops;

        // Helper functions below
        void printRegionTypes();
        void initialize();
        void addInImmediateSubRegions(Region*);
        void addInImmediateSubLoops(Loop*);
        void tagNaturalLoops();

    };

} // End llvm namespace



#endif
