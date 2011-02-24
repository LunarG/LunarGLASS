//===- StructuralAnalysis.h - Structural analysis ------------------------===//
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
