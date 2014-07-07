//===- TopBuilder.h - Help build/query LLVM for LunarGLASS -=====//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (C) 2010-2014 LunarG, Inc.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 
//     Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.
// 
//     Redistributions in binary form must reproduce the above
//     copyright notice, this list of conditions and the following
//     disclaimer in the documentation and/or other materials provided
//     with the distribution.
// 
//     Neither the name of LunarG Inc. nor the names of its
//     contributors may be used to endorse or promote products derived
//     from this software without specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
// LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
// ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
//===----------------------------------------------------------------------===//
//
// Author: John Kessenich, LunarG
// Author: Michael Ilseman, LunarG
//
//===----------------------------------------------------------------------===//

#pragma once
#ifndef TopBuilder_H
#define TopBuilder_H

// LLVM includes
#pragma warning(push, 1)
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/IntrinsicInst.h"
#pragma warning(pop)

// LunarGLASS includes
#include "LunarGLASSManager.h"
#include "LunarGLASSTopIR.h"
#include "metadata.h"

#include <list>
#include <stack>

namespace gla {

//
// Builder is an interface to help build (along with LLVM) the top IR.
// These structures are not part of the definition of the top IR;
// just helpers to build the Top IR.
//

class Builder {
public:
    Builder(llvm::IRBuilder<>& b, Manager*, Metadata);
    ~Builder();

    //
    // Access chain helper for an R-Value vs. L-Value design:
    //
    // There is a single access chain the TopBuilder is building at
    // any particular time.  Such a chain can be used to either to a load or
    // a store, when desired.
    //
    // Expressions can be r-values, l-values, or both, or only r-values:
    //    a[b.c].d = ....  // l-value
    //    ... = a[b.c].d;  // r-value, that also looks like an l-value
    //    ++a[b.c].d;      // r-value and l-value
    //    (x + y)[2];      // r-value only, can't possibly be l-value
    //
    // Computing an r-value means generating code.  Hence,
    // r-values should only be computed when they are needed, not speculatively.
    //
    // Computing an l-value means saving away information for later use in the compiler,
    // no code is generated until the l-value is later dereferenced.  It is okay
    // to speculatively generate an l-value, just not okay to speculatively dereference it.
    //
    // It is pretty easy to have this chain work for both possible directions
    // of building the offsets/accessors:  right-to-left or left-to-right.
    // The user can change which direction the chains for loads and stores are
    // to be evaluated.  By default, the direction is right-to-left.
    //
    // The base of the access chain (the left-most variable or expression
    // from which everything is based) can be set either as an l-value
    // or as an r-value.  Most efficient would be to set an l-value if one
    // is available.  If an expression was evaluated, the resulting r-value
    // can be set as the chain base.
    //
    // The chain base can be set before or after the rest of the chain,
    // but the chain itself has to be set in order from one end to the
    // other.
    //
    // The users of this single access chain can save and restore if they
    // want to nest or manage multiple chains.
    //

    struct AccessChain {
        llvm::Value* base;
        std::vector<llvm::Value*> indexChain;
        llvm::Value* gep;         // cache of base+indexChain
        std::vector<int> swizzle;
        llvm::Value* component;
        llvm::Type* swizzleResultType;
        int swizzleTargetWidth;
        bool isRValue;
        bool trackActive;
        llvm::MDNode* mdNode;
        const char* metadataKind;
    };

    //
    // the top builder maintains a single active chain that
    // the following methods operated on
    //

    // for external save and restore
    AccessChain getAccessChain() { return accessChain; }
    void setAccessChain(AccessChain newChain) { accessChain = newChain; }

    // clear accessChain
    void clearAccessChain();

    // store metadata in the access chain for future tagging of the load/store instruction
    void setAccessChainMetadata(const char* mdk, llvm::MDNode* md)    
    { 
        accessChain.metadataKind = mdk;
        accessChain.mdNode = md; 
    }

    // say whether or not to evaluate a chain right to left (false means left to right)
    void setAccessChainDirectionRightToLeft(bool rightToLeft) { accessRightToLeft = rightToLeft; }

    // set new base as an l-value base
    void setAccessChainLValue(llvm::Value*);

    // set new base value as an r-value
    void setAccessChainRValue(llvm::Value*);

    // push offset onto the left end of the chain
    void accessChainPushLeft(llvm::Value* offset) { accessChain.indexChain.push_back(offset); }

    // push swizzle onto the left of any existing swizzle
    void accessChainPushSwizzleLeft(llvm::ArrayRef<int> swizzle, llvm::Type* type, int width);

    // push swizzle onto the right of any existing swizzle
    void accessChainPushSwizzleRight(llvm::ArrayRef<int> swizzle, llvm::Type* type, int width);

    // push a variable component selection onto the access chain; supporting only one, so unsided
    void accessChainPushComponent(llvm::Value* component) { accessChain.component = component; }

    // set pipeline input as an r-value, when pushing onto the left, meaning
    // the swizzle is yet to be consumed, but the access chain had to have been
    // collapsed already and consumed in order to create the passed in value
    void setAccessChainPipeValue(llvm::Value*);

    // use accessChain and swizzle to store value
    void accessChainStore(llvm::Value*);

    // use accessChain and swizzle to load an r-value
    llvm::Value* accessChainLoad(EMdPrecision);

    // return an offset representing the collection of offsets in the chain
    llvm::Value* collapseInputAccessChain();

    // Enable active tracking of the output variable's array elements or structure
    // members.  Only the subset of the object that might have been used will be
    // copied out when doing copyOutPipeline().
    void accessChainTrackActive() { accessChain.trackActive = true; }

    // Generate all the code needed to finish up a function, including main(),
    // which will copy out to the pipeline all the cached output variables.
    void leaveFunction(bool main);

    // Make the main function. Returns the entry block.
    llvm::BasicBlock* makeMain();

    // Close the main function.
    void closeMain();

    // Return from main. Implicit denotes a return at the very end of main.
    void makeMainReturn(bool implicit=false) { makeReturn(implicit, NULL, true); }

    // Create a return. Pass whether it is a return form main, and the return
    // value (if applicable). In the case of an implicit return, no post-return
    // block is inserted.
    void makeReturn(bool implicit=false, llvm::Value* retVal=NULL, bool isMain = false);

    // Create a discard. Pass whether this is occuring in main. Currently,
    // non-main functions with a discard on back ends that don't have a
    // a discard op are unsupported.
    void makeDiscard(bool isMain);

    // Make a shader-style function, and create its entry block if entry is non zero.
    // Return the function, pass back the entry.
    llvm::Function* makeFunctionEntry(llvm::Type* type, const char* name, llvm::ArrayRef<llvm::Type*> paramTypes,
                                      llvm::BasicBlock** entry = 0, bool external = false);

    //
    // Storage qualifiers for communicating the basic storage class
    // of shader-style variable (not all possible qualification types in the
    // various source languages, nor the way LLVM looks at things).
    //
    enum EStorageQualifier {
        ESQResource,
        ESQUniform,
        ESQInput,       // from a shader, not a function
        ESQOutput,      // from a shader, not a function
        ESQGlobal,      // no storage qualifier, just a global variable
        ESQLocal,
    };

    // Turn the array of constants into a proper LLVM constant type of the requested type.
    static llvm::Constant* getConstant(llvm::ArrayRef<llvm::Constant*>, llvm::Type*);

    // Create an LLVM variable out of a generic "shader-style" description of a
    // variable.
    llvm::Value* createVariable(EStorageQualifier, int storageInstance, llvm::Type*,
                                llvm::Constant* initializer, const std::string* annotation, llvm::StringRef name);

    // Create an alloca in the entry block.
    // (LLVM's promote memory to registers only works when alloca is in the entry block.)
    llvm::Value* createEntryAlloca(llvm::Type*, llvm::StringRef name = "");

    // Store llvm::Value* into another llvm::Value* and return the l-value
    llvm::Value* createStore(llvm::Value* rValue, llvm::Value* lValue);

    // Load llvm::Value* from a llvm::Value* and return it
    llvm::Value* createLoad(llvm::Value*, const char* metadataKind = 0, llvm::MDNode* metadata = 0);

    // Create a GEP to dereference structs, arrays, or matrices
    llvm::Value* createGEP(llvm::Value*, llvm::ArrayRef<llvm::Value*>);

    // Create a InsertValue to handle structs, arrays, or matrices
    llvm::Value* createInsertValue(llvm::Value* target, llvm::Value* source, unsigned* indices, int indexCount);

    // Finish setting all the other information for a copy-out value: metadata to 
    // tag the write instrinsic with and slot information.
    // This must be called after createVariable() with ESQOutput for the llvm::Value*.
    void setOutputMetadata(llvm::Value*, llvm::MDNode*, int baseSlot, int numSlots);

    // Copy out to the pipeline the outputs we've been caching in variables.
    // Do this after all use of the cached variables is complete.  Called
    // automatically by closeMain() or leaveFunction(true) for main.
    void copyOutPipeline();

    // Calling this will turn off implicit copy out when exiting the shader, and require
    // that copyOutPipeline() be called at each point where the outputs should be copied
    // out.
    void setExplicitPipelineCopyOut() { explicitPipelineCopyout = true; }

    // Write to the pipeline directly, without caching through variables
    // (User likely needs to select between the variable/copyOutPipeline
    //  model and the writePipeline model.)
    void writePipeline(llvm::Value*, llvm::Value* slot, int mask = -1,
                       llvm::MDNode* metadata = 0,
                       EInterpolationMethod method = EIMNone, EInterpolationLocation location = EILFragment);

    // Emit the right intrinsic to perform the descriped pipeline read.
    llvm::Value* readPipeline(EMdPrecision, 
                              llvm::Type*, llvm::StringRef name, int slot,
                              llvm::MDNode* metadata = 0,
                              int mask = -1,
                              EInterpolationMethod method = EIMNone, EInterpolationLocation location = EILFragment,
                              llvm::Value* offset = 0, llvm::Value* sampleIdx = 0);

    llvm::Value* createSwizzle(EMdPrecision, llvm::Value* source, int swizzleMask, llvm::Type* finalType);
    llvm::Value* createSwizzle(EMdPrecision, llvm::Value* source, llvm::ArrayRef<int> channels, llvm::Type* finalType);

    // If the value passed in is an instruction and the precision is not EMpNone, 
    // it gets tagged with the requested precision.
    void setInstructionPrecision(llvm::Value* value, EMdPrecision precision)
    {
        if (llvm::Instruction* instr = llvm::dyn_cast<llvm::Instruction>(value))
            setInstructionPrecision(instr, precision);
    }

    void setInstructionPrecision(llvm::Instruction* instr, EMdPrecision precision)
    {
        if (precision != EMpNone)
            instr->setMetadata(gla::PrecisionMdName, metadata.makeMdPrecision(precision));
    }

    // make a type for storing a matrix, which conforms to the 
    // assumptions of how matrices are operated on in the top builder
    llvm::Type* getMatrixType(llvm::Type* elementType, int numColumns, int numRows);

    // handle component-wise matrix operations for either a
    // pair of matrices or a matrix and a scalar
    llvm::Value* createMatrixOp(EMdPrecision, llvm::Instruction::BinaryOps, llvm::Value* left, llvm::Value* right);

    // handle all the possible matrix-related multiply operations
    // (non component wise; linear algebraic) for all combinations
    // of matrices, scalars, and vectors that either consume or
    // create a matrix
    llvm::Value* createMatrixMultiply(EMdPrecision, llvm::Value* left, llvm::Value* right);
    llvm::Value* createMatrixCompare (EMdPrecision, llvm::Value* left, llvm::Value* right, bool allEqual);

    // handle matrix to matrix operations
    llvm::Value* createMatrixTranspose  (EMdPrecision, llvm::Value*);
    llvm::Value* createMatrixInverse    (EMdPrecision, llvm::Value*);
    llvm::Value* createMatrixDeterminant(EMdPrecision, llvm::Value*);

    // Handy way to get intrinsics
    llvm::Function* getIntrinsic(llvm::Intrinsic::ID);
    llvm::Function* getIntrinsic(llvm::Intrinsic::ID, llvm::Type*);
    llvm::Function* getIntrinsic(llvm::Intrinsic::ID, llvm::Type*, llvm::Type*);
    llvm::Function* getIntrinsic(llvm::Intrinsic::ID, llvm::Type*, llvm::Type*, llvm::Type*);
    llvm::Function* getIntrinsic(llvm::Intrinsic::ID, llvm::Type*, llvm::Type*, llvm::Type*, llvm::Type*);
    llvm::Function* getIntrinsic(llvm::Intrinsic::ID, llvm::Type*, llvm::Type*, llvm::Type*, llvm::Type*, llvm::Type*);
    llvm::Function* getIntrinsic(llvm::Intrinsic::ID, llvm::Type*, llvm::Type*, llvm::Type*, llvm::Type*, llvm::Type*, llvm::Type*);

    // Can smear a scalar to a vector for the following forms:
    //   - promoteScalar(scalar, vector)  // smear scalar to width of vector
    //   - promoteScalar(vector, scalar)  // smear scalar to width of vector
    //   - promoteScalar(pointer, scalar) // smear scalar to width of what pointer points to
    //   - promoteScalar(scalar, scalar)  // do nothing
    // Other forms are not allowed.
    void promoteScalar(EMdPrecision, llvm::Value*& left, llvm::Value*& right);

    // make a value by smearing the scalar to fill the type
    llvm::Value* smearScalar(EMdPrecision, llvm::Value* scalarVal, llvm::Type*);

    // List of parameters used to create a texture intrinsic
    struct TextureParameters {
        llvm::Value* ETPCoords;
        llvm::Value* ETPBiasLod;       // Also holds the 'comp' argument for texel gather operations taking a component
        llvm::Value* ETPProj;
        llvm::Value* ETPOffset;
        llvm::Value* ETPShadowRef;
        llvm::Value* ETPGradX;
        llvm::Value* ETPGradY;
        llvm::Value* ETPArrayIndex;
        llvm::Value* ETPSampleNum;
        llvm::Value* ETPSampler;
        llvm::Value* ETPDimensions;
    };

    // Select the correct intrinsic based on all inputs, and make the call
    llvm::Value* createTextureCall(EMdPrecision, llvm::Type*, ESamplerType, int texFlags, const TextureParameters&);
    llvm::Value* createTextureQueryCall(EMdPrecision, llvm::Intrinsic::ID, llvm::Type*, llvm::Constant*, llvm::Value*, llvm::Value*);
    llvm::Value* createSamplePositionCall(EMdPrecision, llvm::Type*, llvm::Value*);
    llvm::Value* createBitFieldExtractCall(EMdPrecision, llvm::Value*, llvm::Value*, llvm::Value*, bool isSigned);
    llvm::Value* createBitFieldInsertCall(EMdPrecision, llvm::Value*, llvm::Value*, llvm::Value*, llvm::Value*);
    llvm::Value* createIntrinsicCall(llvm::Intrinsic::ID);
    llvm::Value* createIntrinsicCall(EMdPrecision, llvm::Intrinsic::ID);
    llvm::Value* createIntrinsicCall(EMdPrecision, llvm::Intrinsic::ID, llvm::Value*);
    llvm::Value* createIntrinsicCall(EMdPrecision, llvm::Intrinsic::ID, llvm::Value*, llvm::Value*);
    llvm::Value* createIntrinsicCall(EMdPrecision, llvm::Intrinsic::ID, llvm::Value*, llvm::Value*, llvm::Value*);
    llvm::Value* createRecip(EMdPrecision, llvm::Value*);

    // For equal and not-equal comparisons:
    // first one is preferred form: uses innate types, works on vectors, matrices, arrays, and structures
    llvm::Value* createCompare(EMdPrecision, llvm::Value*, llvm::Value*, bool /* true if for equal, fales if for not-equal */);
    // the following is deprecated: works on vectors or scalars
    llvm::Value* createCompare(EMdPrecision, llvm::Value* lhs, llvm::Value* rhs, bool equal, bool isFloat, bool isSigned);

    // vector constructor
    llvm::Value* createConstructor(EMdPrecision, const std::vector<llvm::Value*>& sources, llvm::Value* constructee);

    // matrix constructor
    llvm::Value* createMatrixConstructor(EMdPrecision, const std::vector<llvm::Value*>& sources, llvm::Value* constructee);

    // Helper to use for building nested control flow with if-then-else.
    class If {
    public:
        If(llvm::Value* condition, Builder* glaBuilder);
        ~If() {}

        void makeBeginElse();
        void makeEndIf();

    private:
        Builder* glaBuilder;
        llvm::Value* condition;
        llvm::Function* function;
        llvm::BasicBlock* headerBB;
        llvm::BasicBlock* thenBB;
        llvm::BasicBlock* elseBB;
        llvm::BasicBlock* mergeBB;
    };

    // Make a switch statement.  A switch has 'numSegments' of pieces of code, not containing 
    // any case/default labels, all separated by one or more case/default labels.  Each possible
    // case value v is a jump to the caseValues[v] segment.  The defaultSegment is also in this
    // number space.  How to compute the value is given by 'condition', as in switch(condition).
    //
    // The Top Builder will maintain the stack of post-switch merge blocks for nested switches.
    //
    // Use a defaultSegment < 0 if there is no default segment (to branch to post switch).
    //
    // Returns the right set of basic blocks to start each code segment with, so that the caller's
    // recursion stack can hold the memory for it.
    //
    void makeSwitch(llvm::Value* condition, int numSegments, std::vector<llvm::ConstantInt*> caseValues, std::vector<int> valueToSegment, int defaultSegment,
                    std::vector<llvm::BasicBlock*>& segmentBB);  // return argument

    // Add a branch to the innermost switch's merge block.
    void addSwitchBreak();

    // Move to the next code segment, passing in the return argument in makeSwitch()
    void nextSwitchSegment(std::vector<llvm::BasicBlock*>& segmentBB, int segment);

    // Finish off the innermost switch.
    void endSwitch(std::vector<llvm::BasicBlock*>& segmentBB);

    // Start the beginning of a new loop. For inductive loops, specify the
    // inductive variable, what value it starts at, when it finishes, and how
    // much it increments by on each iteration. Also specify whether you want
    // this Builder to do the increment (true), or if you will do it yourself
    // (false).
    void makeNewLoop();
    void makeNewLoop(llvm::Value* inductiveVariable, llvm::Constant* from, llvm::Constant* finish,
                     llvm::Constant* increment,  bool builderDoesIncrement);

    // Add a back-edge (e.g "continue") for the innermost loop that you're in
    void makeLoopBackEdge(bool implicit=false);

    // Add an exit (e.g. "break") for the innermost loop that you're in
    void makeLoopExit();

    // Close the innermost loop that you're in
    void closeLoop();

protected:
    AccessChain accessChain;
    bool accessRightToLeft;
    llvm::Value* collapseAccessChain();
    void simplifyAccessChainSwizzle();

    llvm::Value* createMatrixTimesVector(EMdPrecision, llvm::Value*, llvm::Value*);
    llvm::Value* createVectorTimesMatrix(EMdPrecision, llvm::Value*, llvm::Value*);

    llvm::Value* createComponentWiseMatrixOp(EMdPrecision, llvm::Instruction::BinaryOps llvmOpcode, llvm::Value* left, llvm::Value* right);
    llvm::Value* createSmearedMatrixOp(EMdPrecision, llvm::Instruction::BinaryOps, llvm::Value*, llvm::Value*, bool reverseOrder);
    llvm::Value* createMatrixTimesMatrix(EMdPrecision, llvm::Value*, llvm::Value*);
    llvm::Value* createOuterProduct(EMdPrecision, llvm::Value* lvector, llvm::Value* rvector);
    llvm::Value* createMatrixDeterminant(EMdPrecision, llvm::Value* (&matrix)[4][4], int size);
    void makeMatrixMinor(llvm::Value* (&matrix)[4][4], llvm::Value* (&minor)[4][4], int mRow, int mCol, int size);

    void createAndSetNoPredecessorBlock(llvm::StringRef name);

    llvm::IRBuilder<>& builder;
    Manager* manager;
    llvm::Module* module;
    llvm::LLVMContext &context;
    Metadata metadata;

    // Accumulate output values that are being cached in variables, instead of directly 
    // written to the output pipe, and hence must be copied out to the output pipeline
    // at shader exit instead.
    struct copyOut {
        llvm::Value* value;
        llvm::MDNode* mdNode;
        int baseSlot;
        int numSlots;              // both the number of slots and the number of indexes used in active
        std::vector<bool> active;  // slots that might be active, in order of the flattened value type
    };
    std::vector<copyOut> copyOuts; // the set of values forming the outputs-in-variables cache, one per top-level scalar/vector/aggregate    

    void setActiveOutput(llvm::Value* base, std::vector<llvm::Value*>& indexChain);
    void setActiveOutputSubset(copyOut&, llvm::Type*, int& activeIndex, const std::vector<llvm::Value*>& gepChain, int gepIndex, bool active);
    void copyOutOnePipeline(const copyOut&, llvm::Type*, llvm::MDNode*, int& slot, int& activeIndex, std::vector<llvm::Value*>& gepChain);

    // Switch stack
    std::stack<llvm::BasicBlock*> switches;

    // Data that needs to be kept in order to properly handle loops.
    struct LoopData {
        llvm::BasicBlock* header;
        llvm::BasicBlock* exit;

        llvm::Function* function;

        bool isInductive;
        bool builderDoesIncrement;

        llvm::Value* counter;
        llvm::Constant* finish;
        llvm::Constant* increment;
    };

    // Our loop stack.
    std::stack<LoopData> loops;

    // Special data for the main function to use. For GLSL-style returns, we
    // want to branch to copyOut, which then branches to exit. For GLSL-style
    // discards, we want to directly branch to exit.
    llvm::Function*   mainFunction;
    llvm::BasicBlock* stageEpilogue;
    llvm::BasicBlock* stageExit;
    bool explicitPipelineCopyout;

    // This data structure below is not a matrix... 
    // it's a cache of types for all possible matrix sizes.
    static const int minMatrixSize = 2;
    static const int maxMatrixSize = 4;
    llvm::Type* matrixTypeCache[maxMatrixSize-minMatrixSize+1][maxMatrixSize-minMatrixSize+1];
    bool useColumnBasedMatrixIntrinsics() const;
};  // end Builder class

};  // end gla namespace

#endif // TopBuilder_H
