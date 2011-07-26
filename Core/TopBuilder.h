//===- TopBuilder.h - Help build/query LLVM for LunarGLASS -=====//
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
// Author: John Kessenich, LunarG
// Author: Michael Ilseman, LunarG
//
//===----------------------------------------------------------------------===//

#pragma once
#ifndef TopBuilder_H
#define TopBuilder_H

// LLVM includes
#include "llvm/Support/IRBuilder.h"
#include "llvm/IntrinsicInst.h"

#include "LunarGLASSManager.h"
#include "LunarGLASSTopIR.h"

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
    explicit Builder(llvm::IRBuilder<>& b, gla::Manager*);
    ~Builder();

    //
    // There is not matrix type in or added to LLVM for Top IR.
    //
    // The Matrix class is a structure to encapsulate a choice of
    // how to represent a matrix in LLVM.  It is the best way to
    // form correct Top IR for operating on matrices.
    //
    class Matrix {
    public:
        explicit Matrix(llvm::Value* m);
        Matrix(int c, int r, Matrix*);
        static const llvm::Type* getType(const llvm::Type* elementType, int numColumns, int numRows);

        int getNumRows() const { return numRows; }
        int getNumColumns() const { return numColumns; }

        llvm::Value* getValue() const { return matrix; }
        const llvm::Type* getColumnType() const { return matrix->getType()->getContainedType(0); }
        const llvm::Type* getElementType() const { return getColumnType()->getContainedType(0); }

    protected:
        int numColumns;
        int numRows;

        llvm::Value* matrix;
    };

    //
    // SuperValue can hold either an LLVM value or something else,
    // automatically converting to/from an LLVM value when needed,
    // and automatically converting from the 'something else', but
    // requiring manual access of the 'something else'
    //
    class SuperValue {
    public:
        SuperValue() : type(ELlvm) { value.llvm = 0; }

        // These are both constructors and implicit conversions
        SuperValue(llvm::Value* llvm) : type(ELlvm) { value.llvm = llvm; } // implicitly make a SuperValue out of a Value
        SuperValue(Matrix* m) : type(EMatrix) { value.matrix = m; }        // implicitly make a SuperValue out of a Matrix

        // implicitly make a Value out of a SuperValue
        operator llvm::Value*() const
        {
            return getValue();
        }

        // make a Value when derefencing a SuperValue
        llvm::Value* operator->() const
        {
            return getValue();
        }

        void clear()
        {
            type = ELlvm;
            value.llvm = 0;
        }

        bool isMatrix() const { return type == EMatrix; }
        bool isValue() const { return type == ELlvm; }
        bool isClear() const { return type == ELlvm && value.llvm == 0; }

        llvm::Value* getValue() const
        {
            switch (type) {
            case ELlvm:   return value.llvm;
            case EMatrix: return value.matrix->getValue();
            default:
                assert(0);
                return value.llvm;
            }
        }

        Matrix* getMatrix() const
        {
            assert(type == EMatrix);
            return value.matrix;
        }

    protected:
        enum {
            EMatrix,
            ELlvm
        } type;

        union {
            Matrix* matrix;
            llvm::Value* llvm;
        } value;
    };  // end class SuperValue

    static llvm::Constant* getConstant(std::vector<llvm::Constant*>&, const llvm::Type*);

    // Make the main function. Returns the entry block
    llvm::BasicBlock* makeMain();

    // Close the main function.
    void closeMain();

    void makeMainReturn() { makeReturn(NULL, true); }

    // Create a return. Pass whether it is a return form main, and the return
    // value (if applicable).
    void makeReturn(llvm::Value* retVal=NULL, bool isMain = false);

    // Create a discard. Pass whether this is occuring in main. Currently,
    // non-main functions are unsupported, and so are discards occuring in them.
    // TODO: support discards in non-main functions
    void makeDiscard(bool isMain);


    // Make a shader-style function, create its entry block.
    // Return the function, pass back the entry.
    llvm::Function* makeFunctionEntry(const llvm::Type* type, const char* name, std::vector<const llvm::Type*> paramTypes, llvm::BasicBlock*& entry);

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

    // Create an LLVM variable out of a generic "shader-style" description of a
    // variable.
    SuperValue createVariable(EStorageQualifier, int storageInstance, const llvm::Type*, bool isMatrix,
                              llvm::Constant* initializer, const std::string* annotation, const std::string& name);

    // Store SuperValue into another SuperValue and return the l-value
    SuperValue createStore(SuperValue rValue, SuperValue lValue);

    // Load SuperValue from a SuperValue and return it
    SuperValue createLoad(SuperValue);

    // Create a GEP to dereference structs, arrays, or matrices
    SuperValue createGEP(SuperValue, std::vector<llvm::Value*>);

    // Create a InsertValue to handle structs, arrays, or matrices
    SuperValue createInsertValue(SuperValue, SuperValue, unsigned* indices, int indexCount);

    // Copy out to the pipeline the outputs we've been caching in variables
    void copyOutPipeline();

    llvm::Value* readPipeline(const llvm::Type*, std::string& name, int slot, EInterpolationMode mode = EIMNone, float offsetx = 0.0, float offsety = 0.0);

    llvm::Value* createSwizzle(llvm::Value* source, int swizzleMask, const llvm::Type* finalType);

    // Matrix factory that tracks what to delete
    Matrix* newMatrix(llvm::Value*);

    // handle component-wise matrix operations for either a
    // pair of matrices or a matrix and a scalar
    SuperValue createMatrixOp(llvm::Instruction::BinaryOps, SuperValue left, SuperValue right);

    // handle all the possible matrix-related multiply operations
    // (non component wise; linear algebraic) for all combinations
    // of matrices, scalars, and vectors that either consume or
    // create a matrix
    SuperValue createMatrixMultiply(SuperValue left, SuperValue right);
    SuperValue createMatrixCompare (SuperValue left, SuperValue right, bool allEqual);

    // handle matrix to matrix operations
    Matrix* createMatrixTranspose  (Matrix*);
    Matrix* createMatrixInverse    (Matrix*);
    Matrix* createMatrixDeterminant(Matrix*);

    // Handy way to get intrinsics
    llvm::Function* getIntrinsic(llvm::Intrinsic::ID, const llvm::Type*);
    llvm::Function* getIntrinsic(llvm::Intrinsic::ID, const llvm::Type*, const llvm::Type*);
    llvm::Function* getIntrinsic(llvm::Intrinsic::ID, const llvm::Type*, const llvm::Type*, const llvm::Type*);
    llvm::Function* getIntrinsic(llvm::Intrinsic::ID, const llvm::Type*, const llvm::Type*, const llvm::Type*, const llvm::Type*);

    // if one operand is a scalar and the other is a vector, promote the scalar to match
    void promoteScalar(SuperValue& left, SuperValue& right);

    // make a value by smearing the scalar to fill the type
    llvm::Value* smearScalar(llvm::Value* scalarVal, const llvm::Type*);

    // List of parameters used to create a texture intrinsic
    struct TextureParameters {
        llvm::Value* ETPCoords;
        llvm::Value* ETPBiasLod;
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
    llvm::Value* createTextureCall(const llvm::Type*, gla::ESamplerType, int texFlags, const TextureParameters&);
    llvm::Value* createIntrinsicCall(llvm::Intrinsic::ID, SuperValue);
    llvm::Value* createIntrinsicCall(llvm::Intrinsic::ID, SuperValue, SuperValue);
    llvm::Value* createIntrinsicCall(llvm::Intrinsic::ID, SuperValue, SuperValue, SuperValue);
    llvm::Value* createRecip(llvm::Value*);
    llvm::Value* createCompare(llvm::Value* lhs, llvm::Value* rhs, bool equal, bool isFloat, bool isSigned);

    class If {
    public:
        If(llvm::Value* condition, bool withElse, Builder* glaBuilder);
        ~If() {}

        void makeEndThen();
        void makeEndIf();

    private:
        Builder* glaBuilder;
        llvm::Function* function;
        llvm::BasicBlock* thenBB;
        llvm::BasicBlock* elseBB;
        llvm::BasicBlock* mergeBB;
    };

    // Start the beginning of a new loop. For inductive loops, specify the
    // inductive variable, what value it starts at, when it finishes, and how
    // much it increments by on each iteration. Also specify whether you want
    // this Builder to do the increment (true), or if you will do it yourself
    // (false).
    void makeNewLoop();
    void makeNewLoop(llvm::Value* inductiveVariable, llvm::Constant* from, llvm::Constant* finish,
                     llvm::Constant* increment,  bool builderDoesIncrement);

    // Add a back-edge (e.g "continue") for the innermost loop that you're in
    void makeLoopBackEdge();

    // Add an exit (e.g. "break") for the innermost loop that you're in
    void makeLoopExit();

    // Close the innermost loop that you're in
    void closeLoop();


protected:
    llvm::Value* createMatrixTimesVector(Matrix*, llvm::Value*);
    llvm::Value* createVectorTimesMatrix(llvm::Value*, Matrix*);

    Matrix* createMatrixOp(llvm::Instruction::BinaryOps llvmOpcode, Matrix* left, Matrix* right);
    Matrix* createSmearedMatrixOp(llvm::Instruction::BinaryOps, Matrix*, llvm::Value*, bool reverseOrder);
    Matrix* createMatrixTimesMatrix(Matrix*, Matrix*);
    Matrix* createOuterProduct(llvm::Value* lvector, llvm::Value* rvector);

    llvm::IRBuilder<>& builder;
    gla::Manager* manager;
    llvm::Module* module;
    llvm::LLVMContext &context;

    // accumulate values that must be copied out at the end
    std::vector<llvm::Value*> copyOuts;

    // accumulate matrices that must be deleted at the end
    std::vector<Matrix*> matrixList;

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
};  // end Builder class

};  // end gla namespace

#endif // TopBuilder_H
