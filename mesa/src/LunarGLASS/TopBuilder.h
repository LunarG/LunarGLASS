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
//
//===----------------------------------------------------------------------===//

#pragma once
#ifndef TopBuilder_H
#define TopBuilder_H

// LLVM includes
#include "llvm/Support/IRBuilder.h"
#include "llvm/IntrinsicInst.h"

#include <list>

namespace gla {

//
// Builder is an interface to help build (along with LLVM) the top IR.
// These structures are not part of the definition of the top IR;
// just helpers to build the Top IR.
//

class Builder {
public:
    Builder() { }
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
        static const llvm::Type* getType(llvm::Type* elementType, int numColumns, int numRows);

        int getNumRows() const { return numRows; }
        int getNumColumns() const { return numColumns; }

        llvm::Value* getMatrixValue() const { return matrix; }

    protected:
        int numRows;
        int numColumns;

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
            assert(type == ELlvm);
            return value.llvm;
        }

        // make a Value when derefencing a SuperValue
        llvm::Value* operator->() const
        {
            assert(type == ELlvm);
            return value.llvm;
        }

        void clear()
        {
            type = ELlvm;
            value.llvm = 0;
        }

        bool isMatrix() const { return type == EMatrix; }
        bool isValue() const { return type == ELlvm; }

        llvm::Value* getValue() const
        {
            assert(type == ELlvm);
            return value.llvm;
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

    static llvm::Constant* makeConstant(std::vector<llvm::Constant*>&);

    //
    // Storage qualifiers for communicating the basic storage class
    // of shader-style variable (not all possible qualification types in the
    // various source languages, nor the way LLVM looks at things).
    //
    enum EStorageQualifier {
        ESQUniform,
        ESQInput,       // from a shader, not a function
        ESQOutput,      // from a shader, not a function
        ESQGlobal,      // no storage qualifier, just a global variable
        ESQLocal,
    };

    // Create an LLVM variable out of a generic "shader-style" description of a
    // variable.
    gla::Builder::SuperValue createVariable(llvm::IRBuilder<>&, EStorageQualifier, int storageInstance, const llvm::Type*, bool isMatrix,
                                                    llvm::Constant* initializer, const std::string* annotation, const std::string& name);
    // Copy out to the pipeline the outputs we've been caching in variables
    void copyOutPipeline(llvm::IRBuilder<>& builder);

    static llvm::Value* readPipeline(llvm::IRBuilder<>&, const llvm::Type*, std::string& name, int slot, EInterpolationMode mode = EIMNone, float offsetx = 0.0, float offsety = 0.0);

    // Matrix factory that tracks what to delete
    Matrix* newMatrix(llvm::Value*);

    // handle component-wise matrix operations for either a
    // pair of matrices or a matrix and a scalar
    SuperValue createMatrixOp(llvm::IRBuilder<>&, llvm::Instruction::BinaryOps, SuperValue left, SuperValue right);

    // handle all the possible matrix-related multiply operations
    // (non component wise; linear algebraic) for all combinations
    // of matrices, scalars, and vectors that either consume or
    // create a matrix
    SuperValue createMatrixMultiply(llvm::IRBuilder<>&, SuperValue left, SuperValue right);
    static SuperValue createMatrixCompare (llvm::IRBuilder<>&, SuperValue left, SuperValue right, bool allEqual);

    // handle matrix to matrix operations
    Matrix* createMatrixTranspose  (llvm::IRBuilder<>&, Matrix*);
    Matrix* createMatrixInverse    (llvm::IRBuilder<>&, Matrix*);
    Matrix* createMatrixDeterminant(llvm::IRBuilder<>&, Matrix*);

    // Handy way to get intrinsics
    static llvm::Function* makeIntrinsic(llvm::IRBuilder<>&, llvm::Intrinsic::ID, const llvm::Type*);
    static llvm::Function* makeIntrinsic(llvm::IRBuilder<>&, llvm::Intrinsic::ID, const llvm::Type*, const llvm::Type*);
    static llvm::Function* makeIntrinsic(llvm::IRBuilder<>&, llvm::Intrinsic::ID, const llvm::Type*, const llvm::Type*, const llvm::Type*);
    static llvm::Function* makeIntrinsic(llvm::IRBuilder<>&, llvm::Intrinsic::ID, const llvm::Type*, const llvm::Type*, const llvm::Type*, const llvm::Type*);

    // if one operand is a scalar and the other is a vector, promote the scalar to match
    static void promoteScalar(llvm::IRBuilder<>&, SuperValue& left, SuperValue& right);

    // make a value by smearing the scalar to fill the type
    static llvm::Value* smearScalar(llvm::IRBuilder<>&, llvm::Value* scalarVal, const llvm::Type*);

protected:
    static llvm::Value* createMatrixTimesVector(llvm::IRBuilder<>&, Matrix*, llvm::Value*);
    static llvm::Value* createVectorTimesMatrix(llvm::IRBuilder<>&, llvm::Value*, Matrix*);

    Matrix* createSmearedMatrixOp  (llvm::IRBuilder<>&, llvm::Instruction::BinaryOps, Matrix*, llvm::Value*, bool reverseOrder);
    static Matrix* createMatrixTimesMatrix(llvm::IRBuilder<>&, Matrix*, Matrix*);
    static Matrix* createOuterProduct     (llvm::IRBuilder<>&, llvm::Value* lvector, llvm::Value* rvector);

    // accumulate values that must be copied out at the end
    std::list<llvm::Value*> copyOuts;

    // accumulate matrices that must be deleted at the end
    std::vector<Matrix*> matrixList;

};  // end Builder class

};  // end gla namespace

#endif // TopBuilder_H
