//===- metadata.h - LLVM Metadata for LunarGLASS -============================//
//
//Copyright (C) 2010-2013 LunarG, Inc.
//
//All rights reserved.
//
//Redistribution and use in source and binary forms, with or without
//modification, are permitted provided that the following conditions
//are met:
//
//    Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
//
//    Redistributions in binary form must reproduce the above
//    copyright notice, this list of conditions and the following
//    disclaimer in the documentation and/or other materials provided
//    with the distribution.
//
//    Neither the name of LunarG Inc. nor the names of its
//    contributors may be used to endorse or promote products derived
//    from this software without specific prior written permission.
//
//THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
//FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
//COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
//INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
//BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
//LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
//CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
//LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
//ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
//POSSIBILITY OF SUCH DAMAGE.
//
//===----------------------------------------------------------------------===//
//
// Author: John Kessenich, LunarG
//
//===----------------------------------------------------------------------===//

#pragma once
#ifndef metadata_H
#define metadata_H

// LunarGLASS includes
#include "LunarGLASSTopIR.h"

// LLVM includes
#include "llvm/Module.h"
#include "llvm/Metadata.h"

namespace gla {

// Forms of metadata nodes, pointed to by instructions, by named metadata, or by other metadata
//
//     !input/output/uniform -> { name, EMdInputOutput,  Value*, !typeLayout, !aggregateLayout }
//     Notes:
//         - Value* is a proxy for getting the LLVM type
//         - aggregateLayout == 0 if not in a block
//
//     !sampler -> { EMdSampler, Value*, EMdSamplerDim, array, shadow, EMdSamplerBaseType }
//     Notes:
//         - texel return value has the same type as Value*, modified by EMdSamplerBaseType (e.g., unsigned int)
//         - array is bool, true if it is a samplerArray
//         - shadow is bool, true if it is a samplerShadow
//
//     !typeLayout -> { EMdTypeLayout, EMdPrecision, location, !sampler }
//     Notes:
//         - the type layout is how it is known if something is a matrix or unsigned integer, 
//           as this is not in the LLVM type
//         - location is the *first* location of the variable, which can span many slots/locations
//         - location is >= MaxUserLayoutLocation for non-user specified locations, to be patched
//           later by a linker
//         - location is < MaxUserLayoutLocation for user-assigned locations
//         - the intrinsic's slot can be different when reading a single slot out of the middle of a large input/output
//
//     !precision -> { EMdPrecision }
//
//     !aggregateLayout -> { name, number instances, EMdAggregateLayout, list of typeLayout with nested structures flattened }
//         - TODO: linker: the aggregateLayout form is not yet being generated
//
// Forms of named metadata
//
//     !inputs -> !{ list of all pipeline !input }
//     !outputs -> !{ list of all pipeline !output }
//     !defaultUniforms -> !{ list of all !uniform, not in blocks }
//

const int MaxUserLayoutLocation = 1024;

// what kind of I/O:
enum EMdInputOutput {
    EMioNone,               // for something that is not I/O, should not show up in a metadata node

    // inputs
    EMioPipeIn,             // normal user-streamed input data: attributes, varyings, etc.
    EMioVertexId,
    EMioInstanceId,
    EMioFragmentFace,
    EMioFragmentCoord,
    EMioPointCoord,

    // outputs
    EMioPipeOut,            // normal user-streamed output data, including fragment color
    EMioVertexPosition,
    EMioPointSize,
    EMioClipVertex,
    EMioFragmentDepth,

    // uniforms
    EMioDefaultUniform,      // a uniform not in a block
    EMioUniformBlockMember,  // uniform buffer
    EMioBufferBlockMember,   // shader storage buffer object
};

// How the *interior* of a single, non-aggregate entity is laid out, supplemental to the "Value* for type"
enum EMdTypeLayout {
    EMtlNone,
    EMtlUnsigned,           // unsigned integer type, other type information comes from the LLVM value->type
    EMtlRowMajorMatrix,
    EMtlColMajorMatrix,
    EMtlAggregate,
    EMtlSampler
};

// How a block or structure is laid out
enum EMdAggregateLayout {
    EMblShared,
    EMblStd140,
    EMblStd430,
    EMblPacked,
};

// What kind of sampler
enum EMdSampler {
    EMsTexture,
    EMsImage,
};

// Dimensionality of sampler
enum EMdSamplerDim {
    EMsd1D,
    EMsd2D,
    EMsd3D,
    EMsdCube,
    EMsdRect,
    EMsdBuffer,
};

enum EMdSamplerBaseType {
    EMsbFloat,
    EMsbInt,
    EMsbUint,
};

// ESSL precision qualifier
enum EMdPrecision {
    EMpNone,
    EMpLow,
    EMpMedium,
    EMpHigh,
    EMpCount,
};

//
// Crackers are for the consumer of the IR.
// They take an MD node, or instruction that might point to one, and decode it, as per the enums above.
//

inline bool CrackTypeLayout(const llvm::MDNode* md, EMdTypeLayout& layout, EMdPrecision& precision, int& location, llvm::MDNode*& sampler)
{
    const llvm::ConstantInt* constInt = llvm::dyn_cast<llvm::ConstantInt>(md->getOperand(0));
    if (! constInt)
        return false;
    layout = (EMdTypeLayout)constInt->getSExtValue();

    constInt = llvm::dyn_cast<llvm::ConstantInt>(md->getOperand(1));
    if (! constInt)
        return false;
    precision = (EMdPrecision)constInt->getSExtValue();

    constInt = llvm::dyn_cast<llvm::ConstantInt>(md->getOperand(2));
    if (! constInt)
        return false;
    location = constInt->getSExtValue();

    llvm::Value* speculativeSampler = md->getOperand(3);
    if (speculativeSampler)
        sampler = llvm::dyn_cast<llvm::MDNode>(speculativeSampler);
    else
        sampler = 0;

    return true;
}

inline bool CrackIOMd(const llvm::MDNode* md, std::string& symbolName, EMdInputOutput& qualifier, llvm::Type*& type,
                      EMdTypeLayout& layout, EMdPrecision& precision, int& location, llvm::MDNode*& sampler, llvm::MDNode*& aggregate)
{
    symbolName = md->getOperand(0)->getName();

    const llvm::ConstantInt* constInt = llvm::dyn_cast<llvm::ConstantInt>(md->getOperand(1));
    if (! constInt)
        return false;
    qualifier = (EMdInputOutput)constInt->getSExtValue();

    if (! md->getOperand(2))
        return false;
    type = md->getOperand(2)->getType();

    llvm::MDNode* layoutMd = llvm::dyn_cast<llvm::MDNode>(md->getOperand(3));
    if (! layoutMd)
        return false;

    if (! CrackTypeLayout(layoutMd, layout, precision, location, sampler))
        return false;

    aggregate = 0;
    
    return true;
}

inline bool CrackIOMd(const llvm::Instruction* instruction, llvm::StringRef kind, std::string& symbolName, EMdInputOutput& qualifier, llvm::Type*& type,
                      EMdTypeLayout& layout, EMdPrecision& precision, int& location, llvm::MDNode*& sampler, llvm::MDNode*& aggregate)
{
    const llvm::MDNode* md = instruction->getMetadata(kind);
    if (! md)
        return false;

    return CrackIOMd(md, symbolName, qualifier, type, layout, precision, location, sampler, aggregate);
}

inline bool CrackInputMd(const llvm::Instruction* instruction, std::string& symbolName, EMdInputOutput& qualifier, llvm::Type*& type,
                         EMdTypeLayout& layout, EMdPrecision& precision, int& location, llvm::MDNode*& aggregate)
{
    llvm::MDNode* dummySampler;

    return CrackIOMd(instruction, "input", symbolName, qualifier, type, layout, precision, location, dummySampler, aggregate);
}

inline bool CrackOutputMd(const llvm::Instruction* instruction, std::string& symbolName, EMdInputOutput& qualifier, llvm::Type*& type,
                          EMdTypeLayout& layout, EMdPrecision& precision, int& location, llvm::MDNode*& aggregate)
{
    llvm::MDNode* dummySampler;

    return CrackIOMd(instruction, "output", symbolName, qualifier, type, layout, precision, location, dummySampler, aggregate);
}

inline bool CrackUniformMd(const llvm::Instruction* instruction, std::string& symbolName, EMdInputOutput& qualifier, llvm::Type*& type,
                           EMdTypeLayout& layout, EMdPrecision& precision, int& location, llvm::MDNode*& sampler, llvm::MDNode*& aggregate)
{
    return CrackIOMd(instruction, "uniform", symbolName, qualifier, type, layout, precision, location, sampler, aggregate);
}

inline bool CrackSamplerMd(llvm::MDNode* md, EMdSampler& sampler, llvm::Type*& type, EMdSamplerDim& dim, bool& isArray, bool& isShadow, EMdSamplerBaseType& baseType)
{
    const llvm::ConstantInt* constInt = llvm::dyn_cast<llvm::ConstantInt>(md->getOperand(0));
    if (! constInt)
        return false;
    sampler = (EMdSampler)constInt->getSExtValue();

    if (! md->getOperand(1))
        return false;
    type = md->getOperand(1)->getType();

    constInt = llvm::dyn_cast<llvm::ConstantInt>(md->getOperand(2));
    if (! constInt)
        return false;
    dim = (EMdSamplerDim)constInt->getSExtValue();

    constInt = llvm::dyn_cast<llvm::ConstantInt>(md->getOperand(3));
    if (! constInt)
        return false;
    isArray = constInt->getSExtValue() != 0;

    constInt = llvm::dyn_cast<llvm::ConstantInt>(md->getOperand(4));
    if (! constInt)
        return false;
    isShadow = constInt->getSExtValue() != 0;

    constInt = llvm::dyn_cast<llvm::ConstantInt>(md->getOperand(5));
    if (! constInt)
        return false;
    baseType = (EMdSamplerBaseType)constInt->getSExtValue();

    return true;
}

inline bool CrackPrecisionMd(const llvm::Instruction* instruction, EMdPrecision& precision)
{
    precision = EMpNone;

    const llvm::MDNode* md = instruction->getMetadata("precision");
    if (! md)
        return false;

    const llvm::ConstantInt* constInt = llvm::dyn_cast<llvm::ConstantInt>(md->getOperand(0));
    if (! constInt)
        return false;
    precision = (EMdPrecision)constInt->getSExtValue();

    return true;
}

//
// Metadata class is just for adapter while building the IR.
//
class Metadata {
public:
    Metadata(llvm::LLVMContext& c, llvm::Module* m) : context(c), module(m) 
    {
        // Pre cache the precision qualifier nodes, there are only 4 to reuse
        for (int i = 0; i < EMpCount; ++i) {
            llvm::Value* args[] = {
                gla::MakeIntConstant(context, i),
            };
            precisionMd[i] = llvm::MDNode::get(context, args);
        }
    }

    // "input/output/uniform ->" as per comment at top of file
    llvm::MDNode* makeMdInputOutput(llvm::StringRef symbolName, llvm::StringRef sectionName, EMdInputOutput qualifier, 
                                    llvm::Value* typeProxy, EMdTypeLayout layout, EMdPrecision precision, int location, 
                                    llvm::MDNode* sampler = 0, llvm::MDNode* aggregate = 0)
    {
        llvm::Value* args[] = {
            llvm::MDString::get(context, symbolName),
            gla::MakeIntConstant(context, qualifier),
            typeProxy,
            makeMdTypeLayout(layout, precision, location, sampler)
        };
        llvm::MDNode* md = llvm::MDNode::get(context, args);

        llvm::NamedMDNode* namedNode = module->getOrInsertNamedMetadata(sectionName);
        namedNode->addOperand(md);

        return md;
    }

    // "sampler ->" as per comment at top of file
    llvm::MDNode* makeMdSampler(EMdSampler sampler, llvm::Value* typeProxy, EMdSamplerDim dim, bool isArray, bool isShadow, EMdSamplerBaseType baseType)
    {
        llvm::Value* args[] = {
            gla::MakeIntConstant(context, sampler),
            typeProxy,
            gla::MakeIntConstant(context, dim),
            gla::MakeBoolConstant(context, isArray),
            gla::MakeBoolConstant(context, isShadow),
            gla::MakeIntConstant(context, baseType),
        };
        llvm::MDNode* md = llvm::MDNode::get(context, args);

        llvm::NamedMDNode* namedNode = module->getOrInsertNamedMetadata("samplers");
        namedNode->addOperand(md);

        return md;
    }

    llvm::MDNode* makeMdPrecision(EMdPrecision precision)
    {
        // just use our precision cache

        return precisionMd[precision];
    }

protected:
    llvm::MDNode* makeMdTypeLayout(EMdTypeLayout layout, EMdPrecision precision, int location, llvm::MDNode* sampler)
    {
        llvm::Value* layoutArgs[] = {
            gla::MakeIntConstant(context, layout),
            gla::MakeIntConstant(context, precision),
            gla::MakeIntConstant(context, location),
            sampler,
        };

        return llvm::MDNode::get(context, layoutArgs);
    }

    llvm::LLVMContext& context;
    llvm::Module* module;
    llvm::MDNode* precisionMd[EMpCount];
};

};

#endif // metadata_H
