//===- metadata.h - LLVM Metadata for LunarGLASS -============================//
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
//
//===----------------------------------------------------------------------===//

#pragma once
#ifndef metadata_H
#define metadata_H

// LunarGLASS includes
#include "LunarGLASSTopIR.h"

// LLVM includes
#pragma warning(push, 1)
#include "llvm/IR/Module.h"
#include "llvm/IR/Metadata.h"
#pragma warning(pop)

namespace gla {

// Forms of metadata nodes, pointed to by instructions, by named metadata, or by other metadata.
//
// Only the names starting with "!gla." actually appear in the IR, the other names here are 
// for ease of understanding the linkage between the nodes.
//
//
//     !gla.entrypoint -> { name, EMIoEntrypoint }
//     Notes:
//         - contains the name of an source entry point into the shader; e.g., "main"
//
//     !gla.precision -> { EMdPrecision }
//
//     !gla.intput ->   { name, EMdInputOutput,  Value*, !typeLayout, !aggregate }
//     !gla.output ->   same as above
//     !gla.uniform ->  same as above
//     Notes:
//         - the name is the name of the object (instance name for a block)
//         - Value* is a proxy for getting the LLVM type
//         - !aggregate is for blocks and structures
//                it's a block when EMdInputOutput is EMio*Member, !typeLayout will say how the block is laid out
//                it's a structure when the EMdTypeLayout is EMtlAggregate
//         - for blocks, the instance name is the name above, while the interface name is the name in the !aggregate
//         - for a block with no instance name, the name here will be empty ("")
//
//     !sampler -> { EMdSampler, Value*, EMdSamplerDim, array, shadow, EMdSamplerBaseType }
//     Notes:
//         - texel return value has the same type as Value*, modified by EMdSamplerBaseType (e.g., unsigned int)
//         - array is bool, true if it is a samplerArray
//         - shadow is bool, true if it is a samplerShadow
//
//     !typeLayout -> { EMdTypeLayout, EMdPrecision, location, !sampler, interpMode }
//     Notes:
//         - the type layout is how it is known if something is a matrix or unsigned integer, 
//           because this is not in the LLVM type
//         - location is the *first* location of the variable, which can span many slots/locations
//         - location is >= MaxUserLayoutLocation for non-user specified locations, to be patched
//           later by a linker
//         - location is < MaxUserLayoutLocation for user-assigned locations
//         - the intrinsic's slot can be different when reading a single slot out of the middle of a large input/output
//         - interpMode is present for EMioPipeOut as an integer encoded for MakeInterpolationMode() and CrackInterpolationMode()
//
//     !aggregate -> { name, !typeLayout, list of members: name1, !aggregate1, name2, !aggregate2, ... }
//     Notes:
//         - this recursively represents nested types, paralleling the LLVM type, but complementing it
//           (e.g., an llvm array of array might have EMdTypeLayout of EMtlRowMajorMatrix)
//         - the name in operand 0 is the name of the type (interface name for a block)
//         - each contained member has a pair of operands, one for the member's name, one for the member's type
//
//     When aggregates are used, the starting point is always a !gla.uniform, !gla.output, or gla.input node, which will
//     supply overall information and then point to a !aggregate for the hierarchical information.
//
//     Blocks can have two names:  
//        1) the instance name used in a reference, which could be missing
//        2) the interface name used only in the declaration, which must be present
//     The instance name will be in the !gla.uniform/input/output node, while the interface name will be in the
//     !aggregate node pointed to.
//
// Forms of named metadata
//
//     !gla.inputs = !{ list of all pipeline !input }
//     !gla.outputs = !{ list of all pipeline !output }
//     !gla.uniforms = !{ list of all !uniform }
//     !gla.invariant = !{ subset list of output that were declared as invariant }
//     !gla.entrypoint = !{ list of all entry points }
//     !gla.noStaticUse = !{ subset of input/output/uniform that were not statically referenced in the source shader }
//

// Operand names
const char* const InputMdName = "gla.input";
const char* const OutputMdName = "gla.output";
const char* const UniformMdName = "gla.uniform";
const char* const PrecisionMdName = "gla.precision";

// Named nodes
const char* const InputListMdName = "gla.inputs";
const char* const OutputListMdName = "gla.outputs";
const char* const UniformListMdName = "gla.uniforms";
const char* const InvariantListMdName = "gla.invariant";
const char* const EntrypointListMdName = "gla.entrypoint";
const char* const NoStaticUseMdName = "gla.noStaticUse";

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

    // Entry point into shader
    EMioEntrypoint,

    // in & out blocks
    EMioPipeOutBlock,         // output block
    EMioPipeInBlock,          // input block

    EMioCount,
};

// How the *interior* of a single, non-aggregate entity is laid out, supplemental to the "Value* for type"
// Also, how a block or structure is laid out, if applied to a block or structure
enum EMdTypeLayout {
    EMtlNone,

    // single-entity layouts
    EMtlUnsigned,           // unsigned integer type, other type information comes from the LLVM value->type
    EMtlRowMajorMatrix,
    EMtlColMajorMatrix,
    EMtlAggregate,
    EMtlSampler,

    // aggregate layouts
    EMtlShared,
    EMtlStd140,
    EMtlStd430,
    EMtlPacked,
    EMtlCount,
};

// What kind of sampler
enum EMdSampler {
    EMsTexture,
    EMsImage,
    EMsCount,
};

// Dimensionality of sampler
enum EMdSamplerDim {
    EMsd1D,
    EMsd2D,
    EMsd3D,
    EMsdCube,
    EMsdRect,
    EMsdBuffer,
    EMsdCount,
};

enum EMdSamplerBaseType {
    EMsbFloat,
    EMsbInt,
    EMsbUint,
    EMsbCount,
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

inline bool CrackTypeLayout(const llvm::MDNode* md, EMdTypeLayout& layout, EMdPrecision& precision, int& location, const llvm::MDNode*& sampler, int& interpMode)
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
    location = (int)constInt->getSExtValue();

    llvm::Value* speculativeSampler = md->getOperand(3);
    if (speculativeSampler)
        sampler = llvm::dyn_cast<llvm::MDNode>(speculativeSampler);
    else
        sampler = 0;

    if (md->getNumOperands() >= 5) {
        const llvm::ConstantInt* constInt = llvm::dyn_cast<llvm::ConstantInt>(md->getOperand(4));
        if (constInt)
            interpMode = (int)constInt->getZExtValue();
    } else
        interpMode = 0;

    return true;
}

inline bool CrackIOMd(const llvm::MDNode* md, std::string& symbolName, EMdInputOutput& qualifier, llvm::Type*& type,
                      EMdTypeLayout& layout, EMdPrecision& precision, int& location, const llvm::MDNode*& sampler, const llvm::MDNode*& aggregate, 
                      int& interpMode)
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
    if (! CrackTypeLayout(layoutMd, layout, precision, location, sampler, interpMode))
        return false;

    if (md->getNumOperands() >= 5)
        aggregate = llvm::dyn_cast<llvm::MDNode>(md->getOperand(4));
    else
        aggregate = 0;
    
    return true;
}

inline bool CrackAggregateMd(const llvm::MDNode* md, std::string& symbolName, 
                             EMdTypeLayout& layout, EMdPrecision& precision, int& location, const llvm::MDNode*& sampler)
{
    symbolName = md->getOperand(0)->getName();
    int dummyInterpMode;

    llvm::MDNode* layoutMd = llvm::dyn_cast<llvm::MDNode>(md->getOperand(1));
    if (! layoutMd)
        return false;
    if (! CrackTypeLayout(layoutMd, layout, precision, location, sampler, dummyInterpMode))
        return false;
    
    return true;
}

inline bool CrackIOMd(const llvm::Instruction* instruction, llvm::StringRef kind, std::string& symbolName, EMdInputOutput& qualifier, llvm::Type*& type,
                      EMdTypeLayout& layout, EMdPrecision& precision, int& location, const llvm::MDNode*& sampler, const llvm::MDNode*& aggregate,
                      int& interpMode)
{
    const llvm::MDNode* md = instruction->getMetadata(kind);
    if (! md)
        return false;

    return CrackIOMd(md, symbolName, qualifier, type, layout, precision, location, sampler, aggregate, interpMode);
}

inline bool CrackInputMd(const llvm::Instruction* instruction, std::string& symbolName, EMdInputOutput& qualifier, llvm::Type*& type,
                         EMdTypeLayout& layout, EMdPrecision& precision, int& location, const llvm::MDNode*& aggregate)
{
    const llvm::MDNode* dummySampler;
    int dummyInterpMode;

    return CrackIOMd(instruction, InputMdName, symbolName, qualifier, type, layout, precision, location, dummySampler, aggregate, dummyInterpMode);
}

inline bool CrackOutputMd(const llvm::Instruction* instruction, std::string& symbolName, EMdInputOutput& qualifier, llvm::Type*& type,
                          EMdTypeLayout& layout, EMdPrecision& precision, int& location, const llvm::MDNode*& aggregate)
{
    const llvm::MDNode* dummySampler;
    int dummyInterpMode;

    return CrackIOMd(instruction, OutputMdName, symbolName, qualifier, type, layout, precision, location, dummySampler, aggregate, dummyInterpMode);
}

inline bool CrackUniformMd(const llvm::Instruction* instruction, std::string& symbolName, EMdInputOutput& qualifier, llvm::Type*& type,
                           EMdTypeLayout& layout, EMdPrecision& precision, int& location, const llvm::MDNode*& sampler, const llvm::MDNode*& aggregate)
{
    int dummyInterpMode;

    return CrackIOMd(instruction, UniformMdName, symbolName, qualifier, type, layout, precision, location, sampler, aggregate, dummyInterpMode);
}

inline bool CrackSamplerMd(const llvm::MDNode* md, EMdSampler& sampler, llvm::Type*& type, EMdSamplerDim& dim, bool& isArray, bool& isShadow, EMdSamplerBaseType& baseType)
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

    const llvm::MDNode* md = instruction->getMetadata(PrecisionMdName);
    if (! md)
        return false;

    const llvm::ConstantInt* constInt = llvm::dyn_cast<llvm::ConstantInt>(md->getOperand(0));
    if (! constInt)
        return false;
    precision = (EMdPrecision)constInt->getSExtValue();

    return true;
}

// translate from 0-based counting to aggregate operand number
inline int GetAggregateMdNameOp(int index)
{
    return 2 + 2 * index;
}

inline int GetAggregateMdSubAggregateOp(int index)
{
    return 2 + 2 * index + 1;
}

// Just a short cut for when only the type is cared about, rather than cracking all the operands.
// It looks for both forms of whether this is a deferenced aggregate or a top-level interface md.
inline EMdTypeLayout GetMdTypeLayout(const llvm::MDNode *md)
{
    if (! md)
        return EMtlNone;

    // check for aggregate member form first
    llvm::MDNode* layoutMd = llvm::dyn_cast<llvm::MDNode>(md->getOperand(1));
    if (! layoutMd) {
        // check for top-level interface
        layoutMd = llvm::dyn_cast<llvm::MDNode>(md->getOperand(3));
        if (! layoutMd)
            return EMtlNone;
    }

    const llvm::ConstantInt* constInt = llvm::dyn_cast<llvm::ConstantInt>(layoutMd->getOperand(0));
    if (! constInt)
        return EMtlNone;
    
    return (EMdTypeLayout)constInt->getSExtValue();
}

// A shortcut for just getting the type of a sampler (int, uint, float) from an 
// instruction's metadata
inline EMdSamplerBaseType GetMdSamplerBaseType(const llvm::MDNode* md)
{
    if (! md || md->getNumOperands() < 4)
        return EMsbFloat;

    md = llvm::dyn_cast<llvm::MDNode>(md->getOperand(3));
    if (! md || md->getNumOperands() < 4 || md->getOperand(3) == 0)
        return EMsbFloat;

    md = llvm::dyn_cast<llvm::MDNode>(md->getOperand(3));
    if (! md || md->getNumOperands() < 6 || md->getOperand(5) == 0)
        return EMsbFloat;

    const llvm::ConstantInt* constInt = llvm::dyn_cast<llvm::ConstantInt>(md->getOperand(5));
    if (! constInt)
        return EMsbFloat;

    return (EMdSamplerBaseType)constInt->getSExtValue();    
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
                                    llvm::MDNode* sampler = 0, llvm::MDNode* aggregate = 0, int interpMode = -1)
    {
        llvm::MDNode* layoutMd = makeMdTypeLayout(layout, precision, location, sampler, interpMode);

        llvm::MDNode* md;
        if (aggregate) {
            llvm::Value* args[] = {
                llvm::MDString::get(context, symbolName),
                gla::MakeIntConstant(context, qualifier),
                typeProxy,
                layoutMd,
                aggregate
            };
            md = llvm::MDNode::get(context, args);
        } else  {
            llvm::Value* args[] = {
                llvm::MDString::get(context, symbolName),
                gla::MakeIntConstant(context, qualifier),
                typeProxy,
                layoutMd
            };
            md = llvm::MDNode::get(context, args);
        }        

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

        return md;
    }

    llvm::MDNode* makeMdPrecision(EMdPrecision precision)
    {
        // just use our precision cache

        return precisionMd[precision];
    }

    llvm::MDNode* makeMdTypeLayout(EMdTypeLayout layout, EMdPrecision precision, int location, llvm::MDNode* sampler, int interpMode = -1)
    {
        llvm::MDNode* md;
        if (interpMode >= 0) {
            llvm::Value* layoutArgs[] = {
                gla::MakeIntConstant(context, layout),
                gla::MakeIntConstant(context, precision),
                gla::MakeIntConstant(context, location),
                sampler,
                gla::MakeIntConstant(context, interpMode)
            };
            md = llvm::MDNode::get(context, layoutArgs);
        } else {
            llvm::Value* layoutArgs[] = {
                gla::MakeIntConstant(context, layout),
                gla::MakeIntConstant(context, precision),
                gla::MakeIntConstant(context, location),
                sampler,
            };
            md = llvm::MDNode::get(context, layoutArgs);
        }

        return md;
    }

    void addMdEntrypoint(const char* name)
    {
        llvm::MDNode* md;
        llvm::Value* args[] = {
            llvm::MDString::get(context, name),
            gla::MakeIntConstant(context, EMioEntrypoint),
        };
        md = llvm::MDNode::get(context, args);

        llvm::NamedMDNode* namedNode = module->getOrInsertNamedMetadata(EntrypointListMdName);
        namedNode->addOperand(md);
    }

    void addNoStaticUse(llvm::MDNode* md)
    {
        llvm::NamedMDNode* namedNode = module->getOrInsertNamedMetadata(NoStaticUseMdName);
        namedNode->addOperand(md);
    }        

protected:
    llvm::LLVMContext& context;
    llvm::Module* module;
    llvm::MDNode* precisionMd[EMpCount];
};

};

#endif // metadata_H
