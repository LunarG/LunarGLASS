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

// LunarGLASS includes
#include "LunarGLASSTopIR.h"

// LLVM includes
#include "llvm/Module.h"
#include "llvm/Metadata.h"

namespace gla {

// Forms of metadata nodes, pointed to:
//
//     input/output/uniform -> { name, EMdInputOutput,  Value*, typeLayout, aggregateLayout }
//     Notes:
//         - Value* is a proxy for getting the LLVM type
//         - aggregateLayout == 0 if not in a block
//
//     sampler -> { name, EMdSampler, Value*, EMdSamplerDim, array, shadow }
//     Notes:
//         - texel return value has the same type as Value*
//         - array is bool, true if it is a samplerArray
//         - shadow is bool, true if it is a samplerShadow
//
//     typeLayout -> { EMdTypeLayout, location }
//     Notes:
//         - the type layout is how it is known if something is a matrix or unsigned integer, 
//           as this is not in the LLVM type
//         - location is the *first* location of the variable, which can span many slots/locations
//         - location is >= MaxUserLayoutLocation for non-user specified locations, to be patched
//           later by a linker
//         - location is < MaxUserLayoutLocation for user-assigned locations
//         - the intrinsic's slot can be different when reading a single slot out of the middle of a large input/output
//
//     aggregateLayout name, number instances, EMdAggregateLayout, list of typeLayout with nested structures flattened
//         - TODO: linker: the aggregateLayout form is not yet being generated
//
// Forms of named metadata
//
//     "inputs" -> { list of all pipeline inputs }
//     "outputs" -> { list of all pipeline outputs }
//     "defaultUniforms" -> { list of all uniforms not in blocks }
//     "samplers" -> { list of all samplers }
//

const int MaxUserLayoutLocation = 1024;

// what kind of pipeline I/O:
enum EMdInputOutput {
    EMioNone,               // for something that is not I/O, should not show up in a metadata node

    // inputs
    EMioPipeOut,            // normal user-streamed input data: attributes, varyings, etc.
    EMioVertexId,
    EMioInstanceId,
    EMioFragmentFace,
    EMioFragmentCoord,
    EMioPointCoord,

    // outputs
    EMioPipeIn,             // normal user-streamed output data, including fragment color
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
};

// what kind of uniform:
enum EMdUniform {
};

// How a block or structure is laid out
enum EMdAggregateLayout {
    EMblShared,
    EMblStd140,
    EMblStd430,
    EMblPacked,
};

enum EMdSampler {
    EMsTexture,
    EMsImage,
};

enum EMdSamplerDim {
    EMsd1D,
    EMsd2D,
    EMsd3D,
    EMsdCube,
    EMsdRect,
    EMsdBuffer,
};

class Metadata {
public:
    Metadata(llvm::LLVMContext& c, llvm::Module* m) : context(c), module(m) { }

    // "input/output/uniform ->" as per comment at top of file
    llvm::MDNode* makeMdInputOutput(llvm::StringRef symbolName, llvm::StringRef sectionName, EMdInputOutput qualifier, 
                                    llvm::Value* typeProxy, EMdTypeLayout layout, int location, llvm::MDNode* aggregate = 0)
    {
        llvm::Value* args[] = {
            llvm::MDString::get(context, symbolName),
            gla::MakeIntConstant(context, qualifier),
            typeProxy,
            makeMdTypeLayout(layout, location)
        };
        llvm::MDNode* md = llvm::MDNode::get(context, args);

        llvm::NamedMDNode* namedNode = module->getOrInsertNamedMetadata(sectionName);
        namedNode->addOperand(md);

        return md;
    }

    // "sampler ->" as per comment at top of file
    llvm::MDNode* makeMdSampler(llvm::StringRef symbolName, EMdSampler sampler, llvm::Value* typeProxy, EMdSamplerDim dim, bool isArray, bool isShadow)
    {
        llvm::Value* args[] = {
            llvm::MDString::get(context, symbolName),
            gla::MakeIntConstant(context, sampler),
            typeProxy,
            gla::MakeIntConstant(context, dim),
            gla::MakeBoolConstant(context, isArray),
            gla::MakeBoolConstant(context, isShadow),
        };
        llvm::MDNode* md = llvm::MDNode::get(context, args);

        llvm::NamedMDNode* namedNode = module->getOrInsertNamedMetadata("samplers");
        namedNode->addOperand(md);

        return md;
    }

protected:
    llvm::MDNode* makeMdTypeLayout(EMdTypeLayout layout, int location)
    {
        llvm::Value* layoutArgs[] = {
            gla::MakeIntConstant(context, layout),
            gla::MakeIntConstant(context, location),
        };

        return llvm::MDNode::get(context, layoutArgs);
    }

    llvm::LLVMContext& context;
    llvm::Module* module;
};

};
