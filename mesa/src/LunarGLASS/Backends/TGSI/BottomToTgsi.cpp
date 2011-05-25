//===- BottomToTgsi.cpp - Translate bottom IR to Tgsi ---------------------===//
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
// Usable by the bottom translator to create mesa IR.
//
// Note:  Modeled after ir_to_mesa_visitor in
//        mesa/src/mesa/program/ir_to_mesa.cpp
//
//===----------------------------------------------------------------------===//

#include <cstdio>
#include <string>
#include <map>
#include <vector>

// LunarGLASS includes
#include "Exceptions.h"
#include "Util.h"
#include "BottomIR.h"
#include "Backend.h"
#include "PrivateManager.h"
#include "TgsiTarget.h"

// Mesa includes
extern "C" {
#include "mtypes.h"
#include "program/prog_instruction.h"
#include "program/prog_print.h"
#include "program/prog_optimize.h"
}

class TgsiBackEnd : public gla::BackEnd {
public:
    TgsiBackEnd() { }
    virtual ~TgsiBackEnd() { }

    virtual void getRegisterForm(int& outerSoA, int& innerAoS)
    {
        gla::BackEnd::getRegisterForm(outerSoA, innerAoS);
    }

    virtual void getControlFlowMode(gla::EFlowControlMode& flowControlMode,
                                    bool& breakOp, bool& continueOp,
                                    bool& earlyReturnOp, bool& discardOp)
    {
        gla::BackEnd::getControlFlowMode(flowControlMode, breakOp, continueOp,
                                         earlyReturnOp, discardOp);
    }
};

gla::BackEnd* gla::GetTgsiBackEnd()
{
    return new TgsiBackEnd();
}

void gla::ReleaseTgsiBackEnd(gla::BackEnd* backEnd)
{
    delete backEnd;
}

gl_program* LunarGLASSNewMesaProgram(struct gl_context *ctx, GLenum target, GLuint id)
{
    // gl_fragment_program emulates inheritance by having "base" be
    // its first member

    gl_fragment_program* fragmentProgram = static_cast<gl_fragment_program*>(calloc(1, sizeof(gl_fragment_program)));
    gl_program* prog = &fragmentProgram->Base;

    prog->Target = target;

    return prog;
}

namespace gla {
    class MesaTarget;
};

class gla::MesaTarget : public gla::BackEndTranslator {
public:
    MesaTarget()
    {
        // Initial creation of target.

        // In the real driver, this is is done through a call through the function
        // pointer ctx->Driver.NewProgram(...), so directly call a funtion here
        // that does the same thing, and could later be plugged into that pointer.

        // GLcontext *ctx = 0;
        GLenum target = 0;  //?? need to track original stage through LLVM
        GLuint id = 0;
        maxMesaInstructions = 500;   //?? have to know this number ahead of time, need to refine this
        mesaProgram = LunarGLASSNewMesaProgram(0, target, id);

        mesaInstructions = (struct prog_instruction *)calloc(maxMesaInstructions, sizeof(*mesaInstructions));
        mesaInstruction = mesaInstructions;
        for (int file = 0; file < PROGRAM_FILE_MAX; ++file)
            lastIndex[file] = 0;
    }

    ~MesaTarget()
    {
        // don't free instructions, as they needs to live on
    }

    void startFunctionDeclaration(const llvm::Type*, const std::string&) { }
    void addArgument(const llvm::Value*, bool last) { }
    void endFunctionDeclaration() { }
    void startFunctionBody() { }
    void endFunctionBody() { }

    void add(const llvm::Instruction* llvmInstruction, bool lastBlock);

    //
    // Motivated by need to convert to structured flow control and
    // eliminate phi functions.
    //
    void addIf(const llvm::Value* cond, bool invert=false)
    {
        if (invert)
            UnsupportedFunctionality("condition inversion");

        mesaInstruction->Opcode = OPCODE_IF;
        assert(_mesa_num_inst_src_regs(OPCODE_IF) == 1);
        mapGlaOperand(cond, &mesaInstruction->SrcReg[0]);
        incrementMesaInstruction();
    }

    void addElse()
    {
        mesaInstruction->Opcode = OPCODE_ELSE;
        incrementMesaInstruction();
    }

    void addEndif()
    {
        mesaInstruction->Opcode = OPCODE_ENDIF;
        incrementMesaInstruction();
    }

    void beginConditionalLoop()
    {
        gla::UnsupportedFunctionality("Loops");
    }

    void beginSimpleConditionalLoop(const llvm::CmpInst* cmp, const llvm::Value* op1, const llvm::Value* op2, bool invert=false)
    {
        gla::UnsupportedFunctionality("simple conditional loops");
    }


    void beginSimpleInductiveLoop(const llvm::PHINode* phi, unsigned count)
    {
        UnsupportedFunctionality("inductive loops");
    }

    void beginInductiveLoop()
    {
        gla::UnsupportedFunctionality("Loops");
    }

    void beginLoop()
    {
        gla::UnsupportedFunctionality("Loops");
    }

    void endLoop()
    {
        gla::UnsupportedFunctionality("Loops");
    }

    void addLoopExit(const llvm::Value* condition=NULL, bool invert=false)
    {
        gla::UnsupportedFunctionality("break");
    }

    void addLoopBack(const llvm::Value* condition=NULL, bool invert=false)
    {
        gla::UnsupportedFunctionality("continue");
    }

    void addDiscard()
    {
        gla::UnsupportedFunctionality("discard");
    }


    void addPhiCopy(const llvm::Value* dst, const llvm::Value* src)
    {
        mesaInstruction->Opcode = OPCODE_MOV;
        mapGlaOperand(src, &mesaInstruction->SrcReg[0]);
        mapGlaDestination(dst, &mesaInstruction->DstReg);
        incrementMesaInstruction();
    }

    void print();

protected:

    int getNumMesaInstructions() const
    {
        return mesaInstruction - mesaInstructions;
    }

    prog_instruction* getMesaInstructions() const
    {
        return mesaInstructions;
    }

    void mapGlaIntrinsic(const llvm::IntrinsicInst* llvmInstruction);

    void incrementMesaInstruction()
    {
        // set IndirectRegisterFiles before moving on...
        if (mesaInstruction->DstReg.RelAddr)
            mesaProgram->IndirectRegisterFiles |= 1 << mesaInstruction->DstReg.File;

        for (unsigned src = 0; src < 3; src++)
            if (mesaInstruction->SrcReg[src].RelAddr)
                mesaProgram->IndirectRegisterFiles |= 1 << mesaInstruction->SrcReg[src].File;

        // make sure we aren't running out of space
        assert(getNumMesaInstructions() < maxMesaInstructions);

        ++mesaInstruction;
    }

    gl_register_file mapGlaAddressSpace(const llvm::Value* value)
    {
        if (const llvm::PointerType* pointer = llvm::dyn_cast<llvm::PointerType>(value->getType())) {
            switch (pointer->getAddressSpace()) {
            case gla::UniformAddressSpace:
                return PROGRAM_UNIFORM;
            case gla::GlobalAddressSpace:
                return PROGRAM_TEMPORARY;
            default:
                UnsupportedFunctionality("address space in Bottom IR");
            }
        }

        if (llvm::isa<llvm::Constant>(value)) {
            return PROGRAM_CONSTANT;
        }

        return PROGRAM_TEMPORARY;
    }

    void mapGlaOperand(const llvm::Value* value, prog_src_register *mesaRegister)
    {
        mesaRegister->File = mapGlaAddressSpace(value);
        mesaRegister->Index = getValueIndex(mesaRegister->File, value);
        mesaRegister->Swizzle = mapGlaComponentCountSwizzle(value);
        mesaRegister->RelAddr = 0;
        mesaRegister->Abs = 0;
        mesaRegister->Negate = NEGATE_NONE;
        mesaRegister->HasIndex2 = 0;
        mesaRegister->RelAddr2 = 0;
        mesaRegister->Index2 = 0;
    }

    void mapGlaDestination(const llvm::Value* value, prog_dst_register *mesaRegister)
    {
        mesaRegister->File = mapGlaAddressSpace(value);
        mesaRegister->Index = getValueIndex(mesaRegister->File, value);
        mesaRegister->CondMask = COND_TR;
        mesaRegister->RelAddr = 0;
        mesaRegister->CondSwizzle = SWIZZLE_XYZW;
        mesaRegister->CondSrc = 0;

        mesaRegister->WriteMask = mapGlaComponentCountWriteMask(value);
    }

    GLuint mapGlaComponentCountSwizzle(const llvm::Value* value)
    {
        const llvm::Type* type = value->getType();
        const llvm::PointerType *pointerType = llvm::dyn_cast<llvm::PointerType>(type);
        if (pointerType) {
            // dereference, it's what its pointing to that matters for swizzling
            type = pointerType->getContainedType(0);
        }

        return mapComponentCountSwizzle(GetComponentCount (type));
    }

    GLuint mapComponentCountSwizzle(int numComponents)
    {
        switch (numComponents) {
        case 1:   return MAKE_SWIZZLE4(SWIZZLE_X, SWIZZLE_X, SWIZZLE_X, SWIZZLE_X);
        case 2:   return MAKE_SWIZZLE4(SWIZZLE_X, SWIZZLE_Y, SWIZZLE_Y, SWIZZLE_Y);
        case 3:   return MAKE_SWIZZLE4(SWIZZLE_X, SWIZZLE_Y, SWIZZLE_Z, SWIZZLE_Z);
        case 4:   return MAKE_SWIZZLE4(SWIZZLE_X, SWIZZLE_Y, SWIZZLE_Z, SWIZZLE_W);
        default:
            UnsupportedFunctionality("Vector with more than 4 components in Bottom IR: ", numComponents);
        }

        return SWIZZLE_XYZW;
    }

    GLuint mapComponentSwizzle(int component)
    {
        switch (component) {
        case 0: return SWIZZLE_XXXX;
        case 1: return SWIZZLE_YYYY;
        case 2: return SWIZZLE_ZZZZ;
        case 3: return SWIZZLE_WWWW;
        default:
            UnsupportedFunctionality("Component to high in Bottom IR: ", component);
        }

        return SWIZZLE_XXXX;
    }

    GLuint mapGlaComponentCountWriteMask(const llvm::Value* value)
    {
        switch (GetComponentCount(value)) {
        case 1:   return WRITEMASK_X;
        case 2:   return WRITEMASK_XY;
        case 3:   return WRITEMASK_XYZ;
        case 4:   return WRITEMASK_XYZW;
        default:
            UnsupportedFunctionality("Vector with more than 4 components in Bottom IR: ", GetComponentCount(value));
        }

        return WRITEMASK_X;
    }

    GLuint mapComponentWriteMask(int component)
    {
        switch (component) {
        case 0:   return WRITEMASK_X;
        case 1:   return WRITEMASK_Y;
        case 2:   return WRITEMASK_Z;
        case 3:   return WRITEMASK_W;
        default:
            UnsupportedFunctionality("Component to high in Bottom IR: ", component);
        }

        return WRITEMASK_X;
    }

    GLuint mapGlaSamplerType(const llvm::Value* samplerType, int flags)
    {
        gla::ETextureFlags texFlags = *(gla::ETextureFlags*)&flags;

        switch(GetConstantInt(samplerType)) {
        case ESampler1D:        return (texFlags.EArrayed) ? TEXTURE_1D_ARRAY_INDEX : TEXTURE_1D_INDEX;
        case ESampler2D:        return (texFlags.EArrayed) ? TEXTURE_2D_ARRAY_INDEX : TEXTURE_2D_INDEX;
        case ESampler3D:        return TEXTURE_3D_INDEX;
        case ESamplerCube:      return TEXTURE_CUBE_INDEX;
        case ESampler2DRect:    return TEXTURE_RECT_INDEX;
        default:
            UnsupportedFunctionality("sampler type in Bottom IR");
        }

        return TEXTURE_2D_INDEX;
    }

    prog_opcode getMesaOpFromGlaInst(const llvm::IntrinsicInst* llvmInstruction, int FlagLocOpIndex)
    {
        // Check flags for proj/lod/offset
        int flags = GetConstantInt(llvmInstruction->getOperand(FlagLocOpIndex));

        gla::ETextureFlags texFlags = *(gla::ETextureFlags*)&flags;

        if (texFlags.EProjected)
            return OPCODE_TXP;
        else if (texFlags.EBias)
            return OPCODE_TXB;
        else if (texFlags.ELod)
            return OPCODE_TXL;

        if(IsGradientTexInst(llvmInstruction))
            return OPCODE_TXD;

        return OPCODE_TEX;
    }

    int newIndex(GLuint file)
    {
        ++lastIndex[file];
        assert(lastIndex[file] < (1 << INST_INDEX_BITS));

        return lastIndex[file];
    }

    int getValueIndex(GLuint file, const llvm::Value* value)
    {
        int index = valueMap[file][value];

        if (index == 0) {
            index = newIndex(file);
            valueMap[file][value] = index;
        }

        return index;
    }

    int mapGlaSwizzle(int glaSwizzle)
    {
        // Hard coded to four wide AOS vector for graphics
        int components[4];

        // Pull each two bit channel out of the integer
        for(int i = 0; i < 4; i++)
            components[i] = (glaSwizzle >> i*2) & 0x3;

        return MAKE_SWIZZLE4(components[0], components[1], components[2], components[3]);
    }

    struct gl_program *mesaProgram;
    struct prog_instruction *mesaInstructions;
    struct prog_instruction *mesaInstruction;
    int maxMesaInstructions;

    // this block used only temporarily to map operands/destinations between IRs
    prog_opcode mesaOp;
    int destFromArg;
    int operandFrom[4];  // current max of 4 operands in mesa  ?? is there a define or const for this?

    // mapping from LLVM values to mesa IR indexes, per file type
    std::map<const llvm::Value*, int> valueMap[PROGRAM_FILE_MAX];
    int lastIndex[PROGRAM_FILE_MAX];  //?? currently skipping index 0, because 0 means not found in the map
};

//
// Factory for TGSI translator
//
gla::BackEndTranslator* gla::GetTgsiTranslator()
{
    return new gla::MesaTarget();
}

void gla::ReleaseTgsiTranslator(gla::BackEndTranslator* target)
{
    delete target;
}

//
// Add an LLVM instruction to the end of the mesa instructions.
//
void gla::MesaTarget::add(const llvm::Instruction* llvmInstruction, bool lastBlock)
{
    // create a map for the typcial case, overridden as necessary
    mesaOp = OPCODE_NOP;
    destFromArg = -1;
    operandFrom[0] = 0;
    operandFrom[1] = 1;
    operandFrom[2] = 2;
    operandFrom[3] = 3;

    int temp; // to hold an intermediate (temporary) register
    switch (llvmInstruction->getOpcode()) {
    case llvm::Instruction::FAdd:           mesaOp = OPCODE_ADD;  break;
    case llvm::Instruction::FSub:           mesaOp = OPCODE_SUB;  break;
    case llvm::Instruction::FMul:           mesaOp = OPCODE_MUL;  break;

    // if it's main, we want an END, if it's a function, we want a RET
    // ?? handle functions that aren't main
    case llvm::Instruction::Ret:            mesaOp = OPCODE_END;  break;

    case llvm::Instruction::FDiv:

        // First, take the reciprocal.  This is done per component, but all landing in the
        // same temporary vector.
        temp = newIndex(PROGRAM_TEMPORARY);
        for (int c = 0; c < GetComponentCount(llvmInstruction->getOperand(1)); ++c) {
            mesaInstruction->Opcode = OPCODE_RCP;
            mapGlaOperand(llvmInstruction->getOperand(1), &mesaInstruction->SrcReg[0]);
            mesaInstruction->SrcReg[0].Swizzle = mapComponentSwizzle(c);
            // copy the characteristics of the divisor for the result of the reciprocal
            mapGlaDestination(llvmInstruction->getOperand(1), &mesaInstruction->DstReg);
            mesaInstruction->DstReg.Index = temp;
            mesaInstruction->DstReg.WriteMask = mapComponentWriteMask(c);
            incrementMesaInstruction();
        }

        // Second, do a multiply.
        mesaInstruction->Opcode = OPCODE_MUL;
        mapGlaOperand(llvmInstruction->getOperand(0), &mesaInstruction->SrcReg[0]);
        mapGlaOperand(llvmInstruction->getOperand(1), &mesaInstruction->SrcReg[1]);
        mesaInstruction->SrcReg[1].Index = temp;
        mapGlaDestination(llvmInstruction, &mesaInstruction->DstReg);
        incrementMesaInstruction();

        return;

    case llvm::Instruction::Call: // includes intrinsics...
        if (const llvm::IntrinsicInst* i = llvm::dyn_cast<llvm::IntrinsicInst>(llvmInstruction)) {
            mapGlaIntrinsic(i);
            if (mesaOp == OPCODE_NOP)
                return;
        } else {
            UnsupportedFunctionality("call, non-intrinsic in Bottom IR");
        }
        break;

    case llvm::Instruction::Load:
        if (llvm::isa<llvm::PointerType>(llvmInstruction->getOperand(0)->getType())) {
            mesaOp = OPCODE_MOV;
        } else {
            assert(! "Load instruction is not through pointer\n");
        }
        break;

    case llvm::Instruction::Alloca:
        UnsupportedFunctionality("stack allocation in Bottom IR");
        break;

    case llvm::Instruction::Store:
        if (llvm::isa<llvm::PointerType>(llvmInstruction->getOperand(1)->getType())) {
            mesaOp = OPCODE_MOV;
            destFromArg = 1;
        } else {
            assert(! "Store instruction is not through pointer\n");
        }
        break;

    case llvm::Instruction::PHI:
        // this got turned into copies in predecessors
        return;

    // Comparison
    case llvm::Instruction::FCmp:
        if (const llvm::FCmpInst* fcmp = llvm::dyn_cast<llvm::FCmpInst>(llvmInstruction)) {
            switch (fcmp->getPredicate()) {
            case llvm::FCmpInst::FCMP_OGT:
                mesaOp = OPCODE_SGT;
                break;
            case llvm::FCmpInst::FCMP_OEQ:
                mesaOp = OPCODE_SEQ;
                break;
            case llvm::FCmpInst::FCMP_OGE:
                mesaOp = OPCODE_SGE;
                break;
            case llvm::FCmpInst::FCMP_OLT:
                mesaOp = OPCODE_SLT;
                break;
            case llvm::FCmpInst::FCMP_OLE:
                mesaOp = OPCODE_SLE;
                break;
            case llvm::FCmpInst::FCMP_ONE:
                mesaOp = OPCODE_SNE;
                break;
            default:
                UnsupportedFunctionality("comparison operator in Bottom IR: ", fcmp->getPredicate());
            }
        }
        else {
            assert(! "FCmp instruction found that cannot dyncast to FCmpInst");
        }
        break;

    default:
        UnsupportedFunctionality("opcode in Bottom IR: ", llvmInstruction->getOpcode());
    }

    //??mesaInstruction->CondUpdate = inst->cond_update;

    // op code

    mesaInstruction->Opcode = mesaOp;

    // destination
    if (destFromArg >= 0)
        mapGlaDestination(llvmInstruction->getOperand(destFromArg), &mesaInstruction->DstReg);
    else
        mapGlaDestination(llvmInstruction, &mesaInstruction->DstReg);

    // operands
    for (unsigned int opNum = 0; opNum < _mesa_num_inst_src_regs(mesaOp); ++opNum) {
        mapGlaOperand(llvmInstruction->getOperand(operandFrom[opNum]), &mesaInstruction->SrcReg[opNum]);
    }

    //switch (mesaInstruction->Opcode) {
    //case OPCODE_BGNSUB:
    //    inst->function->inst = i;
    //    mesaInstruction->Comment = strdup(inst->function->sig->function_name());
    //    break;
    //case OPCODE_ENDSUB:
    //    mesaInstruction->Comment = strdup(inst->function->sig->function_name());
    //    break;
    //case OPCODE_CAL:
    //    mesaInstruction->BranchTarget = inst->function->sig_id; /* rewritten later */
    //    break;
    //case OPCODE_ARL:
    //    prog->NumAddressRegs = 1;
    //    break;
    //default:
    //    break;
    //}

    incrementMesaInstruction();
}

void gla::MesaTarget::print()
{
    mesaProgram->Instructions = getMesaInstructions();
    mesaProgram->NumInstructions = getNumMesaInstructions();

    //do_set_program_inouts(shader->ir, prog);
    //count_resources(prog);

    //_mesa_reference_program(ctx, &shader->Program, prog);

    //if ((ctx->Shader.Flags & GLSL_NO_OPT) == 0) {
    //    _mesa_optimize_program(ctx, prog);
    //}

    printf("\nMesa program before mesa optimizations\n");
    _mesa_fprint_program_opt(stdout, mesaProgram, PROG_PRINT_DEBUG, true);

    _mesa_optimize_program(0, mesaProgram);

    printf("\nMesa program after mesa optimizations\n");
    _mesa_fprint_program_opt(stdout, mesaProgram, PROG_PRINT_DEBUG, true);
}

//
// Handle the subcase of an LLVM instruction being an intrinsic call.
//
void gla::MesaTarget::mapGlaIntrinsic(const llvm::IntrinsicInst* llvmInstruction)
{
    mesaOp = OPCODE_NOP;

    // Handle pipeline read/write
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_fWriteData:
        mesaInstruction->Opcode = OPCODE_MOV;
        mapGlaOperand(llvmInstruction->getOperand(1), &mesaInstruction->SrcReg[0]);
        mapGlaDestination(llvmInstruction->getOperand(1), &mesaInstruction->DstReg);
        mesaInstruction->DstReg.File = PROGRAM_OUTPUT;
        mesaInstruction->DstReg.Index = GetConstantInt(llvmInstruction->getOperand(0));
        incrementMesaInstruction();
        mesaOp = OPCODE_NOP;
        return;
    case llvm::Intrinsic::gla_readData:
    case llvm::Intrinsic::gla_fReadInterpolant:
        mesaInstruction->Opcode = OPCODE_MOV;
        mapGlaDestination(llvmInstruction, &mesaInstruction->DstReg);
        mesaInstruction->SrcReg[0].File = PROGRAM_INPUT;
        mesaInstruction->SrcReg[0].Index = GetConstantInt(llvmInstruction->getOperand(0));
        mesaInstruction->SrcReg[0].Swizzle = mapGlaComponentCountSwizzle(llvmInstruction);
        incrementMesaInstruction();
        mesaOp = OPCODE_NOP;
        return;
    }

    // Handle texturing
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_fTextureSample:
    case llvm::Intrinsic::gla_fTextureSampleLod:
    case llvm::Intrinsic::gla_fTextureSampleLodOffset:
    case llvm::Intrinsic::gla_fTextureSampleLodOffsetGrad:
        //TODO:  Mesa expects proj/bias/lod to be in coord.w channel.  This is not implemented yet.
        mesaInstruction->TexSrcTarget = mapGlaSamplerType(llvmInstruction->getOperand(0),
                                                          GetConstantInt(llvmInstruction->getOperand(GetTextureOpIndex(ETOFlag))));
        mesaInstruction->TexShadow    = 0;   // ?? may be only for the shader to generate the compare itself
        mesaInstruction->TexSrcUnit   = 17;  // ?? may be a linker-created slot number for the sampler

        operandFrom[0] = GetTextureOpIndex(ETOCoord);

        if(IsGradientTexInst(llvmInstruction)) {
            operandFrom[1] = GetTextureOpIndex(ETODPdx);
            operandFrom[2] = GetTextureOpIndex(ETODPdy);
            operandFrom[3] = GetTextureOpIndex(ETOSamplerLoc);
        }
        else {
            operandFrom[1] = GetTextureOpIndex(ETOSamplerLoc);
        }

        mesaOp = getMesaOpFromGlaInst(llvmInstruction, GetTextureOpIndex(ETOFlag));
        return;
    }

    // Handle swizzles
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_swizzle:
    case llvm::Intrinsic::gla_fSwizzle:
        mesaInstruction->Opcode = OPCODE_MOV;
        mapGlaOperand(llvmInstruction->getOperand(0), &mesaInstruction->SrcReg[0]);
        mapGlaDestination(llvmInstruction, &mesaInstruction->DstReg);

        // GLA uses 2 bits per channel, Mesa uses 3...
        int glaSwizzle = GetConstantInt(llvmInstruction->getOperand(1));
        mesaInstruction->SrcReg[0].Swizzle = mapGlaSwizzle(glaSwizzle);

        incrementMesaInstruction();
        mesaOp = OPCODE_NOP;
        return;
    }

    // Handle the one-to-one mappings
    switch (llvmInstruction->getIntrinsicID()) {
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_ABS;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_ADD;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_AND;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_ARA;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_ARL;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_ARL_NV; break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_ARR;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_BGNLOOP;break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_BGNSUB; break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_BRA;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_BRK;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_CAL;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_CMP;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_CONT;   break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_COS;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_DDX;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_DDY;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_DP2;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_DP2A;   break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_DP3;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_DP4;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_DPH;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_DST;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_ELSE;   break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_EMIT_VERTEX;   break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_END;           break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_END_PRIMITIVE; break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_ENDIF;         break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_EX2;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_EXP;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_FLR;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_FRC;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_IF;     break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_KIL;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_KIL_NV; break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_LG2;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_LIT;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_LOG;    break;

    case llvm::Intrinsic::gla_fMix:
        operandFrom[0] = 2;
        operandFrom[1] = 0;
        operandFrom[2] = 1;
        mesaOp = OPCODE_LRP;
        break;

    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_MAD;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_MAX;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_MIN;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_MOV;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_MUL;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_NOISE1; break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_NOISE2; break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_NOISE3; break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_NOISE4; break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_NOT;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_NRM3;   break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_NRM4;   break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_OR;     break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_PK2H;   break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_PK2US;  break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_PK4B;   break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_PK4UB;  break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_POW;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_POPA;   break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_PRINT;  break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_PUSHA;  break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_RCC;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_RET;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_RFL;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_RSQ;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_SCS;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_SEQ;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_SFL;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_SGE;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_SGT;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_SIN;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_SLE;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_SLT;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_SNE;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_SSG;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_STR;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_SUB;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_SWZ;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_TEX;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_TXB;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_TXD;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_TXL;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_TXP;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_TXP_NV; break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_TRUNC;  break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_UP2H;   break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_UP2US;  break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_UP4B;   break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_UP4UB;  break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_X2D;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_XOR;    break;
    //case llvm::Intrinsic::LunarGLASS_XXX:    mesaOp = OPCODE_XPD;    break;
    }

    if (mesaOp == OPCODE_NOP)
        UnsupportedFunctionality("intrinsic in Bottom IR");
}
