//===- BottomToTgsi.h - Translate bottom IR to TGSI -----------------------===//
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
// Translate bottom IR to TGSI by doing a manual traversal of the LLVM
// while building up the mesa IR.
//
// Note:  modeling after LLVM/llvm-2.8/lib/VMCore/AsmWriter.cpp
//        and ir_to_mesa_visitor in mesa/src/mesa/program/ir_to_mesa.cpp
//
//===----------------------------------------------------------------------===//

// LLVM includes
#include "llvm/DerivedTypes.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/Intrinsics.h"

#include <cstdio>
#include <string>
#include <map>
#include <vector>

// LunarGLASS includes
#include "LunarGLASSBottomIR.h"
#include "Manager.h"

// Mesa includes
extern "C" {
#include "mtypes.h"
#include "program/prog_instruction.h"
#include "program/prog_print.h"
#include "program/prog_optimize.h"
}

class TgsiBackEnd : public gla::BackEnd {
public:
    TgsiBackEnd() {}
    virtual ~TgsiBackEnd() {};

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

gla::BackEnd* gla::getBackEnd()
{
    return new TgsiBackEnd();
}

gl_program* LunarGLASSNewMesaProgram(GLcontext *ctx, GLenum target, GLuint id)
{
    // gl_fragment_program emulates inheritance by having "base" be
    // its first member

    gl_fragment_program* fragmentProgram = static_cast<gl_fragment_program*>(calloc(1, sizeof(gl_fragment_program)));
    gl_program* prog = &fragmentProgram->Base;

    prog->Target = target;

    return prog;
}

namespace gla {
    class BottomTranslator;

    enum EControlFlow {
        EFCElse,
        EFCEndIf
    };
};

class gla::BottomTranslator {
public:
    BottomTranslator(struct gl_program *p, int m) : mesaProgram(p), maxMesaInstructions(m)
    {
        mesaInstructions = (struct prog_instruction *)calloc(maxMesaInstructions, sizeof(*mesaInstructions));
        mesaInstruction = mesaInstructions;
        for (int file = 0; file < PROGRAM_FILE_MAX; ++file)
            lastIndex[file] = 0;
    }

    ~BottomTranslator()
    {
        // don't free instructions, as they needs to live on
    }

    int getNumMesaInstructions() const
    {
        return mesaInstruction - mesaInstructions;
    }

    prog_instruction* getMesaInstructions() const
    {
        return mesaInstructions;
    }

    //
    // Add an LLVM instruction to the end of the mesa instructions.
    //
    void add(const llvm::Instruction* llvmInstruction)
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
            for (int c = 0; c < getGlaComponentCount(llvmInstruction->getOperand(1)); ++c) {
                mesaInstruction->Opcode = OPCODE_RCP;
                mapGlaOperandToMesa(llvmInstruction->getOperand(1), &mesaInstruction->SrcReg[0]);
                mesaInstruction->SrcReg[0].Swizzle = mapComponentToMesaSwizzle(c);
                // copy the characteristics of the divisor for the result of the reciprocal
                mapGlaDestinationToMesa(llvmInstruction->getOperand(1), &mesaInstruction->DstReg);
                mesaInstruction->DstReg.Index = temp;
                mesaInstruction->DstReg.WriteMask = mapComponentToMesaWriteMask(c);
                incrementMesaInstruction();
            }

            // Second, do a multiply.
            mesaInstruction->Opcode = OPCODE_MUL;
            mapGlaOperandToMesa(llvmInstruction->getOperand(0), &mesaInstruction->SrcReg[0]);
            mapGlaOperandToMesa(llvmInstruction->getOperand(1), &mesaInstruction->SrcReg[1]);
            mesaInstruction->SrcReg[1].Index = temp;
            mapGlaDestinationToMesa(llvmInstruction, &mesaInstruction->DstReg);
            incrementMesaInstruction();

            return;

        case llvm::Instruction::Call: // includes intrinsics...
            if (const llvm::IntrinsicInst* i = llvm::dyn_cast<llvm::IntrinsicInst>(llvmInstruction)) {
                mapGlaIntrinsicToMesa(i);
                if (mesaOp == OPCODE_NOP)
                    return;
            } else {
                assert(! "Unsupported call (non-intrinsic)");
            }
            break;

        case llvm::Instruction::Load:
            if (const llvm::PointerType* pointer = llvm::dyn_cast<llvm::PointerType>(llvmInstruction->getOperand(0)->getType())) {
                mesaOp = OPCODE_MOV;
            } else {
                printf("load instruction is not through pointer\n");
            }
            break;

        case llvm::Instruction::Alloca:
            assert(! "Don't handle stack allocations yet");
            break;

        case llvm::Instruction::Store:
            if (const llvm::PointerType* pointer = llvm::dyn_cast<llvm::PointerType>(llvmInstruction->getOperand(1)->getType())) {
                mesaOp = OPCODE_MOV;
                destFromArg = 1;
            } else {
                printf("store instruction is not through pointer\n");
            }
            break;

        case llvm::Instruction::Br:
            addFlowControl(llvmInstruction);
            if (mesaOp == OPCODE_NOP)
                return;
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
                    printf("Undefined (for now) comparison operator used");
                }
            }
            else {
                printf("FCmp instruction found that cannot dyncast to FCmpInst");
            }




        default:
            printf("UNSUPPORTED opcode %d\n", llvmInstruction->getOpcode());
        }

        //??mesaInstruction->CondUpdate = inst->cond_update;

        // op code

        mesaInstruction->Opcode = mesaOp;

        // destination
        if (destFromArg >= 0)
            mapGlaDestinationToMesa(llvmInstruction->getOperand(destFromArg), &mesaInstruction->DstReg);
        else
            mapGlaDestinationToMesa(llvmInstruction, &mesaInstruction->DstReg);

        // operands
        for (int opNum = 0; opNum < _mesa_num_inst_src_regs(mesaOp); ++opNum) {
            mapGlaOperandToMesa(llvmInstruction->getOperand(operandFrom[opNum]), &mesaInstruction->SrcReg[opNum]);
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

    //
    // Handle the subcase of an LLVM instruction being an intrinsic call.
    //
    void mapGlaIntrinsicToMesa(const llvm::IntrinsicInst* llvmInstruction)
    {
        mesaOp = OPCODE_NOP;

        // Handle pipeline read/write
        switch (llvmInstruction->getIntrinsicID()) {
        case llvm::Intrinsic::gla_writeData:
            mesaInstruction->Opcode = OPCODE_MOV;
            mapGlaOperandToMesa(llvmInstruction->getOperand(1), &mesaInstruction->SrcReg[0]);
            mapGlaDestinationToMesa(llvmInstruction->getOperand(1), &mesaInstruction->DstReg);
            mesaInstruction->DstReg.File = PROGRAM_OUTPUT;
            mesaInstruction->DstReg.Index = mapGlaToConstant(llvmInstruction->getOperand(0));
            incrementMesaInstruction();
            mesaOp = OPCODE_NOP;
            return;
        case llvm::Intrinsic::gla_getInterpolant:
            mesaInstruction->Opcode = OPCODE_MOV;
            mapGlaDestinationToMesa(llvmInstruction, &mesaInstruction->DstReg);
            mesaInstruction->SrcReg[0].File = PROGRAM_INPUT;
            mesaInstruction->SrcReg[0].Index = mapGlaToConstant(llvmInstruction->getOperand(0));
            mesaInstruction->SrcReg[0].Swizzle = mapGlaComponentCountToMesaSwizzle(llvmInstruction);
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
            mesaInstruction->TexSrcTarget = mapGlaSamplerTypeToMesa(llvmInstruction->getOperand(0));
            mesaInstruction->TexShadow    = 0;   // ?? may be only for the shader to generate the compare itself
            mesaInstruction->TexSrcUnit   = 17;  // ?? may be a linker-created slot number for the sampler

            const int samplerLoc = 1;
            const int flagLoc    = 2;
            const int coordLoc   = 3;
            const int ddxLoc     = 6;
            const int ddyLoc     = 7;

            operandFrom[0] = coordLoc;

            if(isGradientTexInst(llvmInstruction)) {
                operandFrom[1] = ddxLoc;
                operandFrom[2] = ddyLoc;
                operandFrom[3] = samplerLoc;
            }
            else {
                operandFrom[1] = samplerLoc;
            }

            mesaOp = getMesaOpFromGlaInst(llvmInstruction, flagLoc);
            return;
        }

        // Handle swizzles
        switch (llvmInstruction->getIntrinsicID()) {
        case llvm::Intrinsic::gla_fSwizzle:
            mesaInstruction->Opcode = OPCODE_MOV;
            mapGlaOperandToMesa(llvmInstruction->getOperand(0), &mesaInstruction->SrcReg[0]);
            mapGlaDestinationToMesa(llvmInstruction, &mesaInstruction->DstReg);

            // GLA uses 2 bits per channel, Mesa uses 3...
            int glaSwizzle = mapGlaToConstant(llvmInstruction->getOperand(1));
            mesaInstruction->SrcReg[0].Swizzle = mapGlaSwizzleToMesa(glaSwizzle);

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

        assert(mesaOp != OPCODE_NOP);
    }


protected:
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

    int mapGlaToConstant(const llvm::Value* value)
    {
        if (const llvm::Constant* constant = llvm::dyn_cast<llvm::Constant>(value)) {
            if (const llvm::ConstantInt *constantInt = llvm::dyn_cast<llvm::ConstantInt>(constant))
                return constantInt->getValue().getSExtValue();
            //if (const llvm::ConstantFP *constantFP = llvm::dyn_cast<llvm::ConstantFP>(constant))
            //    return constantFP->getValueAPF().convertToFloat();
            else
                assert(!"can't handle non-integer constants");
        }

        assert (!"expected constant");

        return 0;
    }

    gl_register_file mapGlaAddressSpaceToMesa(const llvm::Value* value)
    {
        if (const llvm::PointerType* pointer = llvm::dyn_cast<llvm::PointerType>(value->getType())) {
            switch (pointer->getAddressSpace()) {
            case gla::UniformAddressSpace:
                return PROGRAM_UNIFORM;
            case gla::GlobalAddressSpace:
                return PROGRAM_TEMPORARY;
            default:
                assert(!"Unknown gla address space");
            }
        }

        if (const llvm::Constant* constant = llvm::dyn_cast<llvm::Constant>(value)) {
            return PROGRAM_CONSTANT;
        }

        //??assert (!"Unhandled address space");

        return PROGRAM_TEMPORARY;
    }

    void mapGlaOperandToMesa(const llvm::Value* value, prog_src_register *mesaRegister)
    {
        mesaRegister->File = mapGlaAddressSpaceToMesa(value);
        mesaRegister->Index = getValueIndex(mesaRegister->File, value);
        mesaRegister->Swizzle = mapGlaComponentCountToMesaSwizzle(value);
        mesaRegister->RelAddr = 0;
        mesaRegister->Abs = 0;
        mesaRegister->Negate = NEGATE_NONE;
        mesaRegister->HasIndex2 = 0;
        mesaRegister->RelAddr2 = 0;
        mesaRegister->Index2 = 0;
    }

    void mapGlaDestinationToMesa(const llvm::Value* value, prog_dst_register *mesaRegister)
    {
        mesaRegister->File = mapGlaAddressSpaceToMesa(value);
        mesaRegister->Index = getValueIndex(mesaRegister->File, value);
        mesaRegister->CondMask = COND_TR;
        mesaRegister->RelAddr = 0;
        mesaRegister->CondSwizzle = SWIZZLE_XYZW;
        mesaRegister->CondSrc = 0;

        mesaRegister->WriteMask = mapGlaComponentCountToMesaWriteMask(value);
    }

    GLuint mapGlaComponentCountToMesaSwizzle(const llvm::Value* value)
    {
        const llvm::Type* type = value->getType();
        const llvm::PointerType *pointerType = llvm::dyn_cast<llvm::PointerType>(type);
        if (pointerType) {
            // dereference, it's what its pointing to that matters for swizzling
            type = pointerType->getContainedType(0);
        }

        return mapComponentCountToMesaSwizzle(getGlaComponentCount(type));
    }

    GLuint mapComponentCountToMesaSwizzle(int numComponents)
    {
        switch (numComponents) {
        case 1:   return MAKE_SWIZZLE4(SWIZZLE_X, SWIZZLE_X, SWIZZLE_X, SWIZZLE_X);
        case 2:   return MAKE_SWIZZLE4(SWIZZLE_X, SWIZZLE_Y, SWIZZLE_Y, SWIZZLE_Y);
        case 3:   return MAKE_SWIZZLE4(SWIZZLE_X, SWIZZLE_Y, SWIZZLE_Z, SWIZZLE_Z);
        case 4:   return MAKE_SWIZZLE4(SWIZZLE_X, SWIZZLE_Y, SWIZZLE_Z, SWIZZLE_W);
        default:  assert(!"Vector too large");
        }

        return SWIZZLE_XYZW;
    }

    GLuint mapComponentToMesaSwizzle(int component)
    {
        switch (component) {
        case 0: return SWIZZLE_XXXX;
        case 1: return SWIZZLE_YYYY;
        case 2: return SWIZZLE_ZZZZ;
        case 3: return SWIZZLE_WWWW;
        default:  assert(!"Vector too large");
        }

        return SWIZZLE_XXXX;
    }

    GLuint mapGlaComponentCountToMesaWriteMask(const llvm::Value* value)
    {
        switch (getGlaComponentCount(value)) {
        case 1:   return WRITEMASK_X;
        case 2:   return WRITEMASK_XY;
        case 3:   return WRITEMASK_XYZ;
        case 4:   return WRITEMASK_XYZW;
        default:  assert(!"Vector too large");
        }

        return WRITEMASK_X;
    }

    GLuint mapComponentToMesaWriteMask(int component)
    {
        switch (component) {
        case 0:   return WRITEMASK_X;
        case 1:   return WRITEMASK_Y;
        case 2:   return WRITEMASK_Z;
        case 3:   return WRITEMASK_W;
        default:  assert(!"Vector too large");
        }

        return WRITEMASK_X;
    }

    GLuint mapGlaSamplerTypeToMesa(const llvm::Value* samplerType)
    {
        switch(mapGlaToConstant(samplerType)) {
        case ESampler1D:        return TEXTURE_1D_INDEX;
        case ESampler2D:        return TEXTURE_2D_INDEX;
        case ESampler3D:        return TEXTURE_3D_INDEX;
        case ESamplerCube:      return TEXTURE_CUBE_INDEX;
        case ESampler2DRect:    return TEXTURE_RECT_INDEX;
        case ESampler1DArray:   return TEXTURE_1D_ARRAY_INDEX;
        case ESampler2DArray:   return TEXTURE_2D_ARRAY_INDEX;
        default: assert(!"Unsupported samplerType");
        }

        return TEXTURE_2D_INDEX;
    }

    prog_opcode getMesaOpFromGlaInst(const llvm::IntrinsicInst* llvmInstruction, int flagLoc)
    {
        // Check flags for proj/lod/offset
        int flags = mapGlaToConstant(llvmInstruction->getOperand(flagLoc));

        gla::ETextureFlags texFlags = *(gla::ETextureFlags*)&flags;

        if (texFlags.EProjected)
            return OPCODE_TXP;
        else if (texFlags.EBias)
            return OPCODE_TXB;
        else if (texFlags.ELod)
            return OPCODE_TXL;

        if(isGradientTexInst(llvmInstruction))
            return OPCODE_TXD;

        return OPCODE_TEX;
    }

    static int isGradientTexInst(const llvm::IntrinsicInst* llvmInstruction)
    {
        return ( llvmInstruction->getIntrinsicID() ==
                 llvm::Intrinsic::gla_fTextureSampleLodOffsetGrad );
    }

    int getGlaComponentCount(const llvm::Value* value)
    {
        const llvm::Type* type = value->getType();

        return getGlaComponentCount(type);
    }

    int getGlaComponentCount(const llvm::Type* type)
    {
        const llvm::VectorType *vectorType = llvm::dyn_cast<llvm::VectorType>(type);

        if (vectorType)
            return vectorType->getNumElements();
        else
            return 1;
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

    int mapGlaSwizzleToMesa(int glaSwizzle)
    {
        // Hard coded to four wide AOS vector for graphics
        int components[4];

        // Pull each two bit channel out of the integer
        for(int i = 0; i < 4; i++)
            components[i] = (glaSwizzle >> i*2) & 0x3;

        return MAKE_SWIZZLE4(components[0], components[1], components[2], components[3]);
    }

    void addFlowControl(const llvm::Instruction* llvmInstruction)
    {
        // Translate from LLVM CFG style to structured style.  This is done
        // using a stack to keep track of what is pending.

        // Also, translate from SSA form to non-SSA form (remove phi functions).
        // This is done by looking ahead for phi funtions and adding copies in
        // the phi-predecessor blocks.

        // Currently, this is done in a fragile way, assuming no loops are
        // present, and that LLVM branches must be representing if-then-else
        // constructs.
        mesaOp = OPCODE_NOP;

        switch (llvmInstruction->getNumOperands()) {
        case 1:
            // Assume we are jumping to what the top of the flow-control stack says
            addPhiCopies(llvmInstruction);
            switch (flowControl.back()) {
            case EFCEndIf:
                flowControl.pop_back();
                mesaInstruction->Opcode = OPCODE_ENDIF;
                incrementMesaInstruction();                
                break;
            case EFCElse:
                flowControl.pop_back();
                mesaInstruction->Opcode = OPCODE_ELSE;
                incrementMesaInstruction();
                break;
            default:
                printf ("UNSUPPORTED flow control stack\n");
            }
            break;
        case 2:
            // Assume we are entering an if-then statement
            flowControl.push_back(EFCEndIf);
            mesaOp = OPCODE_IF;
            break;
        case 3:
            // Assume we are entering an if-then-else statement
            flowControl.push_back(EFCEndIf);
            flowControl.push_back(EFCElse);
            mesaOp = OPCODE_IF;
            break;
        default:
            printf ("UNSUPPORTED llvm flow control number of operands\n");
        }
    }

    void addPhiCopies(const llvm::Instruction* llvmInstruction)
    {
        // get the destination block
        const llvm::BasicBlock *phiBlock = llvm::dyn_cast<llvm::BasicBlock>(llvmInstruction->getOperand(0));
        assert (phiBlock);

        // for each llvm phi node, add a copy instruction
        for (llvm::BasicBlock::const_iterator i = phiBlock->begin(), e = phiBlock->end(); i != e; ++i) {
            const llvm::Instruction* destInstruction = i;
            const llvm::PHINode *phiNode = llvm::dyn_cast<llvm::PHINode>(destInstruction);

            if (phiNode) {
                // find the operand whose predecessor is us
                // each phi operand takes up two normal operands
                int predIndex = phiNode->getBasicBlockIndex(llvmInstruction->getParent());
                mesaInstruction->Opcode = OPCODE_MOV;
                mapGlaOperandToMesa(phiNode->getIncomingValue(predIndex), &mesaInstruction->SrcReg[0]);
                mapGlaDestinationToMesa(phiNode, &mesaInstruction->DstReg);
                incrementMesaInstruction();
            }
        }
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

    std::vector<enum EControlFlow> flowControl;
};


void gla::PrivateManager::translateBottomToTgsi()
{
    printf("\n===========================================\n"
           "Starting translation from Bottom IR to TGSI\n\n");

    // Initial creation of target.

    // In the real driver, this is is done through a call through the function
    // pointer ctx->Driver.NewProgram(...), so directly call a funtion here
    // that does the same thing, and could later be plugged into that pointer.

    GLcontext *ctx = 0;
    GLenum target = 0;  //?? need to track original stage through LLVM
    GLuint id = 0;
    struct gl_program *mesaProgram = LunarGLASSNewMesaProgram(0, target, id);
    gla::BottomTranslator translator(mesaProgram, 500);  //?? have to know this number ahead of time, need to refine this

    llvm::Module::const_iterator function, lastFunction;
    for (function = module->begin(), lastFunction = module->end(); function != lastFunction; ++function) {
        if (function->isDeclaration()) {
            //?? do we need to handle declarations of functions, or just definitions?
        } else {
            // handle function's with bodies

            // paramaters and arguments
            for (llvm::Function::const_arg_iterator P = function->arg_begin(), E = function->arg_end();
                P != E; ++P) {
                    //?? argument is Attrs.getParamAttributes(Idx));  // Idx has to count as you go through the loop
            }

            // basic blocks
            for (llvm::Function::const_iterator bb = function->begin(), E = function->end(); bb != E; ++bb) {

                // instructions in the basic block
                for (llvm::BasicBlock::const_iterator i = bb->begin(), e = bb->end(); i != e; ++i) {
                    const llvm::Instruction* llvmInstruction = i;

                    //?? what are compare llvmInstruction predicates
                    // if (const CmpInst *CI = dyn_cast<CmpInst>(&llvmInstruction))

                    translator.add(llvmInstruction);
                }
            }
        }
    }

    //set_branchtargets(&v, currentInstructionructions, num_instructions);

    mesaProgram->Instructions = translator.getMesaInstructions();
    mesaProgram->NumInstructions = translator.getNumMesaInstructions();

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

    printf("\nFinishing translation from Bottom IR to TGSI\n");
}
