//===- BottomToGLSL.cpp - Translate bottom IR to GLSL ---------------------===//
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
// Usable by the bottom translator to create Glsl.
//
//===----------------------------------------------------------------------===//

#ifdef _WIN32
#define snprintf _snprintf
#endif

// LLVM includes
#include "llvm/IntrinsicInst.h"

#include <cstdio>
#include <string>
#include <sstream>
#include <map>
#include <vector>
#include <cstdio>

// LunarGLASS includes
#include "LunarGLASSBottomIR.h"
#include "Manager.h"
#include "GlslTarget.h"

const bool Obfuscate = false; // Note:  This should be a command line option
const int GlslVersion = 130;  // Note:  This should come from the original source

//
// Implement the GLSL backend
//
class GlslBackEnd : public gla::BackEnd {
public:
    GlslBackEnd() { }
    virtual ~GlslBackEnd() { }

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

    virtual bool preferRegistersOverMemory()
    {
        return true;
    }
};

//
// factory for the GLSL backend
//
gla::BackEnd* gla::GetGlslBackEnd()
{
    return new GlslBackEnd();
}

void gla::ReleaseGlslBackEnd(gla::BackEnd* backEnd)
{
    delete backEnd;
}

//
// Implement the Bottom IR -> GLSL translator
//
namespace gla {
    class GlslTarget;

    enum EVariableQualifier {
        EVQNone,
        EVQUniform,
        EVQGlobal,
        EVQInput,
        EVQOutput,
        EVQTemporary,
        EVQConstant
    };
};

class gla::GlslTarget : public gla::BackEndTranslator {
public:
    GlslTarget()
    {
        indentLevel = 0;
        lastVariable = 20;
        globalDeclarations << "#version " << GlslVersion << std::endl;
    }

    ~GlslTarget()
    {
    }

    void addGlobal(const llvm::GlobalVariable* global)
    {
        addNewVariable(global, global->getNameStr());
        declareVariable(global, global->getNameStr().c_str());
    }

    void startFunction()
    {
        shader << "void main()";
        newLine();
        newScope();
    }

    void endFunction()
    {
        leaveScope();
    }

    void add(const llvm::Instruction* llvmInstruction);

    //
    // Motivated by need to convert to structured flow control and
    // eliminate phi functions.
    //
    void addIf(const llvm::Value* cond)
    {
        newLine();
        shader << "if (bool(";
        mapGlaOperand(cond);
        shader << ")) ";
        newScope();
    }

    void addElse()
    {
        leaveScope();
        shader << "else ";
        newScope();
    }

    void addEndif()
    {
        leaveScope();
    }

    void addCopy(const llvm::Value* dst, const llvm::Value* src)
    {
        newLine();
        mapGlaDestination(dst);
        shader << " = ";
        mapGlaOperand(src);
        shader << ";";
    }

    void print();

protected:

    void newLine()
    {
        static int count = 0;
        if (Obfuscate) {
            ++count;
            if (count > 4) {
                shader << std::endl;
                count = 0;
            }
        } else {
            shader << std::endl;
            for (int i = 0; i < indentLevel; ++i)
                shader << "    ";
        }
    }

    void newScope()
    {
        ++indentLevel;
        shader << "{";
    }

    void leaveScope()
    {
        --indentLevel;
        newLine();
        shader << "}";
        newLine();
    }

    void mapGlaIntrinsic(const llvm::IntrinsicInst* llvmInstruction);

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

    EVariableQualifier mapGlaAddressSpace(const llvm::Value* value)
    {
        if (const llvm::PointerType* pointer = llvm::dyn_cast<llvm::PointerType>(value->getType())) {
            switch (pointer->getAddressSpace()) {
            case gla::UniformAddressSpace:
                return EVQUniform;
            case gla::GlobalAddressSpace:
                return EVQGlobal;
            default:
                assert(!"Unknown gla address space");
            }
        }

        if (llvm::isa<llvm::Constant>(value)) {
            return EVQConstant;
        }

        return EVQTemporary;
    }

    const char* mapGlaToQualifierString(EVariableQualifier vq)
    {
        const char *string = "UNKNOWN QUALIFIER";

        switch (vq) {
        case EVQUniform:         string = "uniform";                  break;
        case EVQGlobal:          string = "global";                   break;
        case EVQInput:      
            GlslVersion >= 130 ? string = "in" : string = "varying";  break;
        case EVQOutput:     
            GlslVersion >= 130 ? string = "out": string = "varying";  break;
        case EVQTemporary:       string = "temp";                     break;
        case EVQConstant:        string = "const";                    break;
        default: assert(! "unknown VariableQualifier");
        }

        return string;
    }

    void mapGlaOperand(const llvm::Value* value)
    {
        mapGlaValue(value);
        if (Obfuscate) {
            int count = getGlaComponentCount(value);
            if (count > 1)
                mapComponentCountToSwizzle(count);
        }
    }

    void mapGlaDestination(const llvm::Value* value)
    {
        mapGlaValue(value);
    }

    void mapComponentCountToSwizzle(int numComponents)
    {
        shader << ".";

        switch (numComponents) {
        case 1:   shader << "x";     break;
        case 2:   shader << "xy";    break;
        case 3:   shader << "xyz";   break;
        case 4:   shader << "xyzw";  break;
        default:
                  shader << "xyzw";
                  assert(! "Vector too large");
        }
    }

    void mapComponentToSwizzle(int component)
    {
        switch (component) {
        case 0:   shader << "x";     return;
        case 1:   shader << "y";     return;
        case 2:   shader << "z";     return;
        case 3:   shader << "w";     return;
        default:  assert(!"Vector too large");
        }

        shader << "x";
    }

    void mapComponentToLSwizzle(int component)
    {
        mapComponentToSwizzle(component);
    }

    void mapGlaSamplerTypeToMesa(const llvm::Value* samplerType)
    {
        int sampler = mapGlaToConstant(samplerType) ;
        switch(sampler) {
        case ESampler1D:        shader << "texture1D";      break;
        case ESampler2D:        shader << "texture2D";      break;
        case ESampler3D:        shader << "texture3D";      break;
        case ESamplerCube:      shader << "textureCube";    break;
        case ESampler1DShadow:  shader << "shadow1D";       break;
        case ESampler2DShadow:  shader << "shadow2D";       break;
        default:                shader << "unsupported";    break;
        }

        return;
    }

    void mapGlaTextureStyle(const llvm::IntrinsicInst* llvmInstruction)
    {
        // Check flags for proj/lod/offset
        int flags = mapGlaToConstant(llvmInstruction->getOperand(FlagLocAOS));

        gla::ETextureFlags texFlags = *(gla::ETextureFlags*)&flags;

        if (texFlags.EProjected)
            shader << "Proj";
        else if (texFlags.ELod)
            shader << "Lod";

        if(isGradientTexInst(llvmInstruction))
            shader << "Grad";
    }

    bool needsBiasLod(const llvm::IntrinsicInst* llvmInstruction)
    {
        // Check flags for bias/lod
        int flags = mapGlaToConstant(llvmInstruction->getOperand(FlagLocAOS));

        gla::ETextureFlags texFlags = *(gla::ETextureFlags*)&flags;

        if ( texFlags.EBias || texFlags.ELod )
            return true;
        else
            return false;
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

    void getNewVariable(const llvm::Value* value, std::string* varString)
    {
        ++lastVariable;
        const size_t bufSize = 10;
        char buf[bufSize];
        if (Obfuscate) {
            int i;
            for (i = 0; i <= lastVariable-4; i += 4) {
                switch ((i/4) % 4) {
                case 0:   varString->append("x"); break;
                case 1:   varString->append("y"); break;
                case 2:   varString->append("z"); break;
                case 3:   varString->append("w"); break;
                }
            }
            switch (lastVariable - i) {
            case 0:   varString->append("x"); break;
            case 1:   varString->append("y"); break;
            case 2:   varString->append("z"); break;
            case 3:   varString->append("w"); break;
            }
        } else {
            varString->append(mapGlaToQualifierString(mapGlaAddressSpace(value)));
            snprintf(buf, bufSize, "%d", lastVariable);
            varString->append(buf);
        }
    }

    void declareVariable(const llvm::Value* value, const char* varString, EVariableQualifier vq = EVQNone)
    {
        if (vq == EVQNone)
            vq = mapGlaAddressSpace(value);

        switch (vq) {
        case EVQUniform:
        case EVQConstant:
        case EVQInput:
            globalDeclarations << mapGlaToQualifierString(vq) << " ";
            mapGlaType(globalDeclarations, value);
            globalDeclarations << " " << varString << ";" << std::endl;
            break;
        case EVQGlobal:
            mapGlaType(globalDeclarations, value);
            globalDeclarations << " " << varString << ";" << std::endl;
            break;
        case EVQTemporary:
            mapGlaType(shader, value);
            shader << " ";
            break;
        default: assert(! "unknown VariableQualifier");
        }
    }

    void mapGlaType(std::ostringstream& out, const llvm::Value* value)
    {
        if (getGlaComponentCount(value) > 1)
            out << "vec" << getGlaComponentCount(value);
        else
            out << "float";
    }

    void mapGlaValue(const llvm::Value* value)
    {
        // if it isn't already there, add it
        if (valueMap[value] == 0) {
            std::string* newVariable = new std::string;
            getNewVariable(value, newVariable);
            declareVariable(value, newVariable->c_str());
            valueMap[value] = newVariable;
        }

        shader << valueMap[value]->c_str();
    }

    bool addNewVariable(const llvm::Value* value, std::string name)
    {
        if (valueMap[value] == 0) {
            valueMap[value] = new std::string(name);  //?? need to delete these?
            return true;
        } else {
            assert(name == *valueMap[value]);
            return false;
        }
    }

    void mapGlaSwizzle(int glaSwizzle)
    {
        shader << ".";
        // Pull each two bit channel out of the integer
        for(int i = 0; i < 4; i++)
            mapComponentToSwizzle((glaSwizzle >> i*2) & 0x3);
    }

    // mapping from LLVM values to Glsl variables
    std::map<const llvm::Value*, std::string*> valueMap;

    std::ostringstream globalDeclarations;
    std::ostringstream shader;
    int indentLevel;
    int lastVariable;
};

//
// Factory for GLSL back-end translator
//
gla::BackEndTranslator* gla::GetGlslTranslator()
{
    return new gla::GlslTarget();
}

void gla::ReleaseGlslTranslator(gla::BackEndTranslator* target)
{
    delete target;
}

//
// Add an LLVM instruction to the end of the mesa instructions.
//
void gla::GlslTarget::add(const llvm::Instruction* llvmInstruction)
{
    newLine();

    const char* charOp = 0;

    // first, just look for binary ops
    switch (llvmInstruction->getOpcode()) {
    case llvm::Instruction::FAdd:           charOp = "+";  break;
    case llvm::Instruction::FSub:           charOp = "-";  break;
    case llvm::Instruction::FMul:           charOp = "*";  break;
    case llvm::Instruction::FDiv:           charOp = "/";  break;

    case llvm::Instruction::FCmp:
        if (const llvm::FCmpInst* fcmp = llvm::dyn_cast<llvm::FCmpInst>(llvmInstruction)) {
            switch (fcmp->getPredicate()) {
            case llvm::FCmpInst::FCMP_OGT:  charOp = ">";   break;
            case llvm::FCmpInst::FCMP_OEQ:  charOp = "==";  break;
            case llvm::FCmpInst::FCMP_OGE:  charOp = ">=";  break;
            case llvm::FCmpInst::FCMP_OLT:  charOp = "<";   break;
            case llvm::FCmpInst::FCMP_OLE:  charOp = "<=";  break;
            case llvm::FCmpInst::FCMP_ONE:  charOp = "!=";  break;
            default:
                printf("Undefined (for now) comparison operator used");
                return;
            }
        }
        else {
            printf("FCmp instruction found that cannot dyncast to FCmpInst");
            return;
        }
        break;
    default:
        break;
        // fall through to check other ops
    }

    if (charOp) {
        mapGlaDestination(llvmInstruction);
        shader << " = ";
        mapGlaOperand(llvmInstruction->getOperand(0));
        shader << " " << charOp << " ";
        mapGlaOperand(llvmInstruction->getOperand(1));
        shader << ";";
        return;
    }

    switch (llvmInstruction->getOpcode()) {

    case llvm::Instruction::PHI:
        // this got turned into copies in predecessors
        return;

    case llvm::Instruction::Ret:
        shader << "return;";
        return;

    case llvm::Instruction::Call: // includes intrinsics...
        if (const llvm::IntrinsicInst* i = llvm::dyn_cast<llvm::IntrinsicInst>(llvmInstruction)) {
            mapGlaIntrinsic(i);
        } else {
            assert(! "Unsupported call (non-intrinsic)");
        }
        return;

    case llvm::Instruction::Load:
        if (llvm::isa<llvm::PointerType>(llvmInstruction->getOperand(0)->getType())) {
            mapGlaDestination(llvmInstruction);
            shader << " = ";
            mapGlaOperand(llvmInstruction->getOperand(0));
            shader << ";";
        } else {
            printf("load instruction is not through pointer\n");
        }
        return;

    case llvm::Instruction::Alloca:
        mapGlaValue(llvmInstruction);
        shader << ";";
        return;

    case llvm::Instruction::Store:
        if (llvm::isa<llvm::PointerType>(llvmInstruction->getOperand(1)->getType())) {
            mapGlaDestination(llvmInstruction->getOperand(1));
            shader << " = ";
            mapGlaOperand(llvmInstruction->getOperand(0));
            shader << ";";
        } else {
            printf("store instruction is not through pointer\n");
        }
        return;

    default:
        printf("UNSUPPORTED opcode %d\n", llvmInstruction->getOpcode());
    }
}

//
// Handle the subcase of an LLVM instruction being an intrinsic call.
//
void gla::GlslTarget::mapGlaIntrinsic(const llvm::IntrinsicInst* llvmInstruction)
{
    // Handle pipeline read/write
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_writeData:
        switch (mapGlaToConstant(llvmInstruction->getOperand(0)))
        {
        case 0:
            shader << "gl_FragColor = ";
            mapGlaOperand(llvmInstruction->getOperand(1));
            shader << ";";
            return;
        default:
            printf ("Unhandled data output\n");
        }
        return;

    case llvm::Intrinsic::gla_getInterpolant:
        if (addNewVariable(llvmInstruction, llvmInstruction->getNameStr())) {
            declareVariable(llvmInstruction, llvmInstruction->getNameStr().c_str(), EVQInput);
        }
        return;
    }

    // Handle texturing
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_fTextureSample:
    case llvm::Intrinsic::gla_fTextureSampleLod:
    case llvm::Intrinsic::gla_fTextureSampleLodOffset:
    case llvm::Intrinsic::gla_fTextureSampleLodOffsetGrad:

        mapGlaDestination(llvmInstruction);
        shader << " = ";
        mapGlaSamplerTypeToMesa(llvmInstruction->getOperand(0));
        mapGlaTextureStyle(llvmInstruction);
        shader << "(";
        mapGlaOperand(llvmInstruction->getOperand(SamplerLocAOS));
        shader << ", ";
        mapGlaOperand(llvmInstruction->getOperand(CoordLocAOS));

        if(needsBiasLod(llvmInstruction)) {
            shader << ", ";
            mapGlaOperand(llvmInstruction->getOperand(BiasLocAOS));
        }

        if(isGradientTexInst(llvmInstruction)) {  //?? this can move to a place they are shared between back-ends
            shader << ", ";
            mapGlaOperand(llvmInstruction->getOperand(DdxLocAOS));
            shader << ", ";
            mapGlaOperand(llvmInstruction->getOperand(DdyLocAOS));
        }

        shader << ");";

        return;
    }

    // Handle swizzles
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_fSwizzle:
        mapGlaDestination(llvmInstruction);
        shader << " = ";
        mapGlaOperand(llvmInstruction->getOperand(0));
        mapGlaSwizzle(mapGlaToConstant(llvmInstruction->getOperand(1)));
        shader << ";";
        return;
    }

    // Handle the one-to-one mappings
    const char* callString = 0;

    switch (llvmInstruction->getIntrinsicID()) {

    case llvm::Intrinsic::gla_fMix:  callString = "mix";  break;
        break;
    }

    assert(callString);
    mapGlaDestination(llvmInstruction);
    shader << " = " << callString << "(";
    for (unsigned int op = 0; op < llvmInstruction->getNumOperands(); ++op) {
        if (op > 0)
            shader << ", ";
        mapGlaOperand(llvmInstruction->getOperand(op));
    }
    shader << ");";
}

void gla::GlslTarget::print()
{
    printf("\nLunarGoo\n%s\n%s", globalDeclarations.str().c_str(), shader.str().c_str());
}
