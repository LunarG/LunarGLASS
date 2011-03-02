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
#include "Exceptions.h"
#include "LunarGLASSBottomIR.h"
#include "LunarGLASSBackend.h"
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

    virtual bool getRemovePhiFunctions()
    {
        return true;
    }

    virtual bool getDeclarePhiCopies()
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
        const llvm::Type* type;
        if (const llvm::PointerType* pointer = llvm::dyn_cast<llvm::PointerType>(global->getType()))
            type = pointer->getContainedType(0);
        else
            type = global->getType();

        declareVariable(type, global->getNameStr().c_str(), mapGlaAddressSpace(global));
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

    void declarePhiCopy(const llvm::Value* dst)
    {
        newLine();
        mapGlaDestination(dst);
        shader << ";";
    }

    void addPhiCopy(const llvm::Value* dst, const llvm::Value* src)
    {
        newLine();
        mapGlaDestination(dst);
        shader << " = ";
        mapGlaOperand(src);
        shader << ";";
    }

    void addIf(const llvm::Value* cond)
    {
        newLine();
        shader << "if (";
        mapGlaOperand(cond);
        shader << ") ";
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

    EVariableQualifier mapGlaAddressSpace(const llvm::Value* value)
    {
        if (const llvm::PointerType* pointer = llvm::dyn_cast<llvm::PointerType>(value->getType())) {
            switch (pointer->getAddressSpace()) {
            case gla::UniformAddressSpace:
                return EVQUniform;
            case gla::GlobalAddressSpace:
                return EVQGlobal;
            default:
                UnsupportedFunctionality("Address Space in Bottom IR: ", pointer->getAddressSpace());
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
        default:
            assert(! "unknown VariableQualifier");
        }

        return string;
    }

    void mapGlaOperand(const llvm::Value* value)
    {
        mapGlaValue(value);
        if (Obfuscate) {
            int count = GetComponentCount(value);
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
        default:  assert(! "Vector too large");
        }

        shader << "x";
    }

    void mapComponentToLSwizzle(int component)
    {
        mapComponentToSwizzle(component);
    }

    void mapGlaSamplerType(const llvm::Value* samplerType)
    {
        int sampler = GetConstantValue(samplerType) ;
        switch(sampler) {
        case ESampler1D:        shader << "texture1D";      break;
        case ESampler2D:        shader << "texture2D";      break;
        case ESampler3D:        shader << "texture3D";      break;
        case ESamplerCube:      shader << "textureCube";    break;
        case ESampler1DShadow:  shader << "shadow1D";       break;
        case ESampler2DShadow:  shader << "shadow2D";       break;
        default:
            shader << "texture";
            UnsupportedFunctionality("Texturing in Bottom IR: ", sampler, EATContinue);
            break;
        }

        return;
    }

    void mapGlaTextureStyle(const llvm::IntrinsicInst* llvmInstruction)
    {
        // Check flags for proj/lod/offset
        int flags = GetConstantValue(llvmInstruction->getOperand(FlagLocAOS));

        gla::ETextureFlags texFlags = *(gla::ETextureFlags*)&flags;

        if (texFlags.EProjected)
            shader << "Proj";
        else if (texFlags.ELod)
            shader << "Lod";

        if(IsGradientTexInst(llvmInstruction))
            shader << "Grad";
    }

    bool needsBiasLod(const llvm::IntrinsicInst* llvmInstruction)
    {
        // Check flags for bias/lod
        int flags = GetConstantValue(llvmInstruction->getOperand(FlagLocAOS));

        gla::ETextureFlags texFlags = *(gla::ETextureFlags*)&flags;

        if ( texFlags.EBias || texFlags.ELod )
            return true;
        else
            return false;
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

    void declareVariable(const llvm::Type* type, const char* varString, EVariableQualifier vq)
    {
        switch (vq) {
        case EVQUniform:
        case EVQConstant:
        case EVQInput:
            globalDeclarations << mapGlaToQualifierString(vq) << " ";
            mapGlaType(globalDeclarations, type);
            globalDeclarations << " " << varString << ";" << std::endl;
            break;
        case EVQGlobal:
            mapGlaType(globalDeclarations, type);
            globalDeclarations << " " << varString << ";" << std::endl;
            break;
        case EVQTemporary:
            mapGlaType(shader, type);
            shader << " ";
            break;
        default:
            assert(! "unknown VariableQualifier");
        }
    }

    void mapGlaType(std::ostringstream& out, const llvm::Type* type)
    {
        const llvm::VectorType *vectorType = llvm::dyn_cast<llvm::VectorType>(type);
        if (vectorType) {
            if (type->getContainedType(0) == type->getFloatTy(type->getContext()))
                out << "vec";
            else if (type->getContainedType(0) == type->getInt1Ty(type->getContext()))
                out << "bvec";
            else if (type->getContainedType(0) == type->getInt32Ty(type->getContext()))
                out << "ivec";
            else
                UnsupportedFunctionality("Basic Type in Bottom IR");
            out << GetComponentCount(type);
        } else {
            if (type == type->getFloatTy(type->getContext()))
                out << "float";
            else if (type == type->getInt1Ty(type->getContext()))
                out << "bool";
            else if (type == type->getInt32Ty(type->getContext()))
                out << "int";
            else
                UnsupportedFunctionality("Basic Type in Bottom IR");
        }
    }

    void mapGlaValue(const llvm::Value* value)
    {
        // if it isn't already there, add it
        if (valueMap[value] == 0) {
            std::string* newVariable = new std::string;
            getNewVariable(value, newVariable);
            declareVariable(value->getType(), newVariable->c_str(), mapGlaAddressSpace(value));
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
    const char* charOp = 0;

    //
    // Look for binary ops, where the form would be "operand op operand"
    //

    switch (llvmInstruction->getOpcode()) {
    case llvm::Instruction:: Add:
    case llvm::Instruction::FAdd:           charOp = "+";  break;
    case llvm::Instruction:: Sub:
    case llvm::Instruction::FSub:           charOp = "-";  break;
    case llvm::Instruction:: Mul:
    case llvm::Instruction::FMul:           charOp = "*";  break;
    case llvm::Instruction::UDiv:
    case llvm::Instruction::SDiv:
    case llvm::Instruction::FDiv:           charOp = "/";  break;
    case llvm::Instruction::URem:
    case llvm::Instruction::SRem:
    case llvm::Instruction::FRem:           charOp = "%";  break;
    case llvm::Instruction::Shl:            charOp = "<<"; break;
    case llvm::Instruction::LShr:           charOp = ">>"; break;
    case llvm::Instruction::AShr:           charOp = ">>"; break;
    case llvm::Instruction::And:            charOp = "&";  break;
    case llvm::Instruction::Or:             charOp = "|";  break;
    case llvm::Instruction::Xor:            charOp = "^";  break;

    case llvm::Instruction::ICmp:
    case llvm::Instruction::FCmp:
        if (! llvm::isa<llvm::VectorType>(llvmInstruction->getOperand(0)->getType())) {
            
            const llvm::Type* type = llvmInstruction->getOperand(0)->getType();
            if (type != type->getFloatTy(llvmInstruction->getContext()) &&
                type != type->getDoubleTy(llvmInstruction->getContext()) &&
                type != type->getInt32Ty(llvmInstruction->getContext())) {

                UnsupportedFunctionality("Can only compare integers and floats");
                return;
            }

            // Handle float and integer scalars
            // (Vectors are handled as built-in functions)
            if (const llvm::CmpInst* cmp = llvm::dyn_cast<llvm::CmpInst>(llvmInstruction)) {
                switch (cmp->getPredicate()) {
                case llvm::FCmpInst::FCMP_OEQ:
                case llvm::ICmpInst::ICMP_EQ:   charOp = "==";  break;
                
                case llvm::FCmpInst::FCMP_ONE:
                case llvm::ICmpInst::ICMP_NE:   charOp = "!=";  break;
                
                case llvm::FCmpInst::FCMP_OGT:
                case llvm::ICmpInst::ICMP_UGT:
                case llvm::ICmpInst::ICMP_SGT:  charOp = ">";   break;
                
                case llvm::FCmpInst::FCMP_OGE:
                case llvm::ICmpInst::ICMP_UGE:
                case llvm::ICmpInst::ICMP_SGE:  charOp = ">=";  break;
                
                case llvm::FCmpInst::FCMP_OLT:
                case llvm::ICmpInst::ICMP_ULT:
                case llvm::ICmpInst::ICMP_SLT:  charOp = "<";   break;
                
                case llvm::FCmpInst::FCMP_OLE:
                case llvm::ICmpInst::ICMP_ULE:
                case llvm::ICmpInst::ICMP_SLE:  charOp = "<=";  break;
                default:
                    charOp = "==";
                    UnsupportedFunctionality("Comparison Operator in Bottom IR: ", cmp->getPredicate(), EATContinue);
                }
            } else {
                assert(! "Cmp instruction found that cannot dyncast to CmpInst");
            }
        }
        break;

    default:
        break;
        // fall through to check other ops
    }

    // Handle the binary ops
    if (charOp) {
        newLine();
        mapGlaDestination(llvmInstruction);
        shader << " = ";
        mapGlaOperand(llvmInstruction->getOperand(0));
        shader << " " << charOp << " ";
        mapGlaOperand(llvmInstruction->getOperand(1));
        shader << ";";
        return;
    }

    //
    // Look for unary ops, where the form would be "op operand"
    //

    //switch (llvmInstruction->getOpcode()) {
    // LLVM turned these into a binary ops, might want to undo that...
    //case llvm::Instruction:: Neg:
    //case llvm::Instruction::FNeg:           charOp = "-";  break;
    //case llvm::Instruction::Not:            charOp = "!";  break;
    //default:
    //    break;
        // fall through to check other ops
    //}

    // Handle the unary ops
    if (charOp) {
        newLine();
        mapGlaDestination(llvmInstruction);
        shader << " = " << charOp << " ";
        mapGlaOperand(llvmInstruction->getOperand(0));
        shader << ";";
        return;
    }

    //
    // Look for unary ops, where the form would be "op(operand)"
    //

    switch (llvmInstruction->getOpcode()) {
    case llvm::Instruction::FPTrunc:        charOp = "trunc";  break;
    case llvm::Instruction::FPToUI:         charOp = "uint";   break;
    case llvm::Instruction::FPToSI:         charOp = "int";    break;
    case llvm::Instruction::UIToFP:         charOp = "float";  break;
    case llvm::Instruction::SIToFP:         charOp = "float";  break;
    default:
        break;
        // fall through to check other ops
    }

    // Handle the unary ops
    if (charOp) {
        newLine();
        mapGlaDestination(llvmInstruction);
        shader << " = " << charOp << "(";
        mapGlaOperand(llvmInstruction->getOperand(0));
        shader << ");";
        return;
    }

    //
    // Handle remaining ops
    //

    switch (llvmInstruction->getOpcode()) {

    case llvm::Instruction::Ret:
        newLine();
        shader << "return;";
        return;

    case llvm::Instruction::Call: // includes intrinsics...
        if (const llvm::IntrinsicInst* i = llvm::dyn_cast<llvm::IntrinsicInst>(llvmInstruction)) {
            mapGlaIntrinsic(i);
        } else {
            UnsupportedFunctionality("Function Call in Bottom IR");
        }
        return;

    case llvm::Instruction::ICmp:
    case llvm::Instruction::FCmp:
        {
            if (! llvm::isa<llvm::VectorType>(llvmInstruction->getOperand(0)->getType())) {
                UnsupportedFunctionality("Can only compare scalars and vectors");

                return;
            }

            if (const llvm::CmpInst* cmp = llvm::dyn_cast<llvm::CmpInst>(llvmInstruction)) {
                switch (cmp->getPredicate()) {
                case llvm::FCmpInst::FCMP_OEQ:
                case llvm::ICmpInst::ICMP_EQ:   charOp = "equal";             break;
                
                case llvm::FCmpInst::FCMP_ONE:
                case llvm::ICmpInst::ICMP_NE:   charOp = "notEqual";          break;
                
                case llvm::FCmpInst::FCMP_OGT:
                case llvm::ICmpInst::ICMP_UGT:
                case llvm::ICmpInst::ICMP_SGT:  charOp = "greaterThan";       break;
                
                case llvm::FCmpInst::FCMP_OGE:
                case llvm::ICmpInst::ICMP_UGE:
                case llvm::ICmpInst::ICMP_SGE:  charOp = "greaterThanEqual";  break;
                
                case llvm::FCmpInst::FCMP_OLT:
                case llvm::ICmpInst::ICMP_ULT:
                case llvm::ICmpInst::ICMP_SLT:  charOp = "lessThan";          break;
                
                case llvm::FCmpInst::FCMP_OLE:
                case llvm::ICmpInst::ICMP_ULE:
                case llvm::ICmpInst::ICMP_SLE:  charOp = "lessThanEqual";     break;
                default:
                    charOp = "equal";
                    UnsupportedFunctionality("Comparison Vector Operator in Bottom IR: ", cmp->getPredicate(), EATContinue);
                }
            } else {
                assert(! "Cmp vector instruction found that cannot dyncast to CmpInst");
            }
            
            newLine();
            mapGlaValue(llvmInstruction);
            shader << " = " << charOp << "(";
            mapGlaOperand(llvmInstruction->getOperand(0));
            shader << ", ";
            mapGlaOperand(llvmInstruction->getOperand(1));
            shader << ");";
        }
        return;

    case llvm::Instruction::Load:
        addNewVariable(llvmInstruction, llvmInstruction->getOperand(0)->getNameStr());
        return;

    case llvm::Instruction::Alloca:
        newLine();
        mapGlaValue(llvmInstruction);
        shader << ";";
        return;

    case llvm::Instruction::Store:
        if (llvm::isa<llvm::PointerType>(llvmInstruction->getOperand(1)->getType())) {
            newLine();
            mapGlaDestination(llvmInstruction->getOperand(1));
            shader << " = ";
            mapGlaOperand(llvmInstruction->getOperand(0));
            shader << ";";
        } else {
            assert(! "store instruction is not through pointer\n");
        }
        return;

    default:
        UnsupportedFunctionality("Opcode in Bottom IR: ", llvmInstruction->getOpcode(), EATContinue);
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
        switch (GetConstantValue(llvmInstruction->getOperand(0)))
        {
        case 0:
            newLine();
            shader << "gl_FragColor = ";
            mapGlaOperand(llvmInstruction->getOperand(1));
            shader << ";";
            return;
        default:
            UnsupportedFunctionality("Unhandled data output variable in Bottom IR: ", GetConstantValue(llvmInstruction->getOperand(0)));
        }
        return;

    case llvm::Intrinsic::gla_getInterpolant:
        if (addNewVariable(llvmInstruction, llvmInstruction->getNameStr())) {
            declareVariable(llvmInstruction->getType(), llvmInstruction->getNameStr().c_str(), EVQInput);
        }
        return;
    }

    // Handle texturing
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_fTextureSample:
    case llvm::Intrinsic::gla_fTextureSampleLod:
    case llvm::Intrinsic::gla_fTextureSampleLodOffset:
    case llvm::Intrinsic::gla_fTextureSampleLodOffsetGrad:

        newLine();
        mapGlaDestination(llvmInstruction);
        shader << " = ";
        mapGlaSamplerType(llvmInstruction->getOperand(0));
        mapGlaTextureStyle(llvmInstruction);
        shader << "(";
        mapGlaOperand(llvmInstruction->getOperand(SamplerLocAOS));
        shader << ", ";
        mapGlaOperand(llvmInstruction->getOperand(CoordLocAOS));

        if(needsBiasLod(llvmInstruction)) {
            shader << ", ";
            mapGlaOperand(llvmInstruction->getOperand(BiasLocAOS));
        }

        if(IsGradientTexInst(llvmInstruction)) {  //?? this can move to a place they are shared between back-ends
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
        newLine();
        mapGlaDestination(llvmInstruction);
        shader << " = ";
        mapGlaOperand(llvmInstruction->getOperand(0));
        mapGlaSwizzle(GetConstantValue(llvmInstruction->getOperand(1)));
        shader << ";";
        return;
    }

    // Handle the one-to-one mappings
    const char* callString = 0;
    unsigned int callArgs = 0;

    switch (llvmInstruction->getIntrinsicID()) {

    // Floating-Point and Integer Operations
    case llvm::Intrinsic::gla_abs:
    case llvm::Intrinsic::gla_fAbs:         callString = "abs";   callArgs = 1; break;
    case llvm::Intrinsic::gla_sMin:
    case llvm::Intrinsic::gla_uMin:
    case llvm::Intrinsic::gla_fMin:         callString = "min";   callArgs = 2; break;
    case llvm::Intrinsic::gla_sMax:
    case llvm::Intrinsic::gla_uMax:
    case llvm::Intrinsic::gla_fMax:         callString = "max";   callArgs = 2; break;
    case llvm::Intrinsic::gla_sClamp:
    case llvm::Intrinsic::gla_uClamp:
    case llvm::Intrinsic::gla_fClamp:       callString = "clamp"; callArgs = 3; break;

    // Floating-Point Only Operations
    case llvm::Intrinsic::gla_fRadians:     callString = "radians";     callArgs = 1; break;
    case llvm::Intrinsic::gla_fDegrees:     callString = "degrees";     callArgs = 1; break;
    case llvm::Intrinsic::gla_fSin:         callString = "sin";         callArgs = 1; break;
    case llvm::Intrinsic::gla_fCos:         callString = "cos";         callArgs = 1; break;
    case llvm::Intrinsic::gla_fTan:         callString = "tan";         callArgs = 1; break;
    case llvm::Intrinsic::gla_fAsin:        callString = "asin";        callArgs = 1; break;
    case llvm::Intrinsic::gla_fAcos:        callString = "acos";        callArgs = 1; break;
    case llvm::Intrinsic::gla_fAtan:        callString = "atan";        callArgs = 1; break;
    case llvm::Intrinsic::gla_fAtan2:       callString = "atan2";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fSinh:        callString = "sinh";        callArgs = 1; break;
    case llvm::Intrinsic::gla_fCosh:        callString = "cosh";        callArgs = 1; break;
    case llvm::Intrinsic::gla_fTanh:        callString = "tanh";        callArgs = 1; break;
    case llvm::Intrinsic::gla_fAsinh:       callString = "asinh";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fAcosh:       callString = "acosh";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fAtanh:       callString = "atanh";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fPow:         callString = "pow";         callArgs = 2; break;
    //case llvm::Intrinsic::gla_fPowi:        callString = "powi";        callArgs = 2; break;
    case llvm::Intrinsic::gla_fExp:         callString = "exp";         callArgs = 1; break;
    case llvm::Intrinsic::gla_fLog:         callString = "log";         callArgs = 1; break;
    case llvm::Intrinsic::gla_fExp2:        callString = "exp2";        callArgs = 1; break;
    case llvm::Intrinsic::gla_fLog2:        callString = "log2";        callArgs = 1; break;
    case llvm::Intrinsic::gla_fExp10:       callString = "exp10";       break; // callArgs = 1;
    case llvm::Intrinsic::gla_fLog10:       callString = "log10";       break; // callArgs = 1;
    case llvm::Intrinsic::gla_fSqrt:        callString = "sqrt";        callArgs = 1; break;
    case llvm::Intrinsic::gla_fInverseSqrt: callString = "inversesqrt"; callArgs = 1; break;
    case llvm::Intrinsic::gla_fSign:        callString = "sign";        callArgs = 1; break;
    case llvm::Intrinsic::gla_fFloor:       callString = "floor";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fCeiling:     callString = "ceil";        callArgs = 1; break;
    case llvm::Intrinsic::gla_fRoundEven:   callString = "roundEven";   callArgs = 1; break;
    case llvm::Intrinsic::gla_fRoundFast:   callString = "round";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fFraction:    callString = "fract";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fModF:        callString = "modf";        break; // callArgs = 2;
    case llvm::Intrinsic::gla_fMix:         callString = "mix";         callArgs = 3; break;
    case llvm::Intrinsic::gla_fStep:        callString = "step";        callArgs = 2; break;
    case llvm::Intrinsic::gla_fSmoothStep:  callString = "smoothstep";  callArgs = 3; break;
    case llvm::Intrinsic::gla_fIsNan:       callString = "isnan";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fIsInf:       callString = "isinf";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fFma:         callString = "fma";         callArgs = 3; break;

    // Integer-Only Operations
    case llvm::Intrinsic::gla_addCarry:     callString = "addCarry";        callArgs = 2; break;
    case llvm::Intrinsic::gla_subBorrow:    callString = "subBorrow";       callArgs = 2; break;
    case llvm::Intrinsic::gla_umulExtended: callString = "umulExtended";    callArgs = 2; break;
    case llvm::Intrinsic::gla_smulExtended: callString = "smulExtended";    callArgs = 2; break;

    // Bit Operations
    case llvm::Intrinsic::gla_fFloatBitsToInt:  callString = "floatBitsToInt";      callArgs = 1; break;
    //case llvm::Intrinsic::gla_sIntBitsTofloat:  callString = "intBitsTofloat";      callArgs = 1; break;
    //case llvm::Intrinsic::gla_uIntBitsTofloat:  callString = "intBitsTofloat";      callArgs = 1; break;
    case llvm::Intrinsic::gla_sBitFieldExtract:
    case llvm::Intrinsic::gla_uBitFieldExtract: callString = "bitFieldExtract";     callArgs = 3; break;
    case llvm::Intrinsic::gla_bitFieldInsert:   callString = "bitFieldInsert";      callArgs = 3; break;
    case llvm::Intrinsic::gla_bitReverse:       callString = "bitFieldReverse";     callArgs = 1; break;
    case llvm::Intrinsic::gla_bitCount:         callString = "bitCount";            callArgs = 1; break;
    case llvm::Intrinsic::gla_findLSB:          callString = "findLSB";             callArgs = 1; break;
    case llvm::Intrinsic::gla_sFindMSB:
    case llvm::Intrinsic::gla_uFindMSB:         callString = "findMSB";             callArgs = 1; break;

    // Pack and Unpack
    case llvm::Intrinsic::gla_fFrexp:            callString = "frexp";              break;      // callArgs =
    case llvm::Intrinsic::gla_fLdexp:            callString = "ldexp";              break;      // callArgs =
    case llvm::Intrinsic::gla_fPackUnorm2x16:    callString = "packUnorm2x16";      callArgs = 1; break;
    case llvm::Intrinsic::gla_fPackUnorm4x8:     callString = "packUnorm4x8";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fPackSnorm4x8:     callString = "packSnorm4x8";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fUnpackUnorm2x16:  callString = "unpackUnorm2x16";    callArgs = 1; break;
    case llvm::Intrinsic::gla_fUnpackUnorm4x8:   callString = "unpackUnorm4x8";     callArgs = 1; break;
    case llvm::Intrinsic::gla_fUnpackSnorm4x8:   callString = "unpackSnorm4x8";     callArgs = 1; break;
    case llvm::Intrinsic::gla_fPackDouble2x32:   callString = "packDouble2x32";     callArgs = 1; break;
    case llvm::Intrinsic::gla_fUnpackDouble2x32: callString = "unpackDouble2x32";   callArgs = 1; break;

    // Geometry
    case llvm::Intrinsic::gla_fLength:      callString = "length";      callArgs = 1; break;
    case llvm::Intrinsic::gla_fDistance:    callString = "distance";    callArgs = 2; break;
    case llvm::Intrinsic::gla_fDot:         callString = "dot";         callArgs = 2; break;
    case llvm::Intrinsic::gla_fCross:       callString = "cross";       callArgs = 2; break;
    case llvm::Intrinsic::gla_fNormalize:   callString = "normalize";   callArgs = 1; break;
    case llvm::Intrinsic::gla_fNormalize3D: callString = "normalize3D"; break; //     callArgs =
    case llvm::Intrinsic::gla_fLit:         callString = "fLit";        break; //     callArgs =
    case llvm::Intrinsic::gla_fFaceForward: callString = "faceforward"; callArgs = 3; break;
    case llvm::Intrinsic::gla_fReflect:     callString = "reflect";     callArgs = 2; break;
    case llvm::Intrinsic::gla_fRefract:     callString = "refract";     callArgs = 3; break;

    // Derivative and Transform
    case llvm::Intrinsic::gla_fDFdx:           callString = "dFdx";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fDFdy:           callString = "dFdy";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fFilterWidth:    callString = "fwidth";     callArgs = 1; break;
    case llvm::Intrinsic::gla_fFixedTransform: callString = "ftransform"; break; // callArgs =

    // Vector Logical
    case llvm::Intrinsic::gla_not: callString = "not"; callArgs = 1; break;
    case llvm::Intrinsic::gla_any: callString = "any"; callArgs = 1; break;
    case llvm::Intrinsic::gla_all: callString = "all"; callArgs = 1; break;
    }

    if (callString == 0 || callArgs == 0)
        UnsupportedFunctionality("Intrinsic in Bottom IR");
    if (callArgs != llvmInstruction->getNumArgOperands())
        UnsupportedFunctionality("Intrinsic argument count: ", llvmInstruction->getNumOperands(), EATContinue);

    newLine();
    mapGlaDestination(llvmInstruction);
    shader << " = " << callString << "(";
    for (unsigned int arg = 0; arg < llvmInstruction->getNumArgOperands(); ++arg) {
        if (arg > 0)
            shader << ", ";
        mapGlaOperand(llvmInstruction->getOperand(arg));
    }
    shader << ");";
}

void gla::GlslTarget::print()
{
    printf("\n// LunarGoo output\n%s\n%s", globalDeclarations.str().c_str(), shader.str().c_str());
}
