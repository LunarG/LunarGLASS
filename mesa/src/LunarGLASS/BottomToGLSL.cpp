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

#include <cstdio>
#include <string>
#include <sstream>
#include <map>
#include <vector>
#include <cstdio>

// LunarGLASS includes
#include "Revision.h"
#include "Exceptions.h"
#include "LunarGLASSLlvmInterface.h"
#include "LunarGLASSBottomIR.h"
#include "LunarGLASSBackend.h"
#include "Manager.h"
#include "GlslTarget.h"
#include "Options.h"

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
        EVQConstant,
        EVQUndef
    };
};

class gla::GlslTarget : public gla::BackEndTranslator {
public:
    GlslTarget()
    {
        indentLevel = 0;
        lastVariable = 20;
        obfuscate = Options.obfuscate;
        if (Options.backendVersion == DefaultBackendVersion)
            version = 130;
        globalDeclarations << "#version " << version << std::endl;
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

        declareVariable(type, global->getNameStr(), mapGlaAddressSpace(global));
    }

    void startFunctionDeclaration(const llvm::Type* type, const std::string& name)
    {
        newLine();
        emitGlaType(shader, type->getContainedType(0));
        shader << " " << name << "(";
    }

    virtual void addArgument(const llvm::Value* value, bool last)
    {
        emitGlaValue(value);
        if (! last)
            shader << ", ";
    }

    void endFunctionDeclaration()
    {
        shader << ")";
    }

    void startFunctionBody()
    {
        newLine();
        newScope();
    }

    void endFunctionBody()
    {
        leaveScope();
    }

    void add(const llvm::Instruction* llvmInstruction, bool lastBlock);

    void declarePhiCopy(const llvm::Value* dst)
    {
        newLine();
        emitGlaValue(dst);
        shader << ";";
    }

    void addPhiCopy(const llvm::Value* dst, const llvm::Value* src)
    {
        newLine();
        emitGlaValue(dst);
        shader << " = ";
        emitGlaOperand(src);
        shader << ";";
    }

    void addIf(const llvm::Value* cond)
    {
        newLine();
        shader << "if (";
        emitGlaOperand(cond);
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
        if (obfuscate) {
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

    void mapGlaIntrinsic(const llvm::IntrinsicInst*);
    void mapGlaCall(const llvm::CallInst*);
    const char* mapGlaXor(const llvm::Instruction* llvmInstruction, bool intrinsic = false, int* unaryOperand = 0);

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

        // Check for an undef before a constant (since Undef is a
        // subclass of Constant)
        if (Util::isUndef(value)) {
            return EVQUndef;
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
                version >= 130 ? string = "in" : string = "varying";  break;
        case EVQOutput:
                version >= 130 ? string = "out": string = "varying";  break;
        case EVQTemporary:       string = "temp";                     break;
        case EVQConstant:        string = "const";                    break;
        case EVQUndef:           string = "undef";                    break;
        default:
            assert(! "unknown VariableQualifier");
        }

        return string;
    }

    void emitGlaOperand(const llvm::Value* value)
    {
        emitGlaValue(value);
        if (obfuscate) {
            int count = Util::getComponentCount(value);
            if (count > 1)
                emitComponentCountToSwizzle(count);
        }
    }

    void emitComponentCountToSwizzle(int numComponents)
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

    void emitComponentToSwizzle(int component)
    {
        shader << mapComponentToSwizzleChar(component);
    }

    void emitMaskToSwizzle(int mask)
    {
        for (int component = 0; component < 4; ++component)
            if (mask & (1 << component))
                shader << mapComponentToSwizzleChar(component);
    }

    const char* mapComponentToSwizzleChar(int component)
    {
        switch (component) {
        case 0:   return "x";
        case 1:   return "y";
        case 2:   return "z";
        case 3:   return "w";
        default:  assert(! "Vector too large");
        }

        return "x";
    }

    void emitGlaSamplerType(const llvm::Value* samplerType)
    {
        int sampler = Util::getConstantInt(samplerType) ;
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

    void emitGlaTextureStyle(const llvm::IntrinsicInst* llvmInstruction)
    {
        // Check flags for proj/lod/offset
        int flags = Util::getConstantInt(llvmInstruction->getOperand(FlagLocAOS));

        gla::ETextureFlags texFlags = *(gla::ETextureFlags*)&flags;

        if (texFlags.EProjected)
            shader << "Proj";
        else if (texFlags.ELod)
            shader << "Lod";

        if(Util::isGradientTexInst(llvmInstruction))
            shader << "Grad";
    }

    bool needsBiasLod(const llvm::IntrinsicInst* llvmInstruction)
    {
        // Check flags for bias/lod
        int flags = Util::getConstantInt(llvmInstruction->getOperand(FlagLocAOS));

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
        if (obfuscate) {
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
            if (Util::isTempName(value->getNameStr())) {
                varString->append(mapGlaToQualifierString(mapGlaAddressSpace(value)));
                snprintf(buf, bufSize, "%d", lastVariable);
                varString->append(buf);
            } else {
                varString->append(value->getNameStr());
                // LLVM uses "." for phi'd symbols, change to _ so it's parseable by GLSL
                for (int c = 0; c < varString->length(); ++c) {
                    if ((*varString)[c] == '.')
                        (*varString)[c] = '_';
                }
            }
        }
    }

    void declareVariable(const llvm::Type* type, const std::string& varString, EVariableQualifier vq, const llvm::Constant* constant = 0)
    {
        if (varString.substr(0,3) == std::string("gl_"))
            return;

        // If it has an initializer (is a constant and not an undef)
        if (constant && Util::isDefined(constant)) {
            globalDeclarations << mapGlaToQualifierString(vq);
            globalDeclarations << " ";
            emitGlaType(globalDeclarations, type);

            globalDeclarations << " " << varString << " = ";

            switch(constant->getType()->getTypeID()) {
            case llvm::Type::IntegerTyID:
            case llvm::Type::FloatTyID:
                emitScalarConstant(globalDeclarations, constant);
                break;

            case llvm::Type::VectorTyID:
                emitVectorConstant(globalDeclarations, constant);
                break;

            default:
                UnsupportedFunctionality("constant type in Bottom IR", EATContinue);
                globalDeclarations << 0;
            }

            globalDeclarations << ";" << std::endl;
            return;
        }

        // no initializer
        switch (vq) {
        case EVQUniform:
        case EVQConstant:
        case EVQInput:
            globalDeclarations << mapGlaToQualifierString(vq);
            if (varString.find_first_of(' ') == std::string::npos) {
                globalDeclarations << " ";
                emitGlaType(globalDeclarations, type);
            }
            globalDeclarations << " " << varString << ";" << std::endl;
            break;
        case EVQGlobal:
            emitGlaType(globalDeclarations, type);
            globalDeclarations << " " << varString << ";" << std::endl;
            break;
        case EVQTemporary:
            emitGlaType(shader, type);
            shader << " ";
            break;
        case EVQUndef:
            emitGlaType(globalDeclarations, type);
            globalDeclarations << " " << varString << ";" << std::endl;
            break;
        default:
            assert(! "unknown VariableQualifier");
        }
    }

    void emitGlaType(std::ostringstream& out, const llvm::Type* type, int count = -1)
    {
        // if it's a vector, output a vector type
        if (type->getTypeID() == llvm::Type::VectorTyID) {
            const llvm::VectorType *vectorType = llvm::dyn_cast<llvm::VectorType>(type);
            assert(vectorType);

            if (type->getContainedType(0) == type->getFloatTy(type->getContext()))
                out << "vec";
            else if (type->getContainedType(0) == type->getInt1Ty(type->getContext()))
                out << "bvec";
            else if (type->getContainedType(0) == type->getInt32Ty(type->getContext()))
                out << "ivec";
            else
                UnsupportedFunctionality("Basic Type in Bottom IR");

            // output the size of the vecto
            if (count == -1)
                out << Util::getComponentCount(type);
            else
                out << count;
        } else {
            // just output a scalar
            if (type == type->getFloatTy(type->getContext()))
                out << "float";
            else if (type == type->getInt1Ty(type->getContext()))
                out << "bool";
            else if (type == type->getInt32Ty(type->getContext()))
                out << "int";
            else if (type == type->getVoidTy(type->getContext()))
                out << "void";
            else
                UnsupportedFunctionality("Basic Type in Bottom IR");
        }
    }

    void mapGlaValue(const llvm::Value* value)
    {
        if (valueMap[value] == 0) {
            std::string* newVariable = new std::string;
            getNewVariable(value, newVariable);
            declareVariable(value->getType(), *newVariable, mapGlaAddressSpace(value), llvm::dyn_cast<llvm::Constant>(value));
            valueMap[value] = newVariable;
        }
    }

    void emitGlaValue(const llvm::Value* value)
    {
        mapGlaValue(value);
        shader << valueMap[value]->c_str();
    }

    void emitScalarConstant(std::ostringstream& out, const llvm::Constant* constant)
    {
        assert(constant);
        switch(constant->getType()->getTypeID()) {
        case llvm::Type::IntegerTyID:
            {
                const llvm::ConstantInt *constantInt = llvm::dyn_cast<llvm::ConstantInt>(constant);

                if (constantInt->getBitWidth() == 1) {
                    if (constantInt->isZero())
                        out << "false";
                    else
                        out << "true";
                } else
                    out << Util::getConstantInt(constant);
            }
            break;

        case llvm::Type::FloatTyID:
            out << Util::GetConstantFloat(constant);
            break;

        default:
            UnsupportedFunctionality("constant type in Bottom IR", EATContinue);
            out << 0;
        }
    }

    void emitVectorConstant(std::ostringstream& out, const llvm::Constant* constant)
    {
        assert(constant);
        assert(Util::isDefined(constant));
        const llvm::ConstantVector* vector = llvm::dyn_cast<llvm::ConstantVector>(constant);
        if (vector) {
            emitGlaType(out, vector->getType());
            out << "(";

            // are they all the same?
            bool same = true;
            for (int op = 1; op < vector->getNumOperands(); ++op) {
                if (llvm::dyn_cast<const llvm::Constant>(vector->getOperand(0)) != llvm::dyn_cast<const llvm::Constant>(vector->getOperand(op))) {
                    same = false;
                    break;
                }
            }

            // write out the constants
            if (same)
                emitScalarConstant(out, llvm::dyn_cast<const llvm::Constant>(vector->getOperand(0)));
            else {
                for (int op = 0; op < vector->getNumOperands(); ++op) {
                    if (op > 0)
                        out << ", ";
                    emitScalarConstant(out, llvm::dyn_cast<const llvm::Constant>(vector->getOperand(op)));
                }
            }

            out << ")";
            return;
        }

        const llvm::ConstantAggregateZero* aggregate = llvm::dyn_cast<llvm::ConstantAggregateZero>(constant);
        if (aggregate) {
            emitGlaType(out, constant->getType());
            out << "(0)";
            return;
        }

        UnsupportedFunctionality("Vector Constant");
    }

    bool addNewVariable(const llvm::Value* value, std::string name)
    {
        if (valueMap[value] == 0) {
            int spaceLoc = name.find_first_of(' ');
            if (spaceLoc == std::string::npos)
                valueMap[value] = new std::string(name);  //?? need to delete these?
            else
                valueMap[value] = new std::string(name.substr(spaceLoc+1));
            return true;
        } else {
            assert(name == *valueMap[value]);
            return false;
        }
    }

    void emitGlaSwizzle(int glaSwizzle, int width)
    {
        shader << ".";
        // Pull each two bit channel out of the integer
        for(int i = 0; i < width; i++)
            emitComponentToSwizzle((glaSwizzle >> i*2) & 0x3);
    }

    // Whether the given intrinsic's specified operand is the same as the passed
    // value, and its type is a vector.
    bool isSameSource(llvm::Value *source, const llvm::IntrinsicInst *inst, int operand)
    {
        return (inst->getOperand(operand) == source)
            && (source->getType()->getTypeID() == llvm::Type::VectorTyID);
    }

    // Writes out the vector arguments for the RHS of a multiInsert. Sets its
    // first argument to false upon first execution
    void writeVecArgs(bool &firstArg, const llvm::IntrinsicInst *inst, int operand)
    {
        if (firstArg)
            firstArg = false;
        else
            shader << ", ";

        emitGlaValue(inst->getOperand(operand));

        // If it's a vector, extract the value
        if (inst->getOperand(operand)->getType()->getTypeID() == llvm::Type::VectorTyID) {
            shader << ".";
            emitComponentToSwizzle(Util::getConstantInt(inst->getOperand(operand+1)));
        }
    }

    // Returns a pointer to the common source of the multiinsert if they're all
    // the same, otherwise returns null.
    llvm::Value* getCommonSourceMultiInsert(const llvm::IntrinsicInst* inst) {
        llvm::Value* source = NULL;
        bool sameSource = true;
        int wmask = Util::getConstantInt(inst->getOperand(1));

        for (int i = 0; i < 4; ++i) {
            if (wmask & (1 << i)) {
                int operandIndex = (i+1) * 2;
                if (source)
                    sameSource = sameSource && isSameSource(source, inst, operandIndex);
                else
                    source = inst->getOperand(operandIndex);
            }
        }

        return sameSource ? source : NULL;

    }

    void emitGlaMultiInsertRHS(const llvm::IntrinsicInst* inst)
    {
        int wmask = Util::getConstantInt(inst->getOperand(1));
        assert(wmask <= 0xF);
        int argCount = 0;

        // Count the args
        for (int i = 0; i < 4; ++i) {
            if (wmask & (1 << i))
                ++argCount;
        }

        // If they're all from the same source just use/swizzle it. Otherwise
        // construct a new vector
        llvm::Value* source = getCommonSourceMultiInsert(inst);
        if (source) {
            emitGlaValue(source);

            // Build up the rhs mask
            int singleSourceMask = 0;
            for (int i = 0, pos = 0; i < 4; ++i) {
                // If it's not -1, then add it to our swizzle.
                int swizOffset = Util::getConstantInt(inst->getOperand(i*2 + 3));
                if (swizOffset != -1) {
                    singleSourceMask |= ( swizOffset << (pos*2));
                    ++pos;
                }
            }
            assert (singleSourceMask <= 0xFF);
            emitGlaSwizzle(singleSourceMask, argCount);
        } else {
            emitGlaType(shader, inst->getType(), argCount);
            shader << "(";
            bool firstArg = true;

            for (int i = 0; i < 4; ++i) {
                if (wmask & (1 << i)) {
                    int operandIndex = (i+1) * 2;
                    writeVecArgs(firstArg, inst, operandIndex);
                }
            }

            shader << ")";
        }
    }

    void emitGlaMultiInsert(const llvm::IntrinsicInst* inst)
    {
        int wmask = Util::getConstantInt(inst->getOperand(1));

        newLine();

        // Declare it.
        emitGlaValue(inst);

        llvm::Value* op = inst->getOperand(0);

        // If the writemask is full, then just initialize it, and we're done
        if (wmask == 0xF) {
            shader << " = ";
            emitGlaMultiInsertRHS(inst);
            shader << ";";
            return;
        }

        // If the origin is defined, initialize the new instruction to be the
        // origin. If undefined, leave it declared and uninitialized.
        if (Util::isDefined(op)) {
            // Initialize it to be the origin.
            shader << " = ";
            emitGlaValue(op);
        }

        shader << ";";
        newLine();

        // If wmask is not all 1s, then do a lhs swizzle
        emitGlaValue(inst);
        if (wmask != 0xF) {
            shader << ".";
            emitMaskToSwizzle(wmask);
        }
        shader << " = ";

        emitGlaMultiInsertRHS(inst);

        // Finished with the statement
        shader << ";";
    }

    // mapping from LLVM values to Glsl variables
    std::map<const llvm::Value*, std::string*> valueMap;

    std::ostringstream globalDeclarations;
    std::ostringstream shader;
    int indentLevel;
    int lastVariable;
    bool obfuscate;
    int version;
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
void gla::GlslTarget::add(const llvm::Instruction* llvmInstruction, bool lastBlock)
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
    case llvm::Instruction::SRem:           charOp = "%";  break;
    case llvm::Instruction::Shl:            charOp = "<<"; break;
    case llvm::Instruction::LShr:           charOp = ">>"; break;
    case llvm::Instruction::AShr:           charOp = ">>"; break;
    case llvm::Instruction::And:            charOp = "&";  break;
    case llvm::Instruction::Or:             charOp = "|";  break;
    case llvm::Instruction::Xor:            charOp = mapGlaXor(llvmInstruction); break;
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
        emitGlaValue(llvmInstruction);
        shader << " = ";
        emitGlaOperand(llvmInstruction->getOperand(0));
        shader << " " << charOp << " ";
        emitGlaOperand(llvmInstruction->getOperand(1));
        shader << ";";
        return;
    }

    //
    // Look for unary ops, where the form would be "op operand"
    //

    // LLVM turned these into a binary ops, might want to undo that...
    int unaryOperand;
    switch (llvmInstruction->getOpcode()) {
    case llvm::Instruction::Xor:
        charOp = mapGlaXor(llvmInstruction, false /* intrinsic */, &unaryOperand);
        break;
    }

    // Handle the unary ops
    if (charOp) {
        newLine();
        emitGlaValue(llvmInstruction);
        shader << " = " << charOp << " ";
        emitGlaOperand(llvmInstruction->getOperand(unaryOperand));
        shader << ";";
        return;
    }

    //
    // Look for unary ops, where the form would be "op(operand)"
    //

    unaryOperand = 0;
    switch (llvmInstruction->getOpcode()) {
    case llvm::Instruction::FPTrunc:        charOp = "trunc";  break;
    case llvm::Instruction::FPToUI:         charOp = "uint";   break;
    case llvm::Instruction::FPToSI:         charOp = "int";    break;
    case llvm::Instruction::UIToFP:         charOp = "float";  break;
    case llvm::Instruction::SIToFP:         charOp = "float";  break;
    case llvm::Instruction::Xor:
        charOp = mapGlaXor(llvmInstruction, true /* intrinsic */, &unaryOperand);
        break;
    default:
        break;
        // fall through to check other ops
    }

    // Handle the unary ops
    if (charOp) {
        newLine();
        emitGlaValue(llvmInstruction);
        shader << " = " << charOp << "(";
        emitGlaOperand(llvmInstruction->getOperand(unaryOperand));
        shader << ");";
        return;
    }

    //
    // Handle remaining ops
    //

    switch (llvmInstruction->getOpcode()) {

    case llvm::Instruction::Ret:
        newLine();
        if (! lastBlock && llvmInstruction->getNumOperands() == 0) {
            shader << "return;";
        } else if (llvmInstruction->getNumOperands() > 0) {
            shader << "return ";
            emitGlaOperand(llvmInstruction->getOperand(0));
            shader << ";";
        }
        return;

    case llvm::Instruction::Call: // includes intrinsics...
        if (const llvm::IntrinsicInst* intrinsic = llvm::dyn_cast<llvm::IntrinsicInst>(llvmInstruction)) {
            mapGlaIntrinsic(intrinsic);
        } else {
            const llvm::CallInst* call = llvm::dyn_cast<llvm::CallInst>(llvmInstruction);
            assert(call);
            mapGlaCall(call);
        }
        return;

    case llvm::Instruction::FRem:
        newLine();
        emitGlaValue(llvmInstruction);
        shader << " = mod(";
        emitGlaOperand(llvmInstruction->getOperand(0));
        shader << ", ";
        emitGlaOperand(llvmInstruction->getOperand(1));
        shader << ");";
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
            emitGlaValue(llvmInstruction);
            shader << " = " << charOp << "(";
            emitGlaOperand(llvmInstruction->getOperand(0));
            shader << ", ";
            emitGlaOperand(llvmInstruction->getOperand(1));
            shader << ");";
        }
        return;

    case llvm::Instruction::Load:
        addNewVariable(llvmInstruction, llvmInstruction->getOperand(0)->getNameStr());
        return;

    case llvm::Instruction::Alloca:
        newLine();
        emitGlaValue(llvmInstruction);
        shader << ";";
        return;

    case llvm::Instruction::Store:
        if (llvm::isa<llvm::PointerType>(llvmInstruction->getOperand(1)->getType())) {
            newLine();
            emitGlaValue(llvmInstruction->getOperand(1));
            shader << " = ";
            emitGlaOperand(llvmInstruction->getOperand(0));
            shader << ";";
        } else {
            assert(! "store instruction is not through pointer");
        }
        return;

    case llvm::Instruction::ExtractElement:
        {
            mapGlaValue(llvmInstruction->getOperand(0));

            // copy propagate, by name string, the extracted component
            std::string swizzled = *valueMap[llvmInstruction->getOperand(0)];
            swizzled.append(".").append(mapComponentToSwizzleChar(Util::getConstantInt(llvmInstruction->getOperand(1))));
            addNewVariable(llvmInstruction, swizzled.c_str());
        }
        return;

    case llvm::Instruction::InsertElement:
        // copy propagate, by name string the, the starting name of the object
        // addNewVariable(llvmInstruction, valueMap[llvmInstruction->getOperand(0)]->c_str());

        // first, copy whole the structure "inserted into" to the resulting "value" of the insert
        newLine();
        emitGlaValue(llvmInstruction);

        shader << " = ";
        emitGlaOperand(llvmInstruction->getOperand(0));
        shader << ";";

        // second, overwrite the element being inserted
        newLine();
        emitGlaValue(llvmInstruction);
        shader << ".";
        emitComponentToSwizzle(Util::getConstantInt(llvmInstruction->getOperand(2)));
        shader << " = ";
        emitGlaOperand(llvmInstruction->getOperand(1));
        shader << ";";
        return;

    default:
        UnsupportedFunctionality("Opcode in Bottom IR: ", llvmInstruction->getOpcode(), EATContinue);
    }
}

//
// Xor is a strange thing:
//  - for scalar Booleans, it looks like "^^"
//  - for vector Booleans, GLSL doesn't have one
//  - for scalar and vector integers, it looks like "^"
//  - if an integer operand is all 1s, it can be represented as unary "~" on the other operand
//  - if a scalar Boolean operand is true, it can be represented as unary "!" on the other operand
//  - if a vector Boolean operand is all true, it can be represented as "not(...)"
//
// Assumes things are tried in the order
//  1.  binary op
//  2.  unary op
//  3.  intrinsic
//
const char* gla::GlslTarget::mapGlaXor(const llvm::Instruction* llvmInstruction, bool intrinsic, int* unaryOperand)
{
    bool scalar = gla::Util::isGlaScalar(llvmInstruction->getType());
    bool boolean = gla::Util::isGlaBoolean(llvmInstruction->getType());

    bool op0AllSet = Util::hasAllSet(llvmInstruction->getOperand(0));
    bool op1AllSet = Util::hasAllSet(llvmInstruction->getOperand(1));

    if (unaryOperand == 0) {
        // try a binary op

        // if it could be done as a unary op, return 0 so that can happen later
        if (op0AllSet || op1AllSet)
            return 0;

        if (scalar && boolean)
            return "^^";

        if (!boolean)
            return "^";

        UnsupportedFunctionality("xor", EATContinue);
        return "^";
    } else {
        // unary; either an op or an intrinsic

        assert(!op0AllSet && !op0AllSet);

        if (op0AllSet)
            *unaryOperand = 1;
        else
            *unaryOperand = 0;

        if (scalar && boolean)
            return "!";

        if (!boolean)
            return "~";

        if (intrinsic)
            return "not";

        return 0;
    }
}

//
// Handle the subcase of an LLVM instruction being an intrinsic call.
//
void gla::GlslTarget::mapGlaIntrinsic(const llvm::IntrinsicInst* llvmInstruction)
{
    // Handle pipeline read/write
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_fWriteData:
        switch (Util::getConstantInt(llvmInstruction->getOperand(0)))
        {
        case 0:
            newLine();
            shader << "gl_FragColor = ";
            emitGlaOperand(llvmInstruction->getOperand(1));
            shader << ";";
            return;
        default:
            UnsupportedFunctionality("Unhandled data output variable in Bottom IR: ", Util::getConstantInt(llvmInstruction->getOperand(0)));
        }
        return;

    case llvm::Intrinsic::gla_readData:
    case llvm::Intrinsic::gla_fReadInterpolant:
        if (addNewVariable(llvmInstruction, llvmInstruction->getNameStr())) {
            declareVariable(llvmInstruction->getType(), llvmInstruction->getNameStr(), EVQInput);
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
        emitGlaValue(llvmInstruction);
        shader << " = ";
        emitGlaSamplerType(llvmInstruction->getOperand(0));
        emitGlaTextureStyle(llvmInstruction);
        shader << "(";
        emitGlaOperand(llvmInstruction->getOperand(SamplerLocAOS));
        shader << ", ";
        emitGlaOperand(llvmInstruction->getOperand(CoordLocAOS));

        if(needsBiasLod(llvmInstruction)) {
            shader << ", ";
            emitGlaOperand(llvmInstruction->getOperand(BiasLocAOS));
        }

        if(Util::isGradientTexInst(llvmInstruction)) {  //?? this can move to a place they are shared between back-ends
            shader << ", ";
            emitGlaOperand(llvmInstruction->getOperand(DdxLocAOS));
            shader << ", ";
            emitGlaOperand(llvmInstruction->getOperand(DdyLocAOS));
        }

        shader << ");";

        return;
    }

    // Handle swizzles
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_swizzle:
    case llvm::Intrinsic::gla_fSwizzle:
        newLine();
        emitGlaValue(llvmInstruction);
        shader << " = ";

        // Case 0:  it's scalar making a scalar.
        // use nothing, just copy
        if (Util::getComponentCount(llvmInstruction->getOperand(0)) == 1 && Util::getComponentCount(llvmInstruction) == 1) {
            emitGlaOperand(llvmInstruction->getOperand(0));
            shader << ";";
            return;
        }

        // Case 1:  it's a scalar with multiple ".x" to expand it to a vector.
        // use a constructor to turn a scalar into a vector
        if (Util::getComponentCount(llvmInstruction->getOperand(0)) == 1 && Util::getComponentCount(llvmInstruction) > 1) {
            emitGlaType(shader, llvmInstruction->getType());
            shader << "(";
            emitGlaOperand(llvmInstruction->getOperand(0));
            shader << ");";
            return;
        }

        // Case 2:  it's sequential .xy...  subsetting a vector.
        // use a constructor to subset the vectorto a vector
        if (Util::getComponentCount(llvmInstruction->getOperand(0)) > 1 && Util::getComponentCount(llvmInstruction) > 1 &&
            Util::isConsecutiveSwizzle(Util::getConstantInt(llvmInstruction->getOperand(1)), Util::getComponentCount(llvmInstruction))) {

            emitGlaType(shader, llvmInstruction->getType());
            shader << "(";
            emitGlaOperand(llvmInstruction->getOperand(0));
            shader << ");";
            return;
        }

        // Case 3:  it's a non-sequential subsetting of a vector.
        // use GLSL swizzles
        emitGlaOperand(llvmInstruction->getOperand(0));
        if (Util::getComponentCount(llvmInstruction->getOperand(0)) > 1)
            emitGlaSwizzle(Util::getConstantInt(llvmInstruction->getOperand(1)), Util::getComponentCount(llvmInstruction));
        shader << ";";
        return;
    }

    // Handle multiInserts
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_fMultiInsert:
    case llvm::Intrinsic::gla_multiInsert:
        emitGlaMultiInsert(llvmInstruction);
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
    emitGlaValue(llvmInstruction);
    shader << " = " << callString << "(";
    for (unsigned int arg = 0; arg < llvmInstruction->getNumArgOperands(); ++arg) {
        if (arg > 0)
            shader << ", ";
        emitGlaOperand(llvmInstruction->getOperand(arg));
    }
    shader << ");";
}

//
// Handle real function calls.
//
void gla::GlslTarget::mapGlaCall(const llvm::CallInst* call)
{
    newLine();
    emitGlaValue(call);
    shader << " = " << call->getCalledFunction()->getNameStr() << "(";
    for (int arg = 0; arg < call->getNumArgOperands(); ++arg) {
        emitGlaOperand(call->getArgOperand(arg));
        if (arg + 1 < call->getNumArgOperands())
            shader << ", ";
    }

    shader << ");";
}

void gla::GlslTarget::print()
{
    // If we don't have the noRevision options
    // set, then output the revision.

    if (Options.noRevision)
        printf("\n// LunarGOO output\n");
    else
        printf("\n// LunarGOO(r%d) output\n", GLA_REVISION);

    printf("%s%s", globalDeclarations.str().c_str(), shader.str().c_str());
}
