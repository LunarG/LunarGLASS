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
#include <set>
#include <vector>
#include <cstdio>

// LunarGLASS includes
#include "Revision.h"
#include "Exceptions.h"
#include "Util.h"
#include "BottomIR.h"
#include "Backend.h"
#include "PrivateManager.h"
#include "GlslTarget.h"
#include "Options.h"

// LLVM includes
#include "llvm/Module.h"

namespace gla {
    bool IsIdentitySwizzle(int glaSwizzle, int width)
    {
        for (int i = 0; i < width; ++i) {
            if (GetSwizzle(glaSwizzle, i) != i)
                return false;
        }

        return true;
    }

    bool IsIdentitySwizzle(const llvm::SmallVectorImpl<llvm::Constant*>& elts)
    {
        for (int i = 0; i < elts.size(); ++i) {
            if (IsUndef(elts[i]) || i != GetConstantInt(elts[i])) {
                return false;
            }
        }

        return true;
    }
}

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

    virtual bool decomposeNaNCompares()
    {
        return true;
    }

    virtual bool hoistDiscards()
    {
        return false;
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
    GlslTarget(Manager* m) : BackEndTranslator(m), appendInitializers(false), indentLevel(0), lastVariable(20),
                             obfuscate(Options.obfuscate)
    {
        if (Options.backendVersion == DefaultBackendVersion)
            version = 130;
        globalDeclarations << "#version " << version << std::endl;
    }

    ~GlslTarget()
    {
    }

    void addStructType(const std::string name, const llvm::Type* structType)
    {
        structNameMap[structType] = name;
        globalDeclarations << "struct " << name << " {" << std::endl;

        for (int index = 0; index < structType->getNumContainedTypes(); ++index) {
            globalDeclarations << "    ";
            emitGlaType(globalDeclarations, structType->getContainedType(index), -1);
            globalDeclarations << " " << getGlaStructField(structType, index);
            globalDeclarations << ";" << std::endl;
        }

        globalDeclarations << "};" << std::endl << std::endl;
    }

    void addGlobal(const llvm::GlobalVariable* global)
    {
        const llvm::Type* type;
        if (const llvm::PointerType* pointer = llvm::dyn_cast<llvm::PointerType>(global->getType()))
            type = pointer->getContainedType(0);
        else
            type = global->getType();

        std::string name  = global->getNameStr();
        makeParseable(name);
        addNewVariable(global, name);
        declareVariable(type, name, mapGlaAddressSpace(global));

        if (global->hasInitializer()) {
            llvm::Constant* constant = global->getInitializer();
            emitInitializeAggregate(globalInitializers, global->getNameStr(), constant);
        }
    }

    void addOutputs(const gla::PipelineSymbols& outputs)
    {
        for (int i = 0; i < outputs.size(); ++i)
            declareVariable(outputs[i].type, outputs[i].name, EVQOutput);
    }

    void startFunctionDeclaration(const llvm::Type* type, const std::string& name)
    {
        newLine();
        emitGlaType(shader, type->getContainedType(0));
        shader << " " << name << "(";

        if (name == std::string("main"))
            appendInitializers = true;
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

        if (appendInitializers && globalInitializers.str().length() > 0) {
            // Reset bool so these are only emitted in main()
            appendInitializers = false;
            shader << globalInitializers.str();
            newLine();
        }
    }

    void endFunctionBody()
    {
        leaveScope();
    }

    void add(const llvm::Instruction* llvmInstruction, bool lastBlock, bool referencedOutsideScope=false);

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

    void addIf(const llvm::Value* cond, bool invert=false)
    {
        newLine();
        shader << "if (";

        if (invert)
            shader << "! ";

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

    void beginConditionalLoop()
    {
        UnsupportedFunctionality("conditional loops");
    }

    void beginSimpleConditionalLoop(const llvm::CmpInst* cmp, const llvm::Value* op1, const llvm::Value* op2, bool invert=false)
    {
        newLine();
        std::string str;
        std::string opStr;
        int pos = -1;
        getOp(cmp, opStr, pos);

        bool binOp = false;
        if (pos == -1)
            binOp = true;

        // TODO: add support for unary ops (and xor)

        if (! binOp)
            UnsupportedFunctionality("unary op for simple conditional loops");

        shader << "while (";

        if (invert)
            shader << "! (";

        if (const llvm::Instruction* opInst1 = llvm::dyn_cast<llvm::Instruction>(op1)) {
            getExtractElementStr(opInst1, str);
            if (! str.empty())
                shader << str;
            else
                emitGlaValue(op1);
        } else
            emitGlaValue(op1);
        str.clear();

        shader << " " << opStr << " ";

        if (const llvm::Instruction* opInst2 = llvm::dyn_cast<llvm::Instruction>(op2)) {
            getExtractElementStr(opInst2, str);
            if (! str.empty())
                shader << str;
            else
                emitGlaValue(op2);
        } else
            emitGlaValue(op2);
        str.clear();

        if (invert)
            shader << ")";

        shader << ")";

        newScope();
    }

    void beginSimpleInductiveLoop(const llvm::PHINode* phi, unsigned count)
    {
        newLine();

        shader << "for (";
        emitGlaValue(phi);

        shader << " = 0; ";

        emitGlaValue(phi);
        shader << " < " << count;

        shader << "; ++";
        emitGlaValue(phi);
        shader << ") ";

        newScope();
    }

    void beginSimpleInductiveLoop(const llvm::PHINode* phi, const llvm::Value* count)
    {
        newLine();

        shader << "for (";
        emitGlaValue(phi);

        shader << " = 0; ";

        emitGlaValue(phi);
        shader << " < ";
        emitGlaValue(count);

        shader << "; ++";
        emitGlaValue(phi);
        shader << ") ";

        newScope();
    }

    void beginInductiveLoop()
    {
        UnsupportedFunctionality("inductive loops");
    }

    void beginLoop()
    {
        newLine();
        shader << "while (true) ";

        newScope();
    }

    void endLoop()
    {
        leaveScope();
    }

    void addLoopExit(const llvm::Value* condition=NULL, bool invert=false)
    {
        if (condition)
            addIf(condition, invert);

        newLine();
        shader << "break;";

        if (condition)
            addEndif();
    }

    void addLoopBack(const llvm::Value* condition=NULL, bool invert=false)
    {
        if (condition)
            addIf(condition, invert);

        newLine();
        shader << "continue;";

        if (condition)
            addEndif();
    }

    void addDiscard()
    {
        newLine();
        shader << "discard;";
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

    void getOp(const llvm::Instruction* llvmInstruction, std::string& s, int& unaryOperand);

    void mapGlaCall(const llvm::CallInst*);
    const char* mapGlaXor(const llvm::Instruction* llvmInstruction, int& unaryOperand);

    EVariableQualifier mapGlaAddressSpace(const llvm::Value* value)
    {
        if (const llvm::PointerType* pointer = llvm::dyn_cast<llvm::PointerType>(value->getType())) {
            switch (pointer->getAddressSpace()) {
            case gla::ResourceAddressSpace:
                return EVQUniform;
            case gla::GlobalAddressSpace:
                return EVQGlobal;
            default:
                if (pointer->getAddressSpace() >= gla::ConstantAddressSpaceBase)
                    return EVQUniform;

                UnsupportedFunctionality("Address Space in Bottom IR: ", pointer->getAddressSpace());
            }
        }

        if (llvm::isa<llvm::Instruction>(value)) {
            return EVQTemporary;
        }

        // Check for an undef before a constant (since Undef is a
        // subclass of Constant)
        if (AreAllUndefined(value)) {
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
            int count = GetComponentCount(value);
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

    void emitGlaSamplerType(const llvm::IntrinsicInst* llvmInstruction, int texFlags)
    {
        const char *texture;
        const llvm::Value* samplerType = llvmInstruction->getOperand(0);

        // Select texture type based on GLA flag

        if (texFlags & ETFFetch) {
            shader << "texelFetch";

            // For 1.3 and beyond texture functions, no need for the
            // extra logic below, so just return

            return;
        }

        if (IsGradientTexInst(llvmInstruction) || texFlags & ETFOffsetArg) {
            // This opcodes are only available with 1.3 and beyond, so
            // skip the legacy dimension code below.
            shader << "texture";

            return;
        }


        if (texFlags & ETFShadow)
            texture = "shadow";
        else
            texture = "texture";

        int sampler = GetConstantInt(samplerType) ;
        switch(sampler) {
        case ESampler1D:        shader << texture << "1D";  break;
        case ESampler2D:        shader << texture << "2D";  break;
        case ESampler3D:        shader << "texture3D";      break;
        case ESamplerCube:      shader << "textureCube";    break;
        case ESampler2DRect:    shader << "textureRect";    break;
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
        int texFlags = GetConstantInt(llvmInstruction->getOperand(GetTextureOpIndex(ETOFlag)));

        if (texFlags & ETFProjected)
            shader << "Proj";
        if (texFlags & ETFLod)
            shader << "Lod";

        if (IsGradientTexInst(llvmInstruction))
            shader << "Grad";

        if (texFlags & ETFOffsetArg)
            shader << "Offset";
    }

    bool needsShadowRefZArg(const llvm::IntrinsicInst* llvmInstruction)
    {
        // Check flags for RefZ
        int texFlags = GetConstantInt(llvmInstruction->getOperand(GetTextureOpIndex(ETOFlag)));

        return (texFlags & ETFRefZArg);
    }

    bool needsBiasLodArg(const llvm::IntrinsicInst* llvmInstruction)
    {
        // Check flags for bias/lod
        int texFlags = GetConstantInt(llvmInstruction->getOperand(GetTextureOpIndex(ETOFlag)));

        return (texFlags & ETFBiasLodArg);
    }

    bool needsOffsetArg(const llvm::IntrinsicInst* llvmInstruction)
    {
        // Check flags for offset arg
        int texFlags = GetConstantInt(llvmInstruction->getOperand(GetTextureOpIndex(ETOFlag)));

        return (texFlags & ETFOffsetArg);
    }

    void getNewVariable(const llvm::Value* value, std::string* varString)
    {
        ++lastVariable;
        const size_t bufSize = 20;
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
            if (IsTempName(value->getNameStr())) {
                varString->append(mapGlaToQualifierString(mapGlaAddressSpace(value)));
                snprintf(buf, bufSize, "%d", lastVariable);
                varString->append(buf);

                // If it's a constant int or float, make the name contain the
                // value
                if (llvm::isa<llvm::ConstantInt>(value)) {
                    varString->append("_");

                    int val = GetConstantInt(value);

                    // If it's an i1, that is a bool, then have it say true or
                    // false, else have it have the integer value.
                    if (IsBoolean(value->getType()))
                        snprintf(buf, bufSize, val ? "true" : "false");
                    else
                        snprintf(buf, bufSize, "%d", GetConstantInt(value));

                    varString->append(buf);

                } else if (llvm::isa<llvm::ConstantFP>(value)) {
                    varString->append("_");
                    snprintf(buf, bufSize, "%.0f", GetConstantFloat(value));
                    varString->append(buf);
                    varString->append("f");
                }
            } else {
                varString->append(value->getNameStr());
            }

            makeParseable(*varString);

            // Variables starting with gl_ are illegal in GLSL
            if (varString->substr(0,3) == std::string("gl_")) {
                varString->insert(0, "gla_");
            }
        }
    }

    void makeParseable(std::string& varString)
    {
        // LLVM uses "." for phi'd symbols, change to _ so it's parseable by GLSL
        for (int c = 0; c < varString.length(); ++c) {
            if (varString[c] == '.' || varString[c] == '-')
                varString[c] = '_';
        }
    }

    void declareVariable(const llvm::Type* type, const std::string& varString, EVariableQualifier vq, const llvm::Constant* constant = 0)
    {
        if (varString.substr(0,3) == std::string("gl_"))
            return;

        // If it has an initializer (is a constant and not an undef)
        if (constant && ! AreAllUndefined(constant)) {
            globalDeclarations << mapGlaToQualifierString(vq);
            globalDeclarations << " ";
            emitGlaType(globalDeclarations, type);
            globalDeclarations << " " << varString << " = ";
            emitConstantInitializer(globalDeclarations, constant, constant->getType());
            globalDeclarations << ";" << std::endl;
            return;
        }

        // no initializer
        switch (vq) {
        case EVQUniform:
        case EVQConstant:
        case EVQInput:
        case EVQOutput: {
            // If the name has brackets and index, we need to declare an array,
            // not a scalar with a name containing brackets and index.
            int arraySize;
            std::string basename = varString;
            GetArraySizeFromName(varString, basename, arraySize);
            if (arraySize > 0) {
                if (globallyDeclaredArrays.find(basename) != globallyDeclaredArrays.end()) {
                    // we already declared this array
                    break;
                } else {
                    // declare array now,
                    // remember this for next time
                    globallyDeclaredArrays.insert(basename);
                }
            }

            globalDeclarations << mapGlaToQualifierString(vq);

            if (basename.find_first_of(' ') == std::string::npos) {
                globalDeclarations << " ";
                emitGlaType(globalDeclarations, type);
            }
            globalDeclarations << " " << basename;

            if (arraySize > 0)
                globalDeclarations << "[" << arraySize << "]";

            globalDeclarations << ";" << std::endl;
            break;
        }
        case EVQGlobal:
            emitGlaType(globalDeclarations, type);
            globalDeclarations << " " << varString << ";" << std::endl;
            break;
        case EVQTemporary:
            emitGlaType(shader, type);
            shader << " ";
            break;
        case EVQUndef:
            newLine();
            emitGlaType(shader, type);
            shader << " " << varString;
            shader << ";";
            emitInitializeAggregate(shader, varString, constant);
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
                out << GetComponentCount(type);
            else
                out << count;
        } else if (type->getTypeID() == llvm::Type::StructTyID) {
            const llvm::StructType* structType = llvm::dyn_cast<const llvm::StructType>(type);
            out << structNameMap[structType];
        } else if (type->getTypeID() == llvm::Type::ArrayTyID) {
            const llvm::ArrayType* arrayType = llvm::dyn_cast<const llvm::ArrayType>(type);
            emitGlaType(out, arrayType->getContainedType(0));
            out << "[" << arrayType->getNumElements() << "]";
        //} else if (type->getTypeID() == llvm::Type::PointerTyID) {
        //    const llvm::PointerType* pointerType = llvm::dyn_cast<const llvm::PointerType>(type);
        //    emitGlaType(out, pointerType->getContainedType(0));
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

    // If valueMap has no entry for value, generate a name and declaration, and
    // store it in valueMap. If forceGlobal is true, then it will make the
    // declaration occur as a global.
    void mapGlaValue(const llvm::Value* value, bool forceGlobal=false)
    {
        if (valueMap[value] == 0) {
            // Figure out where our declaration should go
            EVariableQualifier evq;
            if (forceGlobal)
                evq = gla::EVQGlobal;
            else if (llvm::isa<llvm::PointerType>(value->getType()))
                evq = gla::EVQTemporary;
            else
                evq = mapGlaAddressSpace(value);

            std::string* newVariable = new std::string;
            getNewVariable(value, newVariable);
            if (const llvm::PointerType* pointerType = llvm::dyn_cast<llvm::PointerType>(value->getType())) {
                declareVariable(pointerType->getContainedType(0), *newVariable, evq);
            } else {
                declareVariable(value->getType(), *newVariable, evq, llvm::dyn_cast<llvm::Constant>(value));
            }
            valueMap[value] = newVariable;
        }
    }

    void emitGlaValue(const llvm::Value* value)
    {
        assert(! llvm::isa<llvm::ConstantExpr>(value));

        mapGlaValue(value);

        shader << valueMap[value]->c_str();
    }


    void emitGlaStructName(std::ostringstream& out, const llvm::Type* structType)
    {
        std::string name = structNameMap[structType];
        assert(name.c_str());
        out << name;
    }

    std::string getGlaStructField(const llvm::Type* structType, int index)
    {
        std::string name;
        const size_t bufSize = 10;
        char buf[bufSize];
        name.append("member");
        snprintf(buf, bufSize, "%d", index);
        name.append(buf);
        return name;
    }

    std::string getGlaValue(const llvm::Value* value)
    {
        mapGlaValue(value);
        return valueMap[value]->c_str();
    }

    void emitFloatConstant(std::ostringstream& out, float f)
    {
        if (floor(f) == f) {
            out << static_cast<int>(floor(f));
            out << ".0";
        } else
            out << f;
    }

    // emitConstantInitializer will be called recursively for aggregate types.
    // If the aggregate is zero initialized, sub-elements will not have a
    // constant associated with them. For that case, and for ConstantAggregateZero,
    // we only use the type to generate correct initializers.
    void emitConstantInitializer(std::ostringstream& out, const llvm::Constant* constant, const llvm::Type* type)
    {
        bool isZero;

        if (! constant || IsUndef(constant))
            isZero = true;
        else if (llvm::isa<llvm::ConstantAggregateZero>(constant))
            isZero = true;
        else
            isZero = false;

        switch (type->getTypeID()) {

        case llvm::Type::IntegerTyID: {
            if (isZero) {
                if (gla::IsBoolean(type))
                        out << "false";
                    else
                    out << "0";
            } else {
                if (gla::IsBoolean(type)) {
                    if (GetConstantInt(constant))
                        out << "true";
                    else
                        out << "false";
                } else
                    out << GetConstantInt(constant);
            }
            break;
        }

        case llvm::Type::FloatTyID: {
            if (isZero)
                emitFloatConstant(out, 0.0);
            else
                emitFloatConstant(out, GetConstantFloat(constant));
            break;
        }

        case llvm::Type::VectorTyID:
        case llvm::Type::ArrayTyID:
        case llvm::Type::StructTyID: {
            emitGlaType(out, type);
            out << "(";

            int numElements = 0;

            if (const llvm::VectorType* vectorType = llvm::dyn_cast<llvm::VectorType>(type)) {
                // If all vector elements are equal, we only need to emit one
                bool same = true;
                if (! isZero) {
                    for (int op = 1; op < vectorType->getNumElements(); ++op) {
                        if (llvm::dyn_cast<const llvm::Constant>(constant->getOperand(0)) != llvm::dyn_cast<const llvm::Constant>(constant->getOperand(op))) {
                            same = false;
                            break;
                        }
                    }
                }
                numElements = same ? 1 : vectorType->getNumElements();
            } else if (const llvm::ArrayType*  arrayType = llvm::dyn_cast<llvm::ArrayType>(type))
                numElements = arrayType->getNumElements();
            else if (const llvm::StructType* structType = llvm::dyn_cast<llvm::StructType>(type))
                numElements = structType->getNumElements();
            else
                assert(0 && "Constant aggregate type");

            for (int op = 0; op < numElements; ++op) {
                if (op > 0)
                    out << ", ";
                emitConstantInitializer(out,
                                        isZero ? 0 : llvm::dyn_cast<llvm::Constant>(constant->getOperand(op)),
                                        type->getContainedType(type->getNumContainedTypes() > 1 ? op : 0));
            }

            out << ")";
            break;
        }

        default:
            assert(0 && "Constant type in Bottom IR");
        }
    }

    void emitInitializeAggregate(std::ostringstream& out, std::string varString, const llvm::Constant* constant)
    {
        if (constant && IsDefined(constant) && ! IsScalar(constant) && ! AreAllDefined(constant)) {
            // For a vector or array with undefined elements, propagate the defined elements
            if (const llvm::ConstantVector* constVec = llvm::dyn_cast<llvm::ConstantVector>(constant)) {
                for (int op = 0; op < constVec->getNumOperands(); ++op) {
                    if (IsDefined(constVec->getOperand(op))) {
                        out << std::endl << "    " << varString;
                        out << "." << mapComponentToSwizzleChar(op) << " = ";
                        out << getGlaValue(constVec->getOperand(op));
                        out << ";";
                    }
                }
            } else if (const llvm::ConstantArray* constArray = llvm::dyn_cast<llvm::ConstantArray>(constant)) {
                for (int op = 0; op < constArray->getNumOperands(); ++op) {
                    if (IsDefined(constArray->getOperand(op))) {
                        out << std::endl << "    " << varString;
                        out << "[" << op << "] = ";
                        out << getGlaValue(constArray->getOperand(op));
                        out << ";";
                    }
                }
            } else if (const llvm::ConstantStruct* constStruct = llvm::dyn_cast<llvm::ConstantStruct>(constant)) {
                for (int op = 0; op < constStruct->getNumOperands(); ++op) {
                    if (IsDefined(constStruct->getOperand(op))) {
                        out << std::endl << "    " << varString;
                        out << "." << getGlaStructField(constant->getType(), op) << " = ";
                        out << getGlaValue(constStruct->getOperand(op));
                        out << ";";
                    }
                }
            } else {
                gla::UnsupportedFunctionality("Partially defined aggregate type");
            }
        } else {
            // This case is always handled by constant propagation
        }
    }

    bool addNewVariable(const llvm::Value* value, std::string& name)
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

    void emitGlaSwizzle(int glaSwizzle, int width, llvm::Value* source = 0)
    {
        if (source && gla::IsScalar(source))
            return;

        shader << ".";
        // Pull each two bit channel out of the integer
        for(int i = 0; i < width; i++)
            emitComponentToSwizzle(GetSwizzle(glaSwizzle, i));
    }

    // Emit the swizzle represented by the vector of channel selections
    void emitGlaSwizzle(const llvm::SmallVectorImpl<llvm::Constant*>& elts)
    {
        shader << ".";

        // Output the components for all defined channels
        for (int i = 0; i < elts.size(); ++i) {
            if (! IsDefined(elts[i]))
                continue;

            emitComponentToSwizzle(GetConstantInt(elts[i]));
        }
    }

    // Emit a writemask. Emits a component for each defined element of the
    // passed vector
    void emitGlaWriteMask(const llvm::SmallVectorImpl<llvm::Constant*>& elts)
    {
        shader << ".";

        // Output the components for all defined channels
        for (int i = 0; i < elts.size(); ++i) {
            if (! IsDefined(elts[i]))
                continue;

            emitComponentToSwizzle(i);
        }
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
            emitComponentToSwizzle(GetConstantInt(inst->getOperand(operand+1)));
        }
    }

    // Returns a pointer to the common source of the multiinsert if they're all
    // the same, otherwise returns null.
    llvm::Value* getCommonSourceMultiInsert(const llvm::IntrinsicInst* inst)
    {
        llvm::Value* source = NULL;
        bool sameSource = true;
        int wmask = GetConstantInt(inst->getOperand(1));

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
        int wmask = GetConstantInt(inst->getOperand(1));
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
                // Get the component select
                llvm::Constant* swizOffset = llvm::dyn_cast<llvm::Constant>(inst->getOperand(i*2 + 3));
                if (IsDefined(swizOffset)) {
                    singleSourceMask |= (GetConstantInt(swizOffset) << (pos*2));
                    ++pos;
                }
            }
            assert (singleSourceMask <= 0xFF);
            emitGlaSwizzle(singleSourceMask, argCount, source);
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
        int wmask = GetConstantInt(inst->getOperand(1));

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
        if (IsDefined(op)) {
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

    // Gets a string representation for the given swizzling ExtractElement
    // instruction, and sets str to it. Does nothing if passed something that
    // isn't an ExtractElement
    void getExtractElementStr(const llvm::Instruction* llvmInstruction, std::string& str)
    {
        if (! llvm::isa<llvm::ExtractElementInst>(llvmInstruction))
            return;

        str.assign(*valueMap[llvmInstruction->getOperand(0)]);
        str.append(".").append(mapComponentToSwizzleChar(GetConstantInt(llvmInstruction->getOperand(1))));
    }

    // Traverse the indices used in either GEP or Insert/ExtractValue, and return a string representing
    // it, not including the base.
    std::string traverseGEPChain(const llvm::Value* value)
    {
        std::string gepChain;

        if (const llvm::GetElementPtrInst* gepInst = llvm::dyn_cast<llvm::GetElementPtrInst>(value)) {

            // Start at index 2 since indices 0 and 1 give you the base and are handled before traverseGEP
            const llvm::Type* gepType = gepInst->getPointerOperandType()->getContainedType(0);
            for (int index = 2; index < gepInst->getNumOperands(); ++index) {
                int gepIndex = 0;
                if (llvm::isa<const llvm::ConstantInt>(gepInst->getOperand(index))) {
                    gepIndex = GetConstantInt(gepInst->getOperand(index));
                    gepType = getGEPDeref(gepType, gepIndex, &gepChain);
                } else  {
                    gepType = getGEPDeref(gepType, -1, &gepChain, gepInst->getOperand(index));
                }
            }

        } else if (const llvm::InsertValueInst* insertValueInst = llvm::dyn_cast<const llvm::InsertValueInst>(value)) {

            const llvm::Type* gepType = insertValueInst->getAggregateOperand()->getType();
            for (llvm::InsertValueInst::idx_iterator iter = insertValueInst->idx_begin(), end = insertValueInst->idx_end();  iter != end; ++iter)
                gepType = getGEPDeref(gepType, *iter, &gepChain);

        } else if (const llvm::ExtractValueInst* extractValueInst = llvm::dyn_cast<const llvm::ExtractValueInst>(value)) {

            const llvm::Type* gepType = extractValueInst->getAggregateOperand()->getType();
            for (llvm::ExtractValueInst::idx_iterator iter = extractValueInst->idx_begin(), end = extractValueInst->idx_end();  iter != end; ++iter)
                gepType = getGEPDeref(gepType, *iter, &gepChain);

        } else {
            assert(0 && "non-GEP in traverseGEP");
        }

        return gepChain;
    }

    // Traverse one step of a dereference chain and append to a string
    // For constant indices, pass it in index.  Otherwise, provide it through gepOp (index will not be used)
    const llvm::Type* getGEPDeref(const llvm::Type* type, unsigned index, std::string* chain, const llvm::Value* gepOp = 0)
    {
        switch (type->getTypeID()) {
        case llvm::Type::ArrayTyID:
            {
                chain->append("[");
                if (gepOp) {
                    chain->append(getGlaValue(gepOp));
                } else {
                    const size_t bufSize = 10;
                    char buf[bufSize];
                    snprintf(buf, bufSize, "%d", index);
                    chain->append(buf);
                }
                chain->append("]");

                return type->getContainedType(0);
            }
        case llvm::Type::StructTyID:
            assert(! gepOp);
            chain->append(".");
            chain->append(getGlaStructField(type, index));

            return type->getContainedType(index);
        default:
            assert(0 && "Dereferencing non array/struct");
        }

        return 0;
    }

    // mapping from LLVM values to Glsl variables
    std::map<const llvm::Value*, std::string*> valueMap;

    // map to track structure names tracked in the module
    std::map<const llvm::Type*, std::string> structNameMap;

    // set to track which arrays have been declared from stripping
    // indexes that were in scalar variable names
    std::set<std::string> globallyDeclaredArrays;

    std::ostringstream globalDeclarations;
    std::ostringstream globalInitializers;
    bool appendInitializers;
    std::ostringstream shader;
    int indentLevel;
    int lastVariable;
    bool obfuscate;
    int version;
};

//
// Factory for GLSL back-end translator
//
gla::BackEndTranslator* gla::GetGlslTranslator(Manager* manager)
{
    return new gla::GlslTarget(manager);
}

void gla::ReleaseGlslTranslator(gla::BackEndTranslator* target)
{
    delete target;
}

// Write the string representation of an operator. If it's an xor of some
// register and true, then unaryOperand will be set to the index for the
// non-true operand, and s will contain "!".
void gla::GlslTarget::getOp(const llvm::Instruction* llvmInstruction, std::string& s, int& unaryOperand)
{
    //
    // Look for binary ops, where the form would be "operand op operand"
    //

    switch (llvmInstruction->getOpcode()) {
    case llvm::Instruction:: Add:
    case llvm::Instruction::FAdd:
        s = "+";
        break;
    case llvm::Instruction:: Sub:
    case llvm::Instruction::FSub:
        s = "-";
        break;
    case llvm::Instruction:: Mul:
    case llvm::Instruction::FMul:
        s = "*";
        break;
    case llvm::Instruction::UDiv:
    case llvm::Instruction::SDiv:
    case llvm::Instruction::FDiv:
        s = "/";
        break;
    case llvm::Instruction::URem:
    case llvm::Instruction::SRem:
        s = "%";
        break;
    case llvm::Instruction::Shl:
        s = "<<";
        break;
    case llvm::Instruction::LShr:
        s = ">>";
        break;
    case llvm::Instruction::AShr:
        s = ">>";
        break;
    case llvm::Instruction::And:
        if (gla::IsBoolean(llvmInstruction->getOperand(0)->getType())) {
            s = "&&";
        } else {
            s = "&";
        }
        break;
    case llvm::Instruction::Or:
        if (gla::IsBoolean(llvmInstruction->getOperand(0)->getType())) {
            s = "||";
        } else {
            s = "|";
        }
        break;
    case llvm::Instruction::Xor:
        s = mapGlaXor(llvmInstruction, unaryOperand);
        break;
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
                case llvm::FCmpInst::FCMP_UEQ: // Possibly revise later
                case llvm::ICmpInst::ICMP_EQ:   s = "==";  break;

                case llvm::FCmpInst::FCMP_ONE:
                case llvm::FCmpInst::FCMP_UNE: // Possibly revise later
                case llvm::ICmpInst::ICMP_NE:   s = "!=";  break;

                case llvm::FCmpInst::FCMP_OGT:
                case llvm::FCmpInst::FCMP_UGT: // Possibly revise later
                case llvm::ICmpInst::ICMP_UGT:
                case llvm::ICmpInst::ICMP_SGT:  s = ">";   break;

                case llvm::FCmpInst::FCMP_OGE:
                case llvm::FCmpInst::FCMP_UGE: // Possibly revise laterw
                case llvm::ICmpInst::ICMP_UGE:
                case llvm::ICmpInst::ICMP_SGE:  s = ">=";  break;

                case llvm::FCmpInst::FCMP_OLT:
                case llvm::FCmpInst::FCMP_ULT: // Possibly revise later
                case llvm::ICmpInst::ICMP_ULT:
                case llvm::ICmpInst::ICMP_SLT:  s = "<";   break;

                case llvm::FCmpInst::FCMP_OLE:
                case llvm::FCmpInst::FCMP_ULE: // Possibly revise later
                case llvm::ICmpInst::ICMP_ULE:
                case llvm::ICmpInst::ICMP_SLE:  s = "<=";  break;
                default:
                    s = "==";
                    UnsupportedFunctionality("Comparison Operator in Bottom IR: ", cmp->getPredicate(), EATContinue);
                }
            } else {
                assert(! "Cmp instruction found that cannot dyncast to CmpInst");
            }
        }
        break;

    case llvm::Instruction::FPToUI:
        switch (gla::GetComponentCount(llvmInstruction)) {
        case 1: s = "uint";  break;
        case 2: s = "uvec2"; break;
        case 3: s = "uvec3"; break;
        case 4: s = "uvec4"; break;
        default: UnsupportedFunctionality("Can only convert scalars and vectors");
        }
        unaryOperand = 0;
        break;
    case llvm::Instruction::ZExt:
    case llvm::Instruction::FPToSI:
        switch (gla::GetComponentCount(llvmInstruction)) {
        case 1: s = "int";   break;
        case 2: s = "ivec2"; break;
        case 3: s = "ivec3"; break;
        case 4: s = "ivec4"; break;
        default: UnsupportedFunctionality("Can only convert scalars and vectors");
        }
        unaryOperand = 0;
        break;
    case llvm::Instruction::UIToFP:
    case llvm::Instruction::SIToFP:
        switch (gla::GetComponentCount(llvmInstruction)) {
        case 1: s = "float"; break;
        case 2: s = "vec2";  break;
        case 3: s = "vec3";  break;
        case 4: s = "vec4";  break;
        default: UnsupportedFunctionality("Can only convert scalars and vectors");
        }
        unaryOperand = 0;
        break;

    default:
        break;
        // fall through to check other ops
    }
}

//
// Add an LLVM instruction to the end of the GLSL instructions.
//
void gla::GlslTarget::add(const llvm::Instruction* llvmInstruction, bool lastBlock, bool referencedOutsideScope)
{
    std::string charOp;
    int unaryOperand = -1;

    // TODO:  This loop will disappear when conditional loops in BottomToGLSL properly updates valueMap
    for (llvm::Instruction::const_op_iterator i = llvmInstruction->op_begin(), e = llvmInstruction->op_end(); i != e; ++i) {
        llvm::Instruction* inst = llvm::dyn_cast<llvm::Instruction>(*i);
        if (inst) {
            if (valueMap[*i] == 0)
                add(inst, lastBlock);
            }
    }

    // If the instruction is referenced outside of the current scope
    // (e.g. inside a loop body), then add a (global) declaration for it.
    if (referencedOutsideScope){
        mapGlaValue(llvmInstruction, referencedOutsideScope);
    }

    getOp(llvmInstruction, charOp, unaryOperand);

    // Handle the binary ops
    if (! charOp.empty() && unaryOperand == -1) {
        newLine();
        emitGlaValue(llvmInstruction);
        shader << " = ";
        emitGlaOperand(llvmInstruction->getOperand(0));
        shader << " " << charOp << " ";
        emitGlaOperand(llvmInstruction->getOperand(1));
        shader << ";";
        return;
    }

    // Handle the unary ops
    if (! charOp.empty()) {
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
                case llvm::FCmpInst::FCMP_UEQ: // Possibly revise later
                case llvm::ICmpInst::ICMP_EQ:   charOp = "equal";             break;

                case llvm::FCmpInst::FCMP_ONE:
                case llvm::FCmpInst::FCMP_UNE: // Possibly revise later
                case llvm::ICmpInst::ICMP_NE:   charOp = "notEqual";          break;

                case llvm::FCmpInst::FCMP_OGT:
                case llvm::FCmpInst::FCMP_UGT: // Possibly revise later
                case llvm::ICmpInst::ICMP_UGT:
                case llvm::ICmpInst::ICMP_SGT:  charOp = "greaterThan";       break;

                case llvm::FCmpInst::FCMP_OGE:
                case llvm::FCmpInst::FCMP_UGE: // Possibly revise later
                case llvm::ICmpInst::ICMP_UGE:
                case llvm::ICmpInst::ICMP_SGE:  charOp = "greaterThanEqual";  break;

                case llvm::FCmpInst::FCMP_OLT:
                case llvm::FCmpInst::FCMP_ULT: // Possibly revise later
                case llvm::ICmpInst::ICMP_ULT:
                case llvm::ICmpInst::ICMP_SLT:  charOp = "lessThan";          break;

                case llvm::FCmpInst::FCMP_OLE:
                case llvm::FCmpInst::FCMP_ULE: // Possibly revise later
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
        {
            assert(! llvm::isa<llvm::ConstantExpr>(llvmInstruction->getOperand(0)));

            if (llvm::isa<llvm::GetElementPtrInst>(llvmInstruction->getOperand(0))) {
                // If we're loading from the result of a GEP, assign it to a new variable
                newLine();
                emitGlaValue(llvmInstruction);
                shader << " = ";
                emitGlaValue(llvmInstruction->getOperand(0));
                shader << ";";
            } else if (llvm::isa<llvm::PHINode>(llvmInstruction->getOperand(0))) {
                // We want phis to use the same variable name created during phi declaration
                addNewVariable(llvmInstruction, *valueMap[llvmInstruction->getOperand(0)]);
            } else {
                std::string name = llvmInstruction->getOperand(0)->getNameStr();
                makeParseable(name);
                addNewVariable(llvmInstruction, name);
            }
        }
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
            std::string swizzled;
            getExtractElementStr(llvmInstruction, swizzled);

            // If we're globally referenced, then we should have a name for
            // ourselves inside valueMap. In that case, update it to be our
            // propagated swizzle name
            if (referencedOutsideScope)
                valueMap[llvmInstruction] = new std::string(swizzled);
            else
                addNewVariable(llvmInstruction, swizzled);
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
        emitComponentToSwizzle(GetConstantInt(llvmInstruction->getOperand(2)));
        shader << " = ";
        emitGlaOperand(llvmInstruction->getOperand(1));
        shader << ";";
        return;

    case llvm::Instruction::Select:
    {
        const llvm::SelectInst* si = llvm::dyn_cast<const llvm::SelectInst>(llvmInstruction);
        assert(si);
        newLine();
        emitGlaValue(llvmInstruction);
        shader << " = ";
        emitGlaValue(si->getCondition());
        shader << " ? ";
        emitGlaValue(si->getTrueValue());
        shader << " : ";
        emitGlaValue(si->getFalseValue());
        shader << ";";
        return;
    }
    case llvm::Instruction::GetElementPtr:
    {
        // For GEP, which always returns a pointer, traverse the dereference chain and store it.
        std::string* newVariable = new std::string(*valueMap[llvmInstruction->getOperand(0)]);
        newVariable->append(traverseGEPChain(llvmInstruction));
        addNewVariable(llvmInstruction, *newVariable);

        return;
    }
    case llvm::Instruction::ExtractValue:
    {
        newLine();
        emitGlaValue(llvmInstruction);
        shader << " = ";

        // emit base
        const llvm::ExtractValueInst* extractValueInst = llvm::dyn_cast<const llvm::ExtractValueInst>(llvmInstruction);
        emitGlaValue(extractValueInst->getAggregateOperand());

        // emit chain
        shader << traverseGEPChain(extractValueInst);
        shader << ";";

        return;
    }
    case llvm::Instruction::InsertValue:
    {
        newLine();

        //emit base
        const llvm::InsertValueInst* insertValueInst = llvm::dyn_cast<const llvm::InsertValueInst>(llvmInstruction);
        emitGlaValue(insertValueInst->getAggregateOperand());

        // emit chain
        shader << traverseGEPChain(insertValueInst);

        shader << " = ";
        emitGlaValue(insertValueInst->getInsertedValueOperand());
        shader << ";";

        // propagate aggregate name
        llvm::Value* dest = llvmInstruction->getOperand(0);
        //valueMap[llvmInstruction] = valueMap[dest];
        addNewVariable(llvmInstruction, *valueMap[dest]);

        return;
    }
    case llvm::Instruction::ShuffleVector: {
        newLine();
        emitGlaValue(llvmInstruction);

        shader << " = ";

        emitGlaType(shader, llvmInstruction->getType());
        shader << "(";

        int sourceWidth = gla::GetComponentCount(llvmInstruction->getOperand(0));
        int resultWidth = gla::GetComponentCount(llvmInstruction);

        const llvm::Constant* mask = llvm::dyn_cast<const llvm::Constant>(llvmInstruction->getOperand(2));
        assert(llvm::isa<llvm::ConstantVector>(mask) || llvm::isa<llvm::ConstantAggregateZero>(mask));

        llvm::SmallVector<llvm::Constant*,4> elts;
        mask->getVectorElements(elts);

        for (int i = 0; i < resultWidth; ++i) {
            if (i != 0)
                shader << ", ";

            // If we're undef, then use ourselves
            if (! IsDefined(elts[i])) {
                emitGlaValue(llvmInstruction);
                shader << ".";
                emitComponentToSwizzle(i);
                continue;
            }

            int comp = GetConstantInt(elts[i]);
            bool useSecond = comp > sourceWidth-1;

            if (useSecond)
                comp -= sourceWidth;

            emitGlaValue(llvmInstruction->getOperand(useSecond ? 1 : 0));
            shader << ".";
            emitComponentToSwizzle(comp);
        }

        shader << ");";

        return;
    }

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
const char* gla::GlslTarget::mapGlaXor(const llvm::Instruction* llvmInstruction, int& unaryOperand)
{
    bool scalar = IsScalar(llvmInstruction->getType());
    bool boolean = IsBoolean(llvmInstruction->getType());

    bool op0AllSet = HasAllSet(llvmInstruction->getOperand(0));
    bool op1AllSet = HasAllSet(llvmInstruction->getOperand(1));

    // first, see if it could be unary

    if (op0AllSet || op1AllSet) {
        // unary; set which operand is the real one to operate on
        if (op0AllSet)
            unaryOperand = 1;
        else
            unaryOperand = 0;

        if (scalar && boolean)
            return "!";

        if (!scalar && boolean)
            return "not";

        if (!boolean)
            return "~";

        UnsupportedFunctionality("xor", EATContinue);

        return "!";
    }

    // now go for binary

    if (scalar && boolean)
        return "^^";

    if (!boolean)
        return "^";

    UnsupportedFunctionality("xor", EATContinue);

    return "^";
}

//
// Handle the subcase of an LLVM instruction being an intrinsic call.
//
void gla::GlslTarget::mapGlaIntrinsic(const llvm::IntrinsicInst* llvmInstruction)
{
    // Handle pipeline read/write
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_fWriteData:
        {
            newLine();
            int location = GetConstantInt(llvmInstruction->getOperand(0));
            std::string name = manager->getPipeOutSymbols()[location].name;
            gla::RemoveArraySizeFromName(name);
            shader << name << " = ";
            emitGlaOperand(llvmInstruction->getOperand(2));
            shader << ";";

            return;
        }
    case llvm::Intrinsic::gla_readData:
    case llvm::Intrinsic::gla_fReadData:
    case llvm::Intrinsic::gla_fReadInterpolant:
        {
            std::string name = llvmInstruction->getNameStr();
            makeParseable(name);

            // Remove inserted size in front of hard-coded array indexes
            // TODO: there must be a better way to be doing this (sideband, unique syntax, etc.)
            std::string declareName = name;
            gla::RemoveArraySizeFromName(name);

            if (addNewVariable(llvmInstruction, name)) {
                declareVariable(llvmInstruction->getType(), declareName, EVQInput);
            }

            return;
        }
    }

    // Handle texturing
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_fTextureSample:
    case llvm::Intrinsic::gla_fTextureSampleLodRefZ:
    case llvm::Intrinsic::gla_fTextureSampleLodRefZOffset:
    case llvm::Intrinsic::gla_fTextureSampleLodRefZOffsetGrad:
    case llvm::Intrinsic::gla_texelFetchOffset:
    case llvm::Intrinsic::gla_fTexelFetchOffset:


        newLine();
        emitGlaValue(llvmInstruction);
        shader << " = ";
        emitGlaSamplerType(llvmInstruction, GetConstantInt(llvmInstruction->getOperand(GetTextureOpIndex(ETOFlag))));
        emitGlaTextureStyle(llvmInstruction);
        shader << "(";
        emitGlaOperand(llvmInstruction->getOperand(GetTextureOpIndex(ETOSamplerLoc)));
        shader << ", ";

        if(needsShadowRefZArg(llvmInstruction)) {

            // Construct a new vector of size coords+1 to hold coords and shadow ref
            int coordWidth = gla::GetComponentCount(llvmInstruction->getOperand(GetTextureOpIndex(ETOCoord)));
            assert(coordWidth < 4);

            const llvm::Type* coordType = llvmInstruction->getOperand(GetTextureOpIndex(ETOCoord))->getType();

            if (coordType->isVectorTy())
                coordType = coordType->getContainedType(0);

            // RefZ must reside in 3rd component or higher, so detect single component case
            int buffer = (coordWidth == 1) ? 1 : 0;

            const llvm::Type* vecType = llvm::VectorType::get(coordType, coordWidth + buffer + 1);

            emitGlaType(shader, vecType);

            shader << "(";

            // Texcoords first
            emitGlaOperand(llvmInstruction->getOperand(GetTextureOpIndex(ETOCoord)));

            shader << ", ";

            // Insert unused channel for 1D coordinate
            if (buffer > 0)
                shader << "0, ";

            // Followed by scalar shadow ref
            assert(gla::IsScalar(llvmInstruction->getOperand(GetTextureOpIndex(ETORefZ))));
            emitGlaOperand(llvmInstruction->getOperand(GetTextureOpIndex(ETORefZ)));

            shader << ")";
        } else {
            emitGlaOperand(llvmInstruction->getOperand(GetTextureOpIndex(ETOCoord)));
        }

        if(needsBiasLodArg(llvmInstruction)) {
            shader << ", ";
            emitGlaOperand(llvmInstruction->getOperand(GetTextureOpIndex(ETOBiasLod)));
        }

        if(IsGradientTexInst(llvmInstruction)) {  //?? this can move to a place they are shared between back-ends
            shader << ", ";
            emitGlaOperand(llvmInstruction->getOperand(GetTextureOpIndex(ETODPdx)));
            shader << ", ";
            emitGlaOperand(llvmInstruction->getOperand(GetTextureOpIndex(ETODPdy)));
        }

        if (needsOffsetArg(llvmInstruction)) {
            shader << ", ";
            emitGlaOperand(llvmInstruction->getOperand(GetTextureOpIndex(ETOOffset)));
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

        llvm::Constant* mask = llvm::dyn_cast<llvm::Constant>(llvmInstruction->getOperand(1));
        assert(mask);

        llvm::SmallVector<llvm::Constant*, 8> elts;
        mask->getVectorElements(elts);

        if (! AreAllDefined(mask)) {
            shader << ";";
            newLine();

            // Set our writemask to correspond to defined components
            emitGlaValue(llvmInstruction);
            emitGlaWriteMask(elts);
        }

        shader << " = ";

        llvm::Value* src = llvmInstruction->getOperand(0);
        int srcVectorWidth = GetComponentCount(src);

        int dstVectorWidth = GetComponentCount(mask);
        assert(dstVectorWidth == GetComponentCount(llvmInstruction));

        // Case 0:  it's scalar making a scalar.
        // use nothing, just copy
        if (srcVectorWidth == 1 && dstVectorWidth == 1) {
            emitGlaOperand(src);
            shader << ";";

            return;
        }

        // Case 1:  it's a scalar with multiple ".x" to expand it to a vector.
        // use a constructor to turn a scalar into a vector
        if (srcVectorWidth == 1 && dstVectorWidth > 1) {
            emitGlaType(shader, llvmInstruction->getType());
            shader << "(";
            emitGlaOperand(src);
            shader << ");";

            return;
        }

        // Case 2:  it's sequential .xy...  subsetting a vector.
        // use a constructor to subset the vector to a vector
        if (srcVectorWidth > 1 && dstVectorWidth > 1 && IsIdentitySwizzle(elts)) {
            emitGlaType(shader, llvmInstruction->getType());
            shader << "(";
            emitGlaOperand(src);
            shader << ");";

            return;
        }

        // Case 3:  it's a non-sequential subsetting of a vector.
        // use GLSL swizzles
        assert(srcVectorWidth > 1);
        emitGlaOperand(src);
        emitGlaSwizzle(elts);
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

    // Handle fixedTransform
    if (llvmInstruction->getIntrinsicID() == llvm::Intrinsic::gla_fFixedTransform) {
        newLine();
        emitGlaValue(llvmInstruction);
        shader << " = " << "ftransform();";

        return;
    }

    // Handle the one-to-one mappings
    const char* callString = 0;
    unsigned int callArgs = 0;
    int forceWidth = 0;

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
    case llvm::Intrinsic::gla_fAtan2:       callString = "atan";        callArgs = 2; break;
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
    case llvm::Intrinsic::gla_fRoundZero:   callString = "trunc";       callArgs = 1; break;
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
    case llvm::Intrinsic::gla_fDot2:
    case llvm::Intrinsic::gla_fDot3:
    case llvm::Intrinsic::gla_fDot4:        callString = "dot";         callArgs = 2; break;
    case llvm::Intrinsic::gla_fCross:       callString = "cross";       callArgs = 2; break;
    case llvm::Intrinsic::gla_fNormalize:   callString = "normalize";   callArgs = 1; break;
    case llvm::Intrinsic::gla_fNormalize3D:                                           break;
    case llvm::Intrinsic::gla_fLit:                                                   break;
    case llvm::Intrinsic::gla_fFaceForward: callString = "faceforward"; callArgs = 3; break;
    case llvm::Intrinsic::gla_fReflect:     callString = "reflect";     callArgs = 2; break;
    case llvm::Intrinsic::gla_fRefract:     callString = "refract";     callArgs = 3; break;

    // Derivative and Transform
    case llvm::Intrinsic::gla_fDFdx:           callString = "dFdx";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fDFdy:           callString = "dFdy";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fFilterWidth:    callString = "fwidth";     callArgs = 1; break;

    // Vector Logical
    case llvm::Intrinsic::gla_not: callString = "not"; callArgs = 1; break;
    case llvm::Intrinsic::gla_any: callString = "any"; callArgs = 1; break;
    case llvm::Intrinsic::gla_all: callString = "all"; callArgs = 1; break;
    }

    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_fDot2:  forceWidth = 2;  break;
    case llvm::Intrinsic::gla_fDot3:  forceWidth = 3;  break;
    case llvm::Intrinsic::gla_fDot4:  forceWidth = 4;  break;
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
        if (forceWidth && forceWidth < GetComponentCount(llvmInstruction->getOperand(arg)))
            emitComponentCountToSwizzle(forceWidth);
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
