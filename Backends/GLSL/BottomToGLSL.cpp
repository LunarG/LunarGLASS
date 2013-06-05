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
#include "TopBuilder.h"
#include "metadata.h"

// glslang includes
#include "../../glslang/glslang/Public/ShaderLang.h"
#include "../../glslang/glslang/MachineIndependent/Versions.h"

// LLVM includes
#include "llvm/Module.h"

#include "Passes/Util/ConstantUtil.h"

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
    }

    ~GlslTarget()
    {
    }

    virtual void start()
    {
        // this information wasn't available at construct time
        version = manager->getVersion();
        profile = static_cast<EProfile>(version >> 16 & 0xFF);
        language = static_cast<EShLanguage>(version >> 24 & 0xFF);
        version &= 0xFFFF;
    }

    void addStructType(llvm::StringRef name, const llvm::Type* structType, const llvm::MDNode* mdAggregate, bool block)
    {
        // this is mutually recursive with emitGlaType

        if (structNameMap.find(structType) != structNameMap.end())
            return;

        // For nested struct types, we have to output the nested one
        // before the containing one.  So, make the current on the side
        // and add it to the global results after its contents are
        // declared.
        std::ostringstream tempStructure;

        structNameMap[structType] = name;
        if (! block)
            tempStructure << "struct ";
        if (mdAggregate)
            tempStructure << std::string(mdAggregate->getOperand(0)->getName());
        else
            tempStructure << name.str();
        tempStructure << " {" << std::endl;

        for (int index = 0; index < structType->getNumContainedTypes(); ++index) {
            tempStructure << "    ";
            if (mdAggregate) {
                const llvm::MDNode* subMdAggregate = llvm::dyn_cast<llvm::MDNode>(mdAggregate->getOperand(GetAggregateMdSubAggregateOp(index)));
                emitGlaType(tempStructure, EMpNone, structType->getContainedType(index), false, subMdAggregate);
                tempStructure << " " << std::string(mdAggregate->getOperand(GetAggregateMdNameOp(index))->getName());
            } else {
                emitGlaType(tempStructure, EMpNone, structType->getContainedType(index), false);
                tempStructure << " " << getGlaStructField(structType, index);
            }
            tempStructure << ";" << std::endl;
        }

        tempStructure << "}";
        if (! block)
            tempStructure << ";" << std::endl << std::endl;

        if (block)
            globalDeclarations << tempStructure.str();
        else
            globalStructures << tempStructure.str();
    }

    void addGlobal(const llvm::GlobalVariable* global)
    {
        // skip uniforms and other objects, just get the regular global variables
        if (mapGlaAddressSpace(global) != EVQGlobal)
            return;

        llvm::Type* type;
        if (const llvm::PointerType* pointer = llvm::dyn_cast<llvm::PointerType>(global->getType()))
            type = pointer->getContainedType(0);
        else
            type = global->getType();

        std::string name = global->getName();
        std::string declareName = name;

        makeParseable(name);
        addNewVariable(global, name);
        declareVariable(EMpNone, type, declareName, mapGlaAddressSpace(global));

        if (global->hasInitializer()) {
            const llvm::Constant* constant = global->getInitializer();
            emitInitializeAggregate(globalInitializers, global->getName(), constant);
        }
    }

    void addUniform(const llvm::MDNode* mdNode)
    {
        globalDeclarations << "uniform ";
        emitGlaType(globalDeclarations, EMpCount, 0, true, mdNode);
        globalDeclarations << " " << std::string(mdNode->getOperand(0)->getName()) << ";" << std::endl;
        // TODO: functionality: arrayed blocks: add [] if an arrayed instance name
    }

    void startFunctionDeclaration(const llvm::Type* type, llvm::StringRef name)
    {
        newLine();
        emitGlaType(shader, EMpNone, type->getContainedType(0));
        // TODO: Goo: ES functionality: how do we know the precision or unsignedness of a function declaration?
        shader << " " << name.str() << "(";

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
        bool nested;
        getOp(cmp, opStr, pos, nested);

        bool binOp = false;
        if (pos == -1)
            binOp = true;

        // TODO: Goo: add support for unary ops (and xor)

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

    void getOp(const llvm::Instruction* llvmInstruction, std::string& s, int& unaryOperand, bool& nested);

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
            if (version >= 130)
                                 string = "in";
            else if (language == EShLangVertex)
                                 string = "attribute";
            else
                                 string = "varying";                  break;
        case EVQOutput:
                version >= 130 ? string = "out": 
                                 string = "varying";                  break;
        case EVQTemporary:       string = "temp";                     break;
        case EVQConstant:        string = "const";                    break;
        case EVQUndef:           string = "undef";                    break;
        default:
            assert(! "unknown VariableQualifier");
        }

        return string;
    }

    const char* mapGlaToPrecisionString(EMdPrecision precision)
    {
        switch (precision) {
        case EMpLow:      return "lowp";
        case EMpMedium:   return "mediump";
        case EMpHigh:     return "highp";
        default:          return "";
        }
    }

    void emitGlaPrecision(std::ostringstream& out, EMdPrecision precision)
    {
        switch (precision) {
        case EMpLow:
        case EMpMedium:
        case EMpHigh:
            out << mapGlaToPrecisionString(precision) << " ";
            break;

        case EMpNone:
            break;

        default:
            out << "badp ";
            break;
        }   
    }

    EMdPrecision getPrecision(const llvm::Value* value)
    {
        EMdPrecision precision = EMpNone;

        if (const llvm::Instruction* instr = llvm::dyn_cast<const llvm::Instruction>(value))
            CrackPrecisionMd(instr, precision);

        return precision;
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

    void emitGlaSampler(const llvm::IntrinsicInst* llvmInstruction, int texFlags)
    {
        const llvm::Value* samplerType = llvmInstruction->getOperand(0);

        // TODO: uint functionality: See if it's a uint sampler, requiring a constructor to convert it

        // Original style shadowing returns vec4 while 2nd generation returns float,
        // so, have to stick to old-style for those cases.
        bool forceOldStyle = IsVector(llvmInstruction->getType()) && (texFlags & ETFShadow);

        if (version >= 130 && ! forceOldStyle) {
            if (texFlags & ETFFetch)
                shader << "texelFetch";
            else
                shader << "texture";
        } else {
            if (texFlags & ETFShadow)
                shader << "shadow";
            else
                shader << "texture";

            int sampler = GetConstantInt(samplerType);

            switch(sampler) {
            case ESampler1D:        shader << "1D";   break;
            case ESampler2D:        shader << "2D";   break;
            case ESampler3D:        shader << "3D";   break;
            case ESamplerCube:      shader << "Cube"; break;
            case ESampler2DRect:    shader << "Rect"; break;
            default:
                UnsupportedFunctionality("Texturing in Bottom IR: ", sampler, EATContinue);
                break;
            }
        }
        
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

    bool needsLodArg(const llvm::IntrinsicInst* llvmInstruction)
    {
        // Check flags for bias/lod
        int texFlags = GetConstantInt(llvmInstruction->getOperand(GetTextureOpIndex(ETOFlag)));

        return (texFlags & ETFLod);
    }

    bool needsBiasArg(const llvm::IntrinsicInst* llvmInstruction)
    {
        // Check flags for bias/lod
        int texFlags = GetConstantInt(llvmInstruction->getOperand(GetTextureOpIndex(ETOFlag)));

        return (texFlags & ETFBiasLodArg) && ! (texFlags & ETFLod);
    }

    bool needsOffsetArg(const llvm::IntrinsicInst* llvmInstruction)
    {
        // Check flags for offset arg
        int texFlags = GetConstantInt(llvmInstruction->getOperand(GetTextureOpIndex(ETOFlag)));

        return (texFlags & ETFOffsetArg);
    }

    void getNewVariableName(const llvm::Value* value, std::string* name)
    {
        ++lastVariable;
        const size_t bufSize = 20;
        char buf[bufSize];
        if (obfuscate) {
            int i;
            for (i = 0; i <= lastVariable-4; i += 4) {
                switch ((i/4) % 4) {
                case 0:   name->append("x"); break;
                case 1:   name->append("y"); break;
                case 2:   name->append("z"); break;
                case 3:   name->append("w"); break;
                }
            }
            switch (lastVariable - i) {
            case 0:   name->append("x"); break;
            case 1:   name->append("y"); break;
            case 2:   name->append("z"); break;
            case 3:   name->append("w"); break;
            }
        } else {
            if (IsTempName(value->getName())) {
                name->append(mapGlaToQualifierString(mapGlaAddressSpace(value)));
                snprintf(buf, bufSize, "%d", lastVariable);
                name->append(buf);

                // If it's a constant int or float, make the name contain the
                // value
                if (llvm::isa<llvm::ConstantInt>(value)) {
                    int val = GetConstantInt(value);

                    // If it's an i1, that is a bool, then have it say true or
                    // false, else have it have the integer value.
                    if (IsBoolean(value->getType())) {
                        name->append("b_");
                        snprintf(buf, bufSize, val ? "true" : "false");
                    } else {
                        name->append("i_");
                        snprintf(buf, bufSize, "%d", GetConstantInt(value));
                    }

                    name->append(buf);

                } else if (llvm::isa<llvm::ConstantFP>(value)) {
                    name->append("f_");
                    snprintf(buf, bufSize, "%.0f", GetConstantFloat(value));
                    name->append(buf);
                }
            } else {
                name->append(value->getName());
            }

            makeParseable(*name);

            // Variables starting with gl_ are illegal in GLSL
            if (name->substr(0,3) == std::string("gl_")) {
                name->insert(0, "gla_");
            }
        }
    }

    void makeParseable(std::string& name)
    {
        // Some symbols were annotated with a prefix and a space
        int spaceLoc = name.find_first_of(' ');
        if (spaceLoc != std::string::npos)
            name.erase(0, spaceLoc+1);

        // LLVM uses "." for phi'd symbols, change to _ so it's parseable by GLSL
        for (int c = 0; c < name.length(); ++c) {
            if (name[c] == '.' || name[c] == '-')
                name[c] = 'd';
        }
    }

    void declareVariable(EMdPrecision precision, llvm::Type* type, const std::string& name, EVariableQualifier vq, 
                         const llvm::Constant* constant = 0, const llvm::MDNode* mdIoNode = 0)
    {
        if (name.substr(0,3) == std::string("gl_"))
            return;

        // If it has an initializer (is a constant and not an undef)
        if (constant && ! AreAllUndefined(constant)) {
            globalDeclarations << mapGlaToQualifierString(vq);
            globalDeclarations << " ";
            emitGlaType(globalDeclarations, precision, type);
            globalDeclarations << " " << name << " = ";
            emitConstantInitializer(globalDeclarations, constant, constant->getType());
            globalDeclarations << ";" << std::endl;

            return;
        }

        // no initializer
        switch (vq) {
        case EVQConstant:
            // Make sure we only declare globals once
            if (globallyDeclared.find(name) != globallyDeclared.end())
                return;
            else
                globallyDeclared.insert(name);

            globalDeclarations << mapGlaToQualifierString(vq);
            globalDeclarations << " ";
            emitGlaType(globalDeclarations, precision, type);
            globalDeclarations << " " << name << ";" << std::endl;
            break;
        case EVQGlobal:
            emitGlaType(globalDeclarations, precision, type);
            globalDeclarations << " " << name << ";" << std::endl;
            break;
        case EVQTemporary:
            emitGlaType(shader, precision, type);
            shader << " ";
            break;
        case EVQUndef:
            newLine();
            emitGlaType(shader, precision, type);
            shader << " " << name << ";";
            emitInitializeAggregate(shader, name, constant);
            break;
        default:
            assert(! "unknown VariableQualifier");
        }
    }

    void declareIOVariable(llvm::Type* type, const std::string& name, EVariableQualifier vq,
                           EInterpolationMethod interpMethod, EInterpolationLocation interpLocation, const llvm::MDNode* mdNode)
    {
        if (name.substr(0,3) == std::string("gl_"))
            return;

        if (globallyDeclared.find(name) != globallyDeclared.end())
            return;
        else
            globallyDeclared.insert(name);

        if (interpLocation != EILLast) {
            if (interpMethod != EIMNone) {
                switch (interpLocation) {
                case EILSample:        globalDeclarations << "sample ";        break;
                case EILCentroid:      globalDeclarations << "centroid ";      break;
                }
            }

            if (version >= 130) {
                if (language != EShLangVertex) {
                    switch (interpMethod) {
                    case EIMNone:          globalDeclarations << "flat ";          break;
                    //case EIMSmooth:        globalDeclarations << "smooth ";        break;
                    case EIMNoperspective: globalDeclarations << "noperspective "; break;
                    }
                }
            }
        }

        globalDeclarations << mapGlaToQualifierString(vq);
        globalDeclarations << " ";
        emitGlaType(globalDeclarations, gla::EMpNone, type, true, mdNode);
        globalDeclarations << " " << name;
        globalDeclarations << ";" << std::endl;
    }

    void emitGlaType(std::ostringstream& out, EMdPrecision precision, llvm::Type* type, bool ioRoot = true, const llvm::MDNode* mdNode = 0, int count = -1)
    {
        bool matrix = false;
        bool notSigned = false;
        std::string name;
        const llvm::MDNode* mdAggregate = 0;
        bool block = false;
        if (mdNode) {
            EMdInputOutput ioKind;
            EMdTypeLayout typeLayout;
            int location;
            const llvm::MDNode* mdSampler;
            if (ioRoot) {
                if (! CrackIOMd(mdNode, name, ioKind, type, typeLayout, precision, location, mdSampler, mdAggregate)) {
                    UnsupportedFunctionality("IO metadata for type");

                    return;
                }
                block = ioKind == EMioUniformBlockMember || ioKind == EMioBufferBlockMember;
            } else {
                if (! CrackAggregateMd(mdNode, name, typeLayout, precision, location, mdSampler)) {
                    UnsupportedFunctionality("aggregate metadata for type");

                    return;
                }
                mdAggregate = mdNode;
            }
            if (typeLayout == EMtlSampler) {
                emitGlaSamplerType(out, mdSampler);

                return;
            }
            matrix = typeLayout == EMtlRowMajorMatrix || typeLayout == EMtlColMajorMatrix;
            notSigned = typeLayout == EMtlUnsigned;
        }

        if (type->getTypeID() == llvm::Type::PointerTyID)
            type = llvm::dyn_cast<llvm::PointerType>(type)->getContainedType(0);

        // if it's a vector, output a vector type
        if (type->getTypeID() == llvm::Type::VectorTyID) {
            const llvm::VectorType *vectorType = llvm::dyn_cast<llvm::VectorType>(type);
            assert(vectorType);
            
            emitGlaPrecision(out, precision);
            if (type->getContainedType(0) == type->getFloatTy(type->getContext()))
                out << "vec";
            else if (type->getContainedType(0) == type->getInt1Ty(type->getContext()))
                out << "bvec";
            else if (type->getContainedType(0) == type->getInt32Ty(type->getContext())) {
                if (notSigned)
                    out << "uvec";
                else
                    out << "ivec";
            } else
                UnsupportedFunctionality("Basic Type in Bottom IR");

            // output the size of the vector
            if (count == -1)
                out << GetComponentCount(type);
            else
                out << count;
        } else if (type->getTypeID() == llvm::Type::StructTyID) {
            const llvm::StructType* structType = llvm::dyn_cast<const llvm::StructType>(type);
            
            // addStructType() is mutually recursive with this function
            addStructType(structType->getName(), structType, mdAggregate, block);
            if (! block) {
                if (mdAggregate)
                    out << std::string(mdAggregate->getOperand(0)->getName());
                else
                    out << structNameMap[structType];
            }
        } else if (type->getTypeID() == llvm::Type::ArrayTyID) {
            const llvm::ArrayType* arrayType = llvm::dyn_cast<const llvm::ArrayType>(type);
            
            if (matrix && arrayType->getNumContainedTypes() > 0 && arrayType->getContainedType(0)->isVectorTy()) {
                // We're at the matrix level in the type tree
                emitGlaPrecision(out, precision);
                out << "mat";
                if (GetNumColumns(type) == GetNumRows(type))
                    out << GetNumColumns(type);
                else
                    out << GetNumColumns(type) << "x" << GetNumRows(type);
            } else {
                // We're still higher up in the type tree than a matrix; e.g., array of matrices
                // (or, not a matrix).
                emitGlaType(out, precision, arrayType->getContainedType(0), false, mdAggregate);
                out << "[" << arrayType->getNumElements() << "]";
            }
        //} else if (type->getTypeID() == llvm::Type::PointerTyID) {
        //    const llvm::PointerType* pointerType = llvm::dyn_cast<const llvm::PointerType>(type);
        //    emitGlaType(out, pointerType->getContainedType(0));
        } else {
            // just output a scalar
            emitGlaPrecision(out, precision);            
            if (type == type->getFloatTy(type->getContext()))
                out << "float";
            else if (type == type->getInt1Ty(type->getContext()))
                out << "bool";
            else if (type == type->getInt32Ty(type->getContext())) {
                if (notSigned)
                    out << "uint";
                else
                    out << "int";
            } else if (type == type->getVoidTy(type->getContext()))
                out << "void";
            else
                UnsupportedFunctionality("Basic Type in Bottom IR");
        }
    }

    void emitGlaSamplerType(std::ostringstream& out, const llvm::MDNode* mdSamplerNode)
    {
        EMdSampler mdSampler;
        llvm::Type* type;
        EMdSamplerDim mdSamplerDim;
        bool isArray;
        bool isShadow;
        EMdSamplerBaseType baseType;
        if (gla::CrackSamplerMd(mdSamplerNode, mdSampler, type, mdSamplerDim, isArray, isShadow, baseType)) {
            switch (baseType) {
            case EMsbFloat:                  break;
            case EMsbInt:        out << "i"; break;
            case EMsbUint:       out << "u"; break;
            default:             UnsupportedFunctionality("base type of sampler");  break;
            }
            switch (mdSampler) {
            case EMsTexture:     out << "sampler";   break;
            case EMsImage:       out << "image";     break;
            default:             UnsupportedFunctionality("kind of sampler");  break;
            }
            switch (mdSamplerDim) {
            case EMsd1D:       out << "1D";      break;
            case EMsd2D:       out << "2D";      break;
            case EMsd3D:       out << "3D";      break;
            case EMsdCube:     out << "Cube";    break;
            case EMsdRect:     out << "Rect";    break;
            case EMsdBuffer:   out << "Buffer";  break;
            default:           UnsupportedFunctionality("kind of sampler");  break;
            }
            if (isShadow)
                out << "Shadow";
            if (isArray)
                out << "Array";
        } else
            UnsupportedFunctionality("sampler metadata", EATContinue);
    }

    void emitGlaConstructor(std::ostringstream& out, llvm::Type* type, int count = -1)
    {
        emitGlaType(out, EMpNone, type, false, 0, count);
    }

    // If valueMap has no entry for value, generate a name and declaration, and
    // store it in valueMap. If forceGlobal is true, then it will make the
    // declaration occur as a global.
    void mapGlaValue(const llvm::Value* value, bool forceGlobal = false)
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

            std::string* newName = new std::string;
            getNewVariableName(value, newName);
            EMdPrecision precision = getPrecision(value);

            if (const llvm::PointerType* pointerType = llvm::dyn_cast<llvm::PointerType>(value->getType())) {
                declareVariable(precision, pointerType->getContainedType(0), *newName, evq);
            } else {
                declareVariable(precision, value->getType(), *newName, evq, llvm::dyn_cast<llvm::Constant>(value));
            }
            valueMap[value] = newName;
        }
    }

    void emitGlaValue(const llvm::Value* value)
    {
        assert(! llvm::isa<llvm::ConstantExpr>(value));

        mapGlaValue(value);

        // TODO: GOO: uint functionality
        bool notSigned = false;
        if (notSigned) {
            emitGlaConstructor(shader, value->getType());
            shader << "(";
        }

        shader << valueMap[value]->c_str();
        if (notSigned)
            shader << ")";
    }


    void emitGlaStructName(std::ostringstream& out, const llvm::Type* structType)
    {
        std::string name = structNameMap[structType];
        assert(name.c_str());
        out << name;
    }

    std::string getGlaStructField(const llvm::Type* structType, int index, llvm::MDNode* mdAggregate = 0)
    {
        std::string name;

        if (mdAggregate) {
            int aggOp = GetAggregateMdNameOp(index);
            if (mdAggregate->getNumOperands() > aggOp) {
                name = mdAggregate->getOperand(aggOp)->getName();

                return name;
            } else
                UnsupportedFunctionality("missing name in aggregate", EATContinue);
        }

        name.append("member");
            
        const size_t bufSize = 10;
        char buf[bufSize];
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
    void emitConstantInitializer(std::ostringstream& out, const llvm::Constant* constant, llvm::Type* type)
    {
        bool isZero;

        if (! constant || IsUndef(constant))
            isZero = true;
        else if (llvm::isa<llvm::ConstantAggregateZero>(constant))
            isZero = true;
        else
            isZero = false;

        switch (type->getTypeID()) {

        case llvm::Type::IntegerTyID:
            {
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

        case llvm::Type::FloatTyID:
            {
                if (isZero)
                    emitFloatConstant(out, 0.0);
                else
                    emitFloatConstant(out, GetConstantFloat(constant));
                break;
            }

        case llvm::Type::VectorTyID:
        case llvm::Type::ArrayTyID:
        case llvm::Type::StructTyID:
            {
                emitGlaConstructor(out, type);
                out << "(";

                int numElements = 0;
                llvm::Constant* splatValue = 0;
                const llvm::ConstantDataVector* dataVector = 0;

                if (const llvm::VectorType* vectorType = llvm::dyn_cast<llvm::VectorType>(type)) {
                    if (! isZero) {
                        // If all vector elements are equal, we only need to emit one
                        bool same = true;
                        dataVector = llvm::dyn_cast<llvm::ConstantDataVector>(constant);
                        if (dataVector) {
                            splatValue = dataVector->getSplatValue();
                            if (! splatValue)
                                same = false;
                        } else if (! isZero) {
                            for (int op = 1; op < vectorType->getNumElements(); ++op) {
                                if (llvm::dyn_cast<const llvm::Constant>(constant->getOperand(0)) != llvm::dyn_cast<const llvm::Constant>(constant->getOperand(op))) {
                                    same = false;
                                    break;
                                }
                            }

                            if (same)
                                splatValue = llvm::dyn_cast<llvm::Constant>(constant->getOperand(0));
                        }
                    }
                    numElements = vectorType->getNumElements();
                } else if (const llvm::ArrayType*  arrayType = llvm::dyn_cast<llvm::ArrayType>(type))
                    numElements = arrayType->getNumElements();
                else if (const llvm::StructType* structType = llvm::dyn_cast<llvm::StructType>(type))
                    numElements = structType->getNumElements();
                else
                    assert(0 && "Constant aggregate type");

                if (isZero || splatValue)
                    emitConstantInitializer(out, isZero ? 0 : splatValue, type->getContainedType(0));
                else {
                    for (int op = 0; op < numElements; ++op) {
                        if (op > 0)
                            out << ", ";

                        const llvm::Constant* constElement;
                        if (dataVector)
                            constElement = dataVector->getElementAsConstant(op);
                        else
                            constElement = llvm::dyn_cast<llvm::Constant>(constant->getOperand(op));

                        emitConstantInitializer(out, constElement,
                                                type->getContainedType(type->getNumContainedTypes() > 1 ? op : 0));
                    }
                }

                out << ")";
                break;
            }

        default:
            assert(0 && "Constant type in Bottom IR");
        }
    }

    void emitInitializeAggregate(std::ostringstream& out, std::string name, const llvm::Constant* constant)
    {
        if (constant && IsDefined(constant) && ! IsScalar(constant) && ! AreAllDefined(constant)) {
            // For a vector or array with undefined elements, propagate the defined elements
            if (const llvm::ConstantVector* constVec = llvm::dyn_cast<llvm::ConstantVector>(constant)) {
                for (int op = 0; op < constVec->getNumOperands(); ++op) {
                    if (IsDefined(constVec->getOperand(op))) {
                        out << std::endl << "    " << name;
                        out << "." << mapComponentToSwizzleChar(op) << " = ";
                        out << getGlaValue(constVec->getOperand(op));
                        out << ";";
                    }
                }
            } else if (const llvm::ConstantArray* constArray = llvm::dyn_cast<llvm::ConstantArray>(constant)) {
                for (int op = 0; op < constArray->getNumOperands(); ++op) {
                    if (IsDefined(constArray->getOperand(op))) {
                        out << std::endl << "    " << name;
                        out << "[" << op << "] = ";
                        out << getGlaValue(constArray->getOperand(op));
                        out << ";";
                    }
                }
            } else if (const llvm::ConstantStruct* constStruct = llvm::dyn_cast<llvm::ConstantStruct>(constant)) {
                for (int op = 0; op < constStruct->getNumOperands(); ++op) {
                    if (IsDefined(constStruct->getOperand(op))) {
                        out << std::endl << "    " << name;
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
            valueMap[value] = new std::string(name);  // TODO: Goo: memory: need to delete these? (can probably do this without ever newing; just pass in a pointer)

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
            emitGlaConstructor(shader, inst->getType(), argCount);
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
    std::string traverseGep(const llvm::Value* value, llvm::MDNode* mdAggregate)
    {
        std::string gepName;

        if (const llvm::GetElementPtrInst* gepInst = llvm::dyn_cast<llvm::GetElementPtrInst>(value)) {

            // Start at operand 2 since indices 0 and 1 give you the base and are handled before traverseGep
            const llvm::Type* gepType = gepInst->getPointerOperandType()->getContainedType(0);
            for (unsigned int op = 2; op < gepInst->getNumOperands(); ++op)
                dereferenceGep(gepType, gepName, gepInst->getOperand(op), -1, op-2, mdAggregate);

        } else if (const llvm::InsertValueInst* insertValueInst = llvm::dyn_cast<const llvm::InsertValueInst>(value)) {

            const llvm::Type* gepType = insertValueInst->getAggregateOperand()->getType();
            for (llvm::InsertValueInst::idx_iterator iter = insertValueInst->idx_begin(), end = insertValueInst->idx_end();  iter != end; ++iter)
                dereferenceGep(gepType, gepName, 0, *iter, 0, mdAggregate);

        } else if (const llvm::ExtractValueInst* extractValueInst = llvm::dyn_cast<const llvm::ExtractValueInst>(value)) {

            const llvm::Type* gepType = extractValueInst->getAggregateOperand()->getType();
            for (llvm::ExtractValueInst::idx_iterator iter = extractValueInst->idx_begin(), end = extractValueInst->idx_end();  iter != end; ++iter)
                dereferenceGep(gepType, gepName, 0, *iter, 0, mdAggregate);

        } else {
            assert(0 && "non-GEP in traverseGEP");
        }

        return gepName;
    }

    // Traverse one step of a dereference chain and append to a string
    // For constant indices, pass it in index.  Otherwise, provide it through gepOp (index will not be used)
    void dereferenceGep(const llvm::Type*& type, std::string& name, llvm::Value* operand, int index, int depth, llvm::MDNode*& mdAggregate)
    {
        if (operand) {
            if (llvm::isa<const llvm::ConstantInt>(operand))
                index = GetConstantInt(operand);
            else
                index = -1;
        }

        switch (type->getTypeID()) {
        case llvm::Type::ArrayTyID:
            assert(operand || index >= 0);

            name.append("[");

            if (index >= 0) {
                const size_t bufSize = 10;
                char buf[bufSize];
                snprintf(buf, bufSize, "%d", index);
                name.append(buf);
            } else
                name.append(getGlaValue(operand));

            name.append("]");

            type = type->getContainedType(0);
            break;
        case llvm::Type::StructTyID:
            assert(index >= 0);

            name.append(".");
            name.append(getGlaStructField(type, index, mdAggregate));

            // Deference the metadata aggregate 
            if (mdAggregate) {
                int aggOp = GetAggregateMdSubAggregateOp(depth);
                if (mdAggregate->getNumOperands() <= aggOp) {
                    UnsupportedFunctionality("not enough mdAggregate operands", EATContinue);
                    mdAggregate = 0;
                } else 
                    mdAggregate = llvm::dyn_cast<llvm::MDNode>(mdAggregate->getOperand(aggOp));
            }

            type = type->getContainedType(index);
            break;
        default:
            assert(0 && "Dereferencing non array/struct");
            break;
        }
    }

    void mapGlaIOIntrinsic(const llvm::IntrinsicInst* llvmInstruction, bool input)
    {
        // Key issue:  We can figure out a slot type from the instruction, 
        // but it's not necessarily the type of the whole variable getting read,
        // just a slice of it.  So, need to get the whole type, from metadata.  
        //
        // Further, there will be multiple instructions (multiple Value*)
        // that fill in the same whole variable, making a many:1 mapping
        // between Value* and input declaration.
        
        std::string name;
        llvm::Type* type;
        EMdInputOutput mdQual;
        int layoutLocation;
        EMdPrecision mdPrecision;
        EMdTypeLayout mdLayout;
        const llvm::MDNode* mdAggregate;
        const llvm::MDNode* dummySampler;
        const llvm::MDNode* mdNode = llvmInstruction->getMetadata(input ? gla::InputMdName : gla::OutputMdName);
        if (! mdNode || ! gla::CrackIOMd(mdNode, name, mdQual, type, mdLayout, mdPrecision, layoutLocation, dummySampler, mdAggregate)) {
            // This path should not exist; it is a backup path for missing metadata.
            // TODO: LunarGOO functionality: fix missing metadata instruction operands.
            UnsupportedFunctionality("couldn't get metadata for input instruction", EATContinue);

            // emulate (through imperfect guessing) the missing metadata
            name = llvmInstruction->getName();
            type = llvmInstruction->getType();
            mdLayout = EMtlNone;
            mdPrecision = EMpNone;
            layoutLocation = 0;
        }
        bool notSigned = mdLayout == EMtlUnsigned;

        // add the dereference syntax
        // TODO: Goo functionality: outputs don't yet have layout slot bases, so indexing into big things will be incorrect
        std::string derefName = name;
        int slotOffset = GetConstantInt(llvmInstruction->getOperand(0)) - layoutLocation;
        dereferenceName(derefName, type, slotOffset);

        EInterpolationMethod interpMethod = EIMLast;
        EInterpolationLocation interpLocation = EILFragment;

        switch (llvmInstruction->getIntrinsicID()) {
        case llvm::Intrinsic::gla_writeData:
        case llvm::Intrinsic::gla_fWriteData:
            // First, emit declaration
            declareIOVariable(type, name, EVQOutput, interpMethod, interpLocation, mdNode);

            // Now, emit the write
            newLine();
            shader << derefName << " = ";
            if (notSigned) {
                emitUintConverter(llvmInstruction->getOperand(2)->getType());
                shader << "(";
            }
            emitGlaOperand(llvmInstruction->getOperand(2));
            if (notSigned)
                shader << ")";
            shader << ";";
            break;
        case llvm::Intrinsic::gla_readData:
        case llvm::Intrinsic::gla_fReadData:
        case llvm::Intrinsic::gla_fReadInterpolant:
            // A pipeline read only emits declaration of the input name, not the use.
            // The use will come later by looking up the mapping between Value* and
            // the name associated with it.
            //
            // NOTE!  The mapped name will include the deferences, like "foo[3].v", *in the name*.
            
            // Add an int-based constructor around it, if needed.
            if (notSigned)
                intWrap(derefName, llvmInstruction->getType());

            if (addNewVariable(llvmInstruction, derefName)) {

                if (llvmInstruction->getIntrinsicID() == llvm::Intrinsic::gla_fReadInterpolant) {
                    EInterpolationMode mode = GetConstantInt(llvmInstruction->getOperand(2));
                    CrackInterpolationMode(mode, interpMethod, interpLocation);
                } else
                    interpMethod = EIMNone;   // needed for 'flat' with non-interpolation 'in'

                declareIOVariable(type, name, EVQInput, interpMethod, interpLocation, mdNode);
            }

            break;
        default:
            UnsupportedFunctionality("IO Intrinsic");
            break;
        }
    }

    //
    // *Textually* deference a name string.
    //
    void dereferenceName(std::string& name, const llvm::Type* type, int slotOffset)
    {
        // Operates recursively...

        if (type->getTypeID() == llvm::Type::PointerTyID) {
            type = llvm::dyn_cast<llvm::PointerType>(type)->getContainedType(0);

            dereferenceName(name, type, slotOffset);
        } else if (type->getTypeID() == llvm::Type::VectorTyID) {
            // should be at the bottom of recursion now

            return;
        } else if (type->getTypeID() == llvm::Type::StructTyID) {
            name = name + ".";
        } else if (type->getTypeID() == llvm::Type::ArrayTyID) {
            char buf[10];
            snprintf(buf, sizeof(buf), "%d", slotOffset);
            name = name + "[" + buf + "]";
            const llvm::ArrayType* arrayType = llvm::dyn_cast<const llvm::ArrayType>(type);
        
            dereferenceName(name, arrayType->getContainedType(0), 0);
        }
    }

    void intWrap(std::string& name, llvm::Type* type)
    {
        std::string wrapped;

        if (GetComponentCount(type) == 1)
            wrapped = "int(";
        else {
            wrapped.append("ivec");
            switch (GetComponentCount(type)) {
            case 2:  wrapped.append("2(");    break;
            case 3:  wrapped.append("3(");    break;
            case 4:  wrapped.append("4(");    break;
            default: UnsupportedFunctionality("int wrapped type");  break;
            }
        }
        wrapped.append(name);
        name = wrapped;
        name.append(")");
    }

    void emitUintConverter(llvm::Type* type)
    {
        if (GetComponentCount(type) == 1)
            shader << "uint";
        else {
            shader << "uvec";
            switch (GetComponentCount(type)) {
            case 2:  shader << "2";    break;
            case 3:  shader << "3";    break;
            case 4:  shader << "4";    break;
            default: UnsupportedFunctionality("uint converter type");  break;
            }
        }
    }

    // mapping from LLVM values to Glsl variables
    std::map<const llvm::Value*, std::string*> valueMap;

    // map to track structure names tracked in the module
    std::map<const llvm::Type*, std::string> structNameMap;

    // set to track what globals are already declared,
    // it potentially only includes globals that are in danger of multiple declaration
    std::set<std::string> globallyDeclared;

    std::ostringstream globalStructures;
    std::ostringstream globalDeclarations;
    std::ostringstream globalInitializers;
    bool appendInitializers;
    std::ostringstream shader;
    int indentLevel;
    int lastVariable;
    bool obfuscate;
    int version;
    EProfile profile;
    EShLanguage language;
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
void gla::GlslTarget::getOp(const llvm::Instruction* llvmInstruction, std::string& s, int& unaryOperand, bool& nested)
{
    nested = false;

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
        case 1: s = "int(uint";  break;
        case 2: s = "ivec2(uvec2"; break;
        case 3: s = "ivec3(uvec3"; break;
        case 4: s = "ivec4(uvec4"; break;
        default: UnsupportedFunctionality("Can only convert scalars and vectors");
        }
        unaryOperand = 0;
        nested = true;
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

    // TODO: Goo:  This loop will disappear when conditional loops in BottomToGLSL properly updates valueMap
    for (llvm::Instruction::const_op_iterator i = llvmInstruction->op_begin(), e = llvmInstruction->op_end(); i != e; ++i) {
        llvm::Instruction* inst = llvm::dyn_cast<llvm::Instruction>(*i);
        if (inst) {
            if (valueMap[*i] == 0)
                add(inst, lastBlock);
            }
    }

    // If the instruction is referenced outside of the current scope
    // (e.g. inside a loop body), then add a (global) declaration for it.
    if (referencedOutsideScope) {
        mapGlaValue(llvmInstruction, referencedOutsideScope);
    }

    bool nested;
    getOp(llvmInstruction, charOp, unaryOperand, nested);

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
        if (nested)
            shader << ")";
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

            if (const llvm::GetElementPtrInst* gepInstr = llvm::dyn_cast<llvm::GetElementPtrInst>(llvmInstruction->getOperand(0))) {
                // See if we have metadata describing a uniform variable to declare
                std::string name;
                llvm::MDNode* mdUniform = llvmInstruction->getMetadata(UniformMdName);
                llvm::MDNode* mdAggregate = 0;

                // get the name
                if (mdUniform)
                    name = mdUniform->getOperand(0)->getName();
                else {
                    UnsupportedFunctionality("missing metadata on load", EATContinue);
                    std::string* prevName = valueMap[gepInstr];
                    if (prevName)
                        name = *prevName;
                    else
                        name = gepInstr->getOperand(0)->getName();
                }

                if (mdUniform && mdUniform->getNumOperands() >= 5)
                    mdAggregate = llvm::dyn_cast<llvm::MDNode>(mdUniform->getOperand(4));
                else
                    UnsupportedFunctionality("missing metadata operands", EATContinue);

                // Process the base (we skipped "case llvm::Instruction::GetElementPtr" when called with that earlier)
                // For GEP, which always returns a pointer, traverse the dereference chain and store it.
                name.append(traverseGep(gepInstr, mdAggregate));
                if (name[0] == '.')
                    name = name.substr(1, std::string::npos);
                addNewVariable(gepInstr, name);

                // If we're loading from the result of a GEP, assign it to a new variable
                newLine();
                emitGlaValue(llvmInstruction);
                shader << " = ";
                emitGlaValue(gepInstr);
                shader << ";";
            } else if (llvm::isa<llvm::PHINode>(llvmInstruction->getOperand(0))) {
                // We want phis to use the same variable name created during phi declaration
                addNewVariable(llvmInstruction, *valueMap[llvmInstruction->getOperand(0)]);
            } else {
                std::string name = llvmInstruction->getOperand(0)->getName();
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
            if (const llvm::GetElementPtrInst* gepInstr = llvm::dyn_cast<llvm::GetElementPtrInst>(llvmInstruction->getOperand(1))) {
                // Process the base (we skipped "case llvm::Instruction::GetElementPtr" when called with that earlier)
                // For GEP, which always returns a pointer, traverse the dereference chain and store it.
                std::string* prevName = valueMap[gepInstr];
                std::string name;
                if (prevName)
                    name = *prevName;
                else
                    name = gepInstr->getOperand(0)->getName();
                name.append(traverseGep(gepInstr, 0));
                addNewVariable(gepInstr, name);
            }

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
        // For GEP, defer processing until we see it in a load, because
        // that's where the metadata went

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
        shader << traverseGep(extractValueInst, 0);
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
        shader << traverseGep(insertValueInst, 0);

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

        emitGlaConstructor(shader, llvmInstruction->getType());
        shader << "(";

        int sourceWidth = gla::GetComponentCount(llvmInstruction->getOperand(0));
        int resultWidth = gla::GetComponentCount(llvmInstruction);

        const llvm::Constant* mask = llvm::dyn_cast<const llvm::Constant>(llvmInstruction->getOperand(2));
        assert(llvm::isa<llvm::ConstantVector>(mask) || llvm::isa<llvm::ConstantAggregateZero>(mask));

        llvm::SmallVector<llvm::Constant*,4> elts;
        gla_llvm::GetElements(mask, elts);

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
    case llvm::Intrinsic::gla_writeData:
    case llvm::Intrinsic::gla_fWriteData:
        mapGlaIOIntrinsic(llvmInstruction, false);

        return;
    case llvm::Intrinsic::gla_readData:
    case llvm::Intrinsic::gla_fReadData:
    case llvm::Intrinsic::gla_fReadInterpolant:
        mapGlaIOIntrinsic(llvmInstruction, true);

        return;
    default:
        // fall through
        break;
    }

    EMdPrecision precision = getPrecision(llvmInstruction);

    // Handle texturing
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_queryTextureSize:

        newLine();
        emitGlaValue(llvmInstruction);
        shader << " = textureSize(";
        emitGlaOperand(llvmInstruction->getOperand(GetTextureOpIndex(ETOSamplerLoc)));
        if (llvmInstruction->getNumArgOperands() > 2) {
            // TODO: Goo: Test: 140: some textureSize() don't have 2nd argument
            shader << ", ";
            emitGlaOperand(llvmInstruction->getOperand(2));
        }
        shader << ");";
        return;

    case llvm::Intrinsic::gla_fQueryTextureLod:

        newLine();
        emitGlaValue(llvmInstruction);
        shader << " = textureQueryLod(";
        emitGlaOperand(llvmInstruction->getOperand(GetTextureOpIndex(ETOSamplerLoc)));
        shader << ", ";
        emitGlaOperand(llvmInstruction->getOperand(2));
        shader << ");";
        return;

    //case llvm::Intrinsic::gla_queryTextureLevels:
    // TODO: Goo: 430 Functionality: textureQueryLevels()

    case llvm::Intrinsic::gla_textureSample:
    case llvm::Intrinsic::gla_fTextureSample:
    case llvm::Intrinsic::gla_rTextureSample1:
    case llvm::Intrinsic::gla_fRTextureSample1:
    case llvm::Intrinsic::gla_rTextureSample2:
    case llvm::Intrinsic::gla_fRTextureSample2:
    case llvm::Intrinsic::gla_rTextureSample3:
    case llvm::Intrinsic::gla_fRTextureSample3:
    case llvm::Intrinsic::gla_rTextureSample4:
    case llvm::Intrinsic::gla_fRTextureSample4:
    case llvm::Intrinsic::gla_textureSampleLodRefZ:
    case llvm::Intrinsic::gla_fTextureSampleLodRefZ:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZ1:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZ1:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZ2:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZ2:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZ3:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZ3:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZ4:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZ4:
    case llvm::Intrinsic::gla_textureSampleLodRefZOffset:
    case llvm::Intrinsic::gla_fTextureSampleLodRefZOffset:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZOffset1:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZOffset1:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZOffset2:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZOffset2:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZOffset3:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZOffset3:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZOffset4:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZOffset4:
    case llvm::Intrinsic::gla_textureSampleLodRefZOffsetGrad:
    case llvm::Intrinsic::gla_fTextureSampleLodRefZOffsetGrad:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZOffsetGrad1:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZOffsetGrad1:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZOffsetGrad2:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZOffsetGrad2:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZOffsetGrad3:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZOffsetGrad3:
    case llvm::Intrinsic::gla_rTextureSampleLodRefZOffsetGrad4:
    case llvm::Intrinsic::gla_fRTextureSampleLodRefZOffsetGrad4:
    case llvm::Intrinsic::gla_texelFetchOffset:
    case llvm::Intrinsic::gla_fTexelFetchOffset:
    case llvm::Intrinsic::gla_texelGather:
    case llvm::Intrinsic::gla_fTexelGather:
    case llvm::Intrinsic::gla_texelGatherOffset:
    case llvm::Intrinsic::gla_fTexelGatherOffset:
    case llvm::Intrinsic::gla_texelGatherOffsets:
    case llvm::Intrinsic::gla_fTexelGatherOffsets:

        newLine();
        emitGlaValue(llvmInstruction);
        shader << " = ";
        emitGlaSampler(llvmInstruction, GetConstantInt(llvmInstruction->getOperand(GetTextureOpIndex(ETOFlag))));
        shader << "(";
        emitGlaOperand(llvmInstruction->getOperand(GetTextureOpIndex(ETOSamplerLoc)));
        shader << ", ";

        if(needsShadowRefZArg(llvmInstruction)) {

            // Construct a new vector of size coords+1 to hold coords and shadow ref
            int coordWidth = gla::GetComponentCount(llvmInstruction->getOperand(GetTextureOpIndex(ETOCoord)));
            assert(coordWidth < 4);

            llvm::Type* coordType = llvmInstruction->getOperand(GetTextureOpIndex(ETOCoord))->getType();

            if (coordType->isVectorTy())
                coordType = coordType->getContainedType(0);

            // RefZ must reside in 3rd component or higher, so detect single component case
            int buffer = (coordWidth == 1) ? 1 : 0;

            llvm::Type* vecType = llvm::VectorType::get(coordType, coordWidth + buffer + 1);

            emitGlaType(shader, precision, vecType);

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

        if(needsLodArg(llvmInstruction)) {
            shader << ", ";
            emitGlaOperand(llvmInstruction->getOperand(GetTextureOpIndex(ETOBiasLod)));
        }

        if(IsGradientTexInst(llvmInstruction)) {
            shader << ", ";
            emitGlaOperand(llvmInstruction->getOperand(GetTextureOpIndex(ETODPdx)));
            shader << ", ";
            emitGlaOperand(llvmInstruction->getOperand(GetTextureOpIndex(ETODPdy)));
        }

        if (needsOffsetArg(llvmInstruction)) {
            shader << ", ";
            emitGlaOperand(llvmInstruction->getOperand(GetTextureOpIndex(ETOOffset)));
        }

        if(needsBiasArg(llvmInstruction)) {
            shader << ", ";
            emitGlaOperand(llvmInstruction->getOperand(GetTextureOpIndex(ETOBiasLod)));
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
        gla_llvm::GetElements(mask, elts);

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
            emitGlaType(shader, precision, llvmInstruction->getType());
            shader << "(";
            emitGlaOperand(src);
            shader << ");";

            return;
        }

        // Case 2:  it's sequential .xy...  subsetting a vector.
        // use a constructor to subset the vector to a vector
        if (srcVectorWidth > 1 && dstVectorWidth > 1 && IsIdentitySwizzle(elts)) {
            emitGlaType(shader, precision, llvmInstruction->getType());
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
    case llvm::Intrinsic::gla_sign:         callString = "sign";        callArgs = 1; break;
    case llvm::Intrinsic::gla_fFloor:       callString = "floor";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fCeiling:     callString = "ceil";        callArgs = 1; break;
    case llvm::Intrinsic::gla_fRoundEven:   callString = "roundEven";   callArgs = 1; break;
    case llvm::Intrinsic::gla_fRoundZero:   callString = "trunc";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fRoundFast:   callString = "round";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fFraction:    callString = "fract";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fModF:        callString = "modf";        callArgs = 2; break;
    case llvm::Intrinsic::gla_fMix:         callString = "mix";         callArgs = 3; break;
    case llvm::Intrinsic::gla_fbMix:        callString = "mix";         callArgs = 3; break;
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
    case llvm::Intrinsic::gla_fIntBitsTofloat:  callString = "intBitsToFloat";      callArgs = 1; break;
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
    case llvm::Intrinsic::gla_fUnpackUnorm2x16:  callString = "unpackUnorm2x16";    callArgs = 1; break;

    case llvm::Intrinsic::gla_fPackSnorm2x16:    callString = "packSnorm2x16";      callArgs = 1; break;
    case llvm::Intrinsic::gla_fUnpackSnorm2x16:  callString = "unpackSnorm2x16";    callArgs = 1; break;

    case llvm::Intrinsic::gla_fPackHalf2x16:     callString = "packHalf2x16";       callArgs = 1; break;        
    case llvm::Intrinsic::gla_fUnpackHalf2x16:   callString = "unpackHalf2x16";     callArgs = 1; break;        

    case llvm::Intrinsic::gla_fPackUnorm4x8:     callString = "packUnorm4x8";       callArgs = 1; break;
    case llvm::Intrinsic::gla_fPackSnorm4x8:     callString = "packSnorm4x8";       callArgs = 1; break;

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
    shader << " = " << std::string(call->getCalledFunction()->getName()) << "(";
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

    // #version...
    printf("#version %d", version);
    if (version >= 150 && profile != ENoProfile) {
        switch (profile) {
        case ECoreProfile:          printf(" core");          break;
        case ECompatibilityProfile: printf(" compatibility"); break;
        case EEsProfile:            printf(" es");            break;
        default:
            UnsupportedFunctionality("profile");
            break;
        }
    }
    printf("\n");

    // rest of shader...
    printf("%s%s%s", globalStructures.str().c_str(), globalDeclarations.str().c_str(), shader.str().c_str());
}
