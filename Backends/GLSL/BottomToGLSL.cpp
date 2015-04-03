//===- BottomToGLSL.cpp - Translate bottom IR to GLSL ---------------------===//
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
// A GLSL back end for LunarGLASS.
//
// Inherits from 
//   - BackEnd to supply back end information to LunarGLASS.
//   - BackEndTranslator to translate to GLSL, by implementing the methods called
//     by the bottom translator.
// Factories for each of these appear below.
//
// It writes the resulting GLSL shader to standard output.
//
//===----------------------------------------------------------------------===//

#define _CRT_SECURE_NO_WARNINGS
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
#include "Core/Revision.h"
#include "Core/Exceptions.h"
#include "Core/Util.h"
#include "Core/BottomIR.h"
#include "Core/Backend.h"
#include "Core/PrivateManager.h"
#include "Core/TopBuilder.h"
#include "Core/metadata.h"
#include "Core/Passes/Util/ConstantUtil.h"
#include "GlslTarget.h"

// glslang includes
#include "glslang/Public/ShaderLang.h"
#include "glslang/MachineIndependent/Versions.h"

// LLVM includes
#pragma warning(push, 1)
#include "llvm/IR/Module.h"
#pragma warning(pop)

namespace {
    bool UseLogicalIO = true;

    bool ValidIdentChar(int c)
    {
        return c == '_' || 
               (c >= 'a' && c <= 'z') ||
               (c >= 'A' && c <= 'Z') ||
               (c >= '0' && c <= '9');
    }

    // Taken from VS <function> hash:
    unsigned int _Hash_seq(const unsigned char *_First, unsigned int _Count)
    {	// FNV-1a hash function for bytes in [_First, _First+_Count)
        const unsigned int _FNV_offset_basis = 2166136261U;
        const unsigned int _FNV_prime = 16777619U;

        unsigned int _Val = _FNV_offset_basis;
        for (unsigned int _Next = 0; _Next < _Count; ++_Next)
        {	// fold in another byte
            _Val ^= (unsigned int)_First[_Next];
            _Val *= _FNV_prime;
        }

        return (_Val);
    }

    void IntToString(unsigned int i, std::string& string)
    {
        char buf[2];
        buf[1] = 0;
        int radix = 36;
        while (i > 0) {
            unsigned int r = i % radix;
            if (r < 10) 
                buf[0] = '0' + r;
            else
                buf[0] = 'a' + r - 10;
            string.append(buf);
            i = i / radix;
        }
    }

};

//
// Implement the GLSL backend
//
class GlslBackEnd : public gla::BackEnd {
public:
    GlslBackEnd()
    {
        //decompose[gla::EDiClamp] = true;
        //decompose[gla::EDiMax] = true;
        //decompose[gla::EDiMin] = true;
        //decompose[gla::EDiSmoothStep] = true;
        //decompose[gla::EDiFilterWidth] = true;
    }
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

    //virtual bool useColumnBasedMatrixIntrinsics()
    //{
    //    return true;
    //}

    virtual bool useLogicalIo()
    {
        return UseLogicalIO;
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
};

class MetaType {
public:
    MetaType() : precision(gla::EMpNone), matrix(false), notSigned(false), block(false), mdAggregate(0), mdSampler(0) { }
    std::string name;
    gla::EMdPrecision precision;
    bool matrix;
    bool notSigned;
    bool block;
    const llvm::MDNode* mdAggregate;
    const llvm::MDNode* mdSampler;
};

class Assignment;

class gla::GlslTarget : public gla::GlslTranslator {
public:
    GlslTarget(Manager* m, bool obfuscate, bool filterInactive, int substitutionLevel) :
        GlslTranslator(m, obfuscate, filterInactive, substitutionLevel),
        appendInitializers(false),
        indentLevel(0), lastVariable(0)
    {
        #ifdef _WIN32
            unsigned int oldFormat = _set_output_format(_TWO_DIGIT_EXPONENT);
        #endif
        indentString = "\t";
    }

    ~GlslTarget()
    {
        while (! toDelete.empty()) {
            delete toDelete.back();
            toDelete.pop_back();
        }
            
        delete generatedShader;
        delete indexShader;

        for (std::map<const llvm::Value*, std::string*>::const_iterator it = nonConvertedMap.begin(); it != nonConvertedMap.end(); ++it)
            delete it->second;
        for (std::map<const llvm::Value*, std::string*>::const_iterator it = valueMap.begin(); it != valueMap.end(); ++it)
            delete it->second;
        for (std::map<std::string, const std::string*>::const_iterator it = constMap.begin(); it != constMap.end(); ++it)
            delete it->second;
    }

    virtual void start(llvm::Module& module)
    {
        // Call this before doing actual translation.

        // The following information wasn't available at construct time:
        version = manager->getVersion();
        profile = (EProfile)manager->getProfile();
        stage = (EShLanguage)manager->getStage();
        usingSso = version >= 410 || manager->getRequestedExtensions().find("GL_ARB_separate_shader_objects") != manager->getRequestedExtensions().end();

        // Set up noStaticUse() cache
        const llvm::NamedMDNode* mdList = module.getNamedMetadata(NoStaticUseMdName);
        if (mdList) {
            for (unsigned int m = 0; m < mdList->getNumOperands(); ++m)
                noStaticUseSet.insert(mdList->getOperand(m));
        }

        // Get the top-levels modes for this shader.
        int mdInt;
        switch (stage) {
        case EShLangVertex:
            break;

        case EShLangTessControl:
            // output vertices is not optional
            globalStructures << "layout(vertices = " << GetMdNamedInt(module, gla::NumVerticesMdName) << ") out;" << std::endl;
            break;

        case EShLangTessEvaluation:
            // input primitives are not optional
            globalStructures << "layout(";
            switch (GetMdNamedInt(module, gla::InputPrimitiveMdName)) {
            case EMlgTriangles:
                globalStructures << "triangles";
                switch (GetMdNamedInt(module, gla::VertexOrderMdName)) {
                case EMvoNone:
                    // ordering is optional
                    break;
                case EMvoCw:
                    globalStructures << ", cw";
                    break;
                case EMvoCcw:
                    globalStructures << ", ccw";
                    break;
                default:
                    UnsupportedFunctionality("tess eval input ordering", EATContinue);
                    break;
                }
                break;
            case EMlgQuads:
                globalStructures << "quads";
                break;
            case EMlgIsolines:
                globalStructures << "isolines";
                break;
            default:
                UnsupportedFunctionality("tess eval input primitive", EATContinue);
                break;
            }
            globalStructures << ") in;" << std::endl;

            // vertex spacing is optional
            mdInt = GetMdNamedInt(module, gla::VertexSpacingMdName);
            if (mdInt) {
                globalStructures << "layout(";
                switch (mdInt) {
                case EMvsEqual:
                    globalStructures << "equal_spacing";
                    break;
                case EMvsFractionalEven:
                    globalStructures << "fractional_even_spacing";
                    break;
                case EMvsFractionalOdd:
                    globalStructures << "fractional_odd_spacing";
                    break;
                default:
                    UnsupportedFunctionality("tess eval vertex spacing", EATContinue);
                    break;
                }
                globalStructures << ") in;" << std::endl;
            }

            if (GetMdNamedInt(module, gla::PointModeMdName))
                globalStructures << "layout(point_mode) in;" << std::endl;
            break;

        case EShLangGeometry:
            // input primitives are not optional
            globalStructures << "layout(";
            switch (GetMdNamedInt(module, gla::InputPrimitiveMdName)) {
            case EMlgPoints:
                globalStructures << "points";
                break;
            case EMlgLines:
                globalStructures << "lines";
                break;
            case EMlgLinesAdjacency:
                globalStructures << "lines_adjacency";
                break;
            case EMlgTriangles:
                globalStructures << "triangles";
                break;
            case EMlgTrianglesAdjacency:
                globalStructures << "triangles_adjacency";
                break;
            default:
                UnsupportedFunctionality("geometry input primitive", EATContinue);
                break;
            }
            globalStructures << ") in;" << std::endl;

            // invocations is optional
            mdInt = GetMdNamedInt(module, gla::InvocationsMdName);
            if (mdInt)
                globalStructures << "layout(invocations = " << mdInt << ") in;" << std::endl;

            // output primitives are not optional
            globalStructures << "layout(";
            switch (GetMdNamedInt(module, gla::OutputPrimitiveMdName)) {
            case EMlgPoints:
                globalStructures << "points";
                break;
            case EMlgLineStrip:
                globalStructures << "line_strip";
                break;
            case EMlgTriangleStrip:
                globalStructures << "triangle_strip";
                break;
            default:
                UnsupportedFunctionality("geometry output primitive", EATContinue);
                break;
            }
            globalStructures << ") out;" << std::endl;

            // max_vertices is not optional
            globalStructures << "layout(max_vertices = " << GetMdNamedInt(module, gla::NumVerticesMdName) << ") out;" << std::endl;
            break;

        case EShLangFragment:
            if (GetMdNamedInt(module, PixelCenterIntegerMdName))
                globalStructures << "layout(pixel_center_integer) in;" << std::endl;

            if (GetMdNamedInt(module, OriginUpperLeftMdName))
                globalStructures << "layout(origin_upper_left) in;" << std::endl;

            break;

        case EShLangCompute:
            break;

        default:
            UnsupportedFunctionality("shader stage", EATContinue);
            break;
        }
    }
    virtual void end(llvm::Module&);

    void addGlobal(const llvm::GlobalVariable* global);
    void addGlobalConst(const llvm::GlobalVariable* global);
    void addIoDeclaration(gla::EVariableQualifier qualifier, const llvm::MDNode* mdNode);
    void startFunctionDeclaration(const llvm::Type* type, llvm::StringRef name);
    void addArgument(const llvm::Value* value, bool last);
    void endFunctionDeclaration();
    void startFunctionBody();
    void endFunctionBody();
    void addInstruction(const llvm::Instruction* llvmInstruction, bool lastBlock, bool referencedOutsideScope=false);
    bool needCanonicalSwap(const llvm::Instruction* instr) const;

    void declarePhiCopy(const llvm::Value* dst);
    void declarePhiAlias(const llvm::Value* dst) { }  // since we will do aliasing, there is no need to declare the intermediate variable
    void addPhiCopy(const llvm::Value* dst, const llvm::Value* src);
    void addPhiAlias(const llvm::Value* dst, const llvm::Value* src);
    void addIf(const llvm::Value* cond, bool invert = false);
    void addElse();
    void addEndif();
    void beginConditionalLoop();
    void beginSimpleConditionalLoop(const llvm::CmpInst* cmp, const llvm::Value* op1, const llvm::Value* op2, bool invert = false);
    void beginForLoop(const llvm::PHINode* phi, llvm::ICmpInst::Predicate, unsigned bound, unsigned increment);
    void beginSimpleInductiveLoop(const llvm::PHINode* phi, const llvm::Value* count);
    void beginInductiveLoop();
    void beginLoop();
    void endLoop();
    void addLoopExit(const llvm::Value* condition = 0, bool invert = false);
    void addLoopBack(const llvm::Value* condition = 0, bool invert = false);
    void addDiscard();
    void print();

// protected:
    bool filteringIoNode(const llvm::MDNode*);

    void newLine();
    void newScope();
    void leaveScope();

    void addStructType(std::ostringstream& out, std::string& name, const llvm::Type* structType, const llvm::MDNode* mdAggregate, bool block);
    void mapVariableName(const llvm::Value* value, std::string& name);
    void mapExpressionString(const llvm::Value* value, const std::string& name);
    bool getExpressionString(const llvm::Value* value, std::string& name) const;
    bool samplerIsUint(llvm::Value* sampler) const;
    void makeNewVariableName(const llvm::Value* value, std::string& name, const char* rhs);
    void makeNewVariableName(const char* base, std::string& name);
    void makeHashName(const char* prefix, const char* key, std::string& name);
    void makeObfuscatedName(std::string& name);
    void canonicalizeName(std::string& name);
    void makeExtractElementStr(const llvm::Instruction* llvmInstruction, std::string& str);
    void mapPointerExpression(const llvm::Value* ptr, const llvm::Value* additionalToMap = 0);

    void emitGlaIntrinsic(std::ostringstream&, const llvm::IntrinsicInst*);
    void emitGlaCall(std::ostringstream&, const llvm::CallInst*);
    void emitGlaPrecision(std::ostringstream&, EMdPrecision precision);
    void emitComponentCountToSwizzle(std::ostringstream&, int numComponents);
    void emitComponentToSwizzle(std::ostringstream&, int component);
    void emitMaskToSwizzle(std::ostringstream&, int mask);
    void emitGlaSamplerFunction(std::ostringstream&, const llvm::IntrinsicInst* llvmInstruction, int texFlags);
    void emitNamelessConstDeclaration(const llvm::Value*, const llvm::Constant*);
    void emitVariableDeclaration(EMdPrecision precision, llvm::Type* type, const std::string& name, EVariableQualifier qualifier, 
                                 const llvm::Constant* constant = 0, const llvm::MDNode* mdIoNode = 0);
    int emitGlaType(std::ostringstream& out, EMdPrecision precision, EVariableQualifier qualifier, llvm::Type* type, 
                    bool ioRoot = false, const llvm::MDNode* mdNode = 0, int count = -1, bool araryChild = false);
    bool decodeMdTypesEmitMdQualifiers(std::ostringstream& out, bool ioRoot, const llvm::MDNode* mdNode, llvm::Type*& type, bool arrayChild, MetaType&);
    void emitGlaArraySize(std::ostringstream&, int arraySize);
    void emitGlaSamplerType(std::ostringstream&, const llvm::MDNode* mdSamplerNode);
    void emitGlaInterpolationQualifier(EVariableQualifier qualifier, EInterpolationMethod interpMethod, EInterpolationLocation interpLocation);
    void emitGlaLayout(std::ostringstream&, gla::EMdTypeLayout layout, int location, bool binding);
    void emitGlaConstructor(std::ostringstream&, llvm::Type* type, int count = -1);
    void emitGlaValueDeclaration(const llvm::Value* value, const char* rhs, bool forceGlobal = false);
    void emitGlaValue(std::ostringstream&, const llvm::Value* value, const char* rhs);
    void emitGlaOperand(std::ostringstream&, const llvm::Value* value);
    void emitNonconvertedGlaValue(std::ostringstream&, const llvm::Value* value);
    void propagateNonconvertedGlaValue(const llvm::Value* dst, const llvm::Value* src);
    std::string* mapGlaValueAndEmitDeclaration(const llvm::Value* value);
    void emitFloatConstant(std::ostringstream& out, float f);
    void emitConstantInitializer(std::ostringstream&, const llvm::Constant* constant, llvm::Type* type);
    void emitInitializeAggregate(std::ostringstream&, std::string name, const llvm::Constant* constant);
    void emitGlaSwizzle(std::ostringstream&, int glaSwizzle, int width, llvm::Value* source = 0);
    void emitGlaSwizzle(std::ostringstream&, const llvm::SmallVectorImpl<llvm::Constant*>& elts);
    void emitGlaWriteMask(std::ostringstream&, const llvm::SmallVectorImpl<llvm::Constant*>& elts);
    int getDefinedCount(const llvm::SmallVectorImpl<llvm::Constant*>& elts);
    void emitVectorArguments(std::ostringstream&, bool &firstArg, const llvm::IntrinsicInst *inst, int operand);
    void emitGlaMultiInsertRHS(std::ostringstream& out, const llvm::IntrinsicInst* inst);
    void emitGlaMultiInsert(std::ostringstream& out, const llvm::IntrinsicInst* inst);
    void emitMapGlaIOIntrinsic(const llvm::IntrinsicInst* llvmInstruction, bool input);
    void emitInvariantDeclarations(llvm::Module&);
    void buildFullShader();

    // 'gep' is potentially a gep, either an instruction or a constantExpr.
    // See which one, if any, and return it.
    // Return 0 if not a gep.
    const llvm::GetElementPtrInst* getGepAsInst(const llvm::Value* gep)
    {
        const llvm::GetElementPtrInst* gepInst = llvm::dyn_cast<const llvm::GetElementPtrInst>(gep);
        if (gepInst)
            return gepInst;

        const llvm::ConstantExpr *constantGep = llvm::dyn_cast<const llvm::ConstantExpr>(gep);
        if (constantGep) {
            // seems LLVM's "Instruction *ConstantExpr::getAsInstruction()" is declared wrong that constantGEP can't be const
            llvm::Instruction *instruction = const_cast<llvm::ConstantExpr*>(constantGep)->getAsInstruction();
            toDelete.push_back(instruction);
            gepInst = llvm::dyn_cast<llvm::GetElementPtrInst>(instruction);
        }

        return gepInst;
    }

    std::string traverseGep(const llvm::Instruction* instr, EMdTypeLayout* mdTypeLayout = 0);
    void dereferenceGep(const llvm::Type*& type, std::string& name, llvm::Value* operand, int index, const llvm::MDNode*& mdAggregate, EMdTypeLayout* mdTypeLayout = 0);

    bool cheapExpression(const std::string&);
    bool shouldSubstitute(const llvm::Instruction*);
    bool modifiesPrecision(const llvm::Instruction*);
    bool writeOnceAlloca(const llvm::Value*);
    int getSubstitutionLevel() const { return substitutionLevel; }

    // set of all IO mdNodes in the noStaticUse list
    std::set<const llvm::MDNode*> noStaticUseSet;

    // list of llvm Values to free on exit
    std::vector<llvm::Value*> toDelete;

    // mapping from LLVM values to Glsl variables
    std::map<const llvm::Value*, std::string*> valueMap;

    // mapping of the string representation of a constant's initializer to a const variable name;
    std::map<std::string, const std::string*> constMap;

    // all names that came from hashing, to ensure uniqueness
    std::set<std::string> hashedNames;

    std::map<std::string, int> canonMap;

    // Map from IO-related global variables, by name, to their mdNodes describing them.
    std::map<std::string, const llvm::MDNode*> mdMap;

    // For uint/matrix I/O names that need conversion, preserve the non-converted expression.
    std::map<const llvm::Value*, std::string*> nonConvertedMap;

    // map to track block names tracked in the module
    std::map<const llvm::Type*, std::string> blockNameMap;

    // Map to track structure names tracked in the module.  This could 
    // replicate a block entry if the block's type was reused to declare
    // a shadow variable.
    std::map<const llvm::Type*, std::string> structNameMap;

    // Shadowed variables for a block will have the same type as the block,
    // but the block name cannot be used to declare the shadow and the member
    // names are likely not reusable.  So, use this to track such types
    // so that at the right points, legal GLSL can be produced.
    // Note: these only matter for non-logical IO mode
    std::set<const llvm::Type*> shadowingBlock;

    // map from llvm type to the mdAggregate nodes that describe their types
    std::map<const llvm::Type*, const llvm::MDNode*> typeMdAggregateMap;

    // set to track what globals are already declared;
    // it potentially only includes globals that are in danger of multiple declaration
    std::set<std::string> globallyDeclared;

    std::ostringstream globalStructures;
    std::ostringstream globalDeclarations;
    std::ostringstream globalInitializers;
    std::ostringstream fullShader;
    bool appendInitializers;
    std::ostringstream shader;
    int indentLevel;
    int lastVariable;
    int version;
    EProfile profile;
    EShLanguage stage;
    bool usingSso;
    const char* indentString;
    friend class Assignment;
};

//
// To build up and hold an assignment until the entire statement is known, so it
// can be emitted all at once, and the left-hand side can be given a name then
// based on the entire statement.  Or, the entire right-hand side could just be 
// mapped as a forward substitution.
//
class Assignment : public std::ostringstream {
public:
    Assignment(gla::GlslTarget* target, const llvm::Instruction* instruction) : target(*target), instruction(instruction), lvalue(true) { }

    void emit()
    {
        if (member.size() > 0) {
            // emit a declaration, no assignment
            if (target.valueMap.find(instruction) == target.valueMap.end()) {
                target.newLine();
                target.emitGlaValue(target.shader, instruction, str().c_str());
                target.shader << ";";
            }
        }
        if (! lvalue && str().size() == 0)
            return;

        // emit the l-value
        target.newLine();
        if (lvalue)
            target.emitGlaValue(target.shader, instruction, str().c_str());
        if (member.size() > 0)
            target.shader << member;
        if (lvalue && str().size() > 0)
            target.shader << " = ";

        // emit the expression
        target.shader << str() << ";";
    }

    bool cantMap()
    {
        return member.size() > 0 || ! lvalue;
    }

    void map(bool needsParens)
    {
        if (cantMap()) {
            emit();
            return;
        }

        std::string parenthesized;
        if (needsParens)
            parenthesized.append("(");
        parenthesized.append(str());
        if (needsParens)
            parenthesized.append(")");
        target.mapExpressionString(instruction, parenthesized);
    }

    // TODO: generated-code performance: pass in false here in more places where
    // it can be known there is not an expansion of data.  E.g., sum of two
    // variables that die.
    // increasesData: means it is definitely known there is more data made, so always better to forward substitute (map)
    void mapOrEmit(bool increasesData, bool needsParens)
    {
        if (target.getSubstitutionLevel() == 0 || cantMap()) {
            emit();
            return;
        }

        bool doSubstitute = true;

        if (! target.cheapExpression(str())) {

            if (target.getSubstitutionLevel() < 2)
                doSubstitute = false;

            if (doSubstitute && str().size() > 120)
                doSubstitute = false;

            if (doSubstitute && target.modifiesPrecision(instruction))
                doSubstitute = false;

            if (doSubstitute && target.valueMap.find(instruction) != target.valueMap.end())
                doSubstitute = false;

            if (doSubstitute && ! increasesData && ! target.shouldSubstitute(instruction))
                doSubstitute = false;
        }

        if (doSubstitute)
            map(needsParens);
        else
            emit();
    }

    void setMember(const char* m) { member = m; }
    void setNoLvalue() { lvalue = false; }

protected:
    const llvm::Instruction* instruction;
    gla::GlslTarget& target;
    std::string member;
    bool lvalue;
};

//
// Factory for GLSL back-end translator
//

gla::GlslTranslator* gla::GetGlslTranslator(Manager* manager, bool obfuscate, bool filterInactive, int substitutionLevel)
{
    return new gla::GlslTarget(manager, obfuscate, filterInactive, substitutionLevel);
}

void gla::ReleaseGlslTranslator(gla::BackEndTranslator* target)
{
    delete target;
}

//
// File-local helpers
//

namespace {

using namespace gla;

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
    for (int i = 0; i < (int)elts.size(); ++i) {
        if (IsUndef(elts[i]) || i != GetConstantInt(elts[i])) {
            return false;
        }
    }

    return true;
}

// Create the start of a scalar/vector conversion, but not for matrices.
void ConversionStart(std::ostringstream& out, llvm::Type* type, bool toIO)
{
    if (! IsInteger(type))
        return;

    if (GetComponentCount(type) == 1)
        out << (toIO ? "uint(" : "int(");
    else {
        out << (toIO ? "uvec" : "ivec");
        switch (GetComponentCount(type)) {
        case 2:  out << "2(";    break;
        case 3:  out << "3(";    break;
        case 4:  out << "4(";    break;
        default: UnsupportedFunctionality("conversion wrapper");  break;
        }
    }
}

void ConversionStop(std::ostringstream& out, llvm::Type* type)
{
    if (! IsInteger(type))
        return;

    out << ")";
}

// Either convert to (from) an unsigned int or to (from) a matrix
// toIO means converting from an internal variable to an I/O variable, if false, it's the other direction
// integer means doing unsigned/signed conversion, if false, then doing matrix/array conversion
void ConversionWrap(std::string& name, llvm::Type* type, bool toIO)
{
    std::ostringstream wrapped;

    if (IsInteger(type)) {
        ConversionStart(wrapped, type, toIO);
        wrapped << name;
        ConversionStop(wrapped, type);
    } else if (CouldBeMatrix(type)) {
        llvm::ArrayType* array = llvm::dyn_cast<llvm::ArrayType>(type);
        llvm::VectorType* vector = llvm::dyn_cast<llvm::VectorType>(array->getContainedType(0));
        if (toIO)
            wrapped << "mat" << array->getNumElements() << "x" << vector->getNumElements();
        else
            wrapped << "vec" << vector->getNumElements() << "[" << array->getNumElements() << "]";
        wrapped << "(";
        if (toIO)
            wrapped << name;
        else {
            for (int e = 0; e < array->getNumElements(); ++e) {
                if (e > 0)
                    wrapped << ", ";
                wrapped << name << "[" << e << "]";
            }
        }
        wrapped << ")";
    } else
        return;

    name = wrapped.str();
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
const char* MapGlaXor(const llvm::Instruction* llvmInstruction, int& unaryOperand)
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

EVariableQualifier MapGlaAddressSpace(const llvm::Value* value)
{
    if (const llvm::PointerType* pointer = llvm::dyn_cast<llvm::PointerType>(value->getType())) {
        switch (pointer->getAddressSpace()) {
        case ResourceAddressSpace:
            return EVQUniform;
        case GlobalAddressSpace:
            return EVQGlobal;
        default:
            if (pointer->getAddressSpace() >= ConstantAddressSpaceBase)
                return EVQUniform;

            UnsupportedFunctionality("Address Space in Bottom IR: ", pointer->getAddressSpace());
            break;
        }
    }

    if (llvm::isa<llvm::Instruction>(value))
        return EVQTemporary;

    // Check for an undef before a constant (since Undef is a
    // subclass of Constant)
    if (AreAllUndefined(value))
        return EVQUndef;

    if (llvm::isa<llvm::Constant>(value))
        return EVQConstant;

    return EVQTemporary;
}

const char* MapGlaToQualifierString(int version, EShLanguage stage, EVariableQualifier vq)
{
    switch (vq) {
    case EVQUniform:                      return "uniform";

    case EVQInput:
        if (version >= 130)               return "in";
        else if (stage == EShLangVertex)  return "attribute";
        else                              return "varying";

    case EVQOutput:
        if (version >= 130)               return "out";
        else                              return "varying";

    case EVQConstant:                     return "const";

    case EVQNone:
    case EVQGlobal:
    case EVQTemporary:
    case EVQUndef:                        return "";

    default:
        UnsupportedFunctionality("Unknown EVariableQualifier", EATContinue);
        return "";
    }
}

const char* MapGlaToPrecisionString(EMdPrecision precision)
{
    switch (precision) {
    case EMpLow:      return "lowp";
    case EMpMedium:   return "mediump";
    case EMpHigh:     return "highp";
    default:          return "";
    }
}

const char* MapComponentToSwizzleChar(int component)
{
    switch (component) {
    case 0:   return "x";
    case 1:   return "y";
    case 2:   return "z";
    case 3:   return "w";
    default:  assert(! "Vector too large"); break;
    }

    return "x";
}

std::string MapGlaStructField(const llvm::Type* structType, int index, const llvm::MDNode* mdAggregate = 0)
{
    std::string name;

    if (mdAggregate) {
        int aggOp = GetAggregateMdNameOp(index);
        if ((int)mdAggregate->getNumOperands() > aggOp) {
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

// Write the string representation of an operator. If it's an xor of some
// register and true, then unaryOperand will be set to the index for the
// non-true operand, and s will contain "!".
void GetOp(const llvm::Instruction* llvmInstruction, bool allowBitwise, std::string& s, int& unaryOperand, bool& nested, bool& emulateBitwise, bool& unsignedOp)
{
    nested = false;
    emulateBitwise = false;

    // first, just see if we need to force an unsigned operation
    switch (llvmInstruction->getOpcode()) {
    case llvm::Instruction::UDiv:
    case llvm::Instruction::URem:
    case llvm::Instruction::LShr:
        unsignedOp = true;
        break;
    case llvm::Instruction::ICmp:
        if (const llvm::CmpInst* cmp = llvm::dyn_cast<llvm::CmpInst>(llvmInstruction)) {
            switch (cmp->getPredicate()) {
            case llvm::ICmpInst::ICMP_UGT:
            case llvm::ICmpInst::ICMP_UGE:
            case llvm::ICmpInst::ICMP_ULT:
            case llvm::ICmpInst::ICMP_ULE:
                unsignedOp = true;
                break;
            default:
                unsignedOp = false;
                break;
            }
        } else
            assert(! "Cmp instruction found that cannot dyncast to CmpInst");
        break;

    default:
        unsignedOp = false;
        break;
    }

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
        if (allowBitwise)
            s = "<<";
        else {
            emulateBitwise = true;
            s = "*";
        }
        break;
    case llvm::Instruction::LShr:
    case llvm::Instruction::AShr:
        if (allowBitwise)
            s = ">>";
        else {
            emulateBitwise = true;
            s = "/";
        }
        break;
    case llvm::Instruction::And:
        if (IsBoolean(llvmInstruction->getOperand(0)->getType())) {
            s = "&&";
        } else {
            if (allowBitwise)
                s = "&";
            else {
                emulateBitwise = true;
                s = "-";
            }
        }
        break;
    case llvm::Instruction::Or:
        if (IsBoolean(llvmInstruction->getOperand(0)->getType())) {
            s = "||";
        } else {
            if (allowBitwise)
                s = "|";
            else {
                emulateBitwise = true;
                s = "+";
            }
        }
        break;
    case llvm::Instruction::Xor:
        s = MapGlaXor(llvmInstruction, unaryOperand);
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
                    break;
                }
            } else
                assert(! "Cmp instruction found that cannot dyncast to CmpInst");
        }
        break;

    case llvm::Instruction::FPToUI:
        switch (GetComponentCount(llvmInstruction)) {
        case 1: s = "int(uint";  break;
        case 2: s = "ivec2(uvec2"; break;
        case 3: s = "ivec3(uvec3"; break;
        case 4: s = "ivec4(uvec4"; break;
        default: UnsupportedFunctionality("Can only convert scalars and vectors"); break;
        }
        unaryOperand = 0;
        nested = true;
        break;
    case llvm::Instruction::ZExt:
    case llvm::Instruction::FPToSI:
        switch (GetComponentCount(llvmInstruction)) {
        case 1: s = "int";   break;
        case 2: s = "ivec2"; break;
        case 3: s = "ivec3"; break;
        case 4: s = "ivec4"; break;
        default: UnsupportedFunctionality("Can only convert scalars and vectors"); break;
        }
        unaryOperand = 0;
        break;
    case llvm::Instruction::UIToFP:
    case llvm::Instruction::SIToFP:
        switch (GetComponentCount(llvmInstruction)) {
        case 1: s = "float"; break;
        case 2: s = "vec2";  break;
        case 3: s = "vec3";  break;
        case 4: s = "vec4";  break;
        default: UnsupportedFunctionality("Can only convert scalars and vectors"); break;
        }
        unaryOperand = 0;
        break;

    default:
        break;
    }
}

void InvertOp(std::string& op)
{
    if (op == "==")
        op = "!=";
    else if (op == "!=")
        op = "==";
    else if (op == ">")
        op = "<=";
    else if (op == "<=")
        op = ">";
    else if (op == "<")
        op = ">=";
    else if (op == ">=")
        op = "<";
    else
        gla::UnsupportedFunctionality("unknown op to invert", EATContinue);
}

EMdPrecision GetPrecision(const llvm::Value* value)
{
    EMdPrecision precision = EMpNone;

    if (const llvm::Instruction* instr = llvm::dyn_cast<const llvm::Instruction>(value))
        CrackPrecisionMd(instr, precision);

    return precision;
}

bool NeedsShadowRefZArg(const llvm::IntrinsicInst* llvmInstruction)
{
    // Check flags for RefZ
    int texFlags = GetConstantInt(llvmInstruction->getOperand(GetTextureOpIndex(ETOFlag)));

    return (texFlags & ETFRefZArg) != 0;
}

bool NeedsLodArg(const llvm::IntrinsicInst* llvmInstruction)
{
    // Check flags for bias/lod
    int texFlags = GetConstantInt(llvmInstruction->getOperand(GetTextureOpIndex(ETOFlag)));

    return (texFlags & ETFLod) != 0;
}

bool NeedsBiasArg(const llvm::IntrinsicInst* llvmInstruction)
{
    // Check flags for bias/lod
    int texFlags = GetConstantInt(llvmInstruction->getOperand(GetTextureOpIndex(ETOFlag)));

    return (texFlags & ETFBiasLodArg) != 0 && (texFlags & ETFLod) == 0;
}

bool NeedsOffsetArg(const llvm::IntrinsicInst* llvmInstruction, bool& offsets)
{
    // Check flags for offset arg
    int texFlags = GetConstantInt(llvmInstruction->getOperand(GetTextureOpIndex(ETOFlag)));

    offsets = ((texFlags & ETFOffsets) != 0);
    return (texFlags & ETFOffsetArg) != 0;
}

bool NeedsComponentArg(const llvm::IntrinsicInst* llvmInstruction)
{
    // Check flags for component arg
    int texFlags = GetConstantInt(llvmInstruction->getOperand(GetTextureOpIndex(ETOFlag)));

    return (texFlags & ETFComponentArg) != 0;
}

void MakeParseable(std::string& name)
{
    // LLVM uses "." for phi'd symbols, change to _ so it's parseable by GLSL
    // Also, glslang uses @ for an internal name.
    // If the name changes, add a "__goo" so that it's not coincidentally a user name.

    bool changed = false;
    // bool hasDoubleUnderscore = false; // use this once "__" is accepted everywhere
    for (int c = 0; c < (int)name.length(); ++c) {
        if (name[c] == '.' || name[c] == '-' || name[c] == '@') {
            name[c] = '_';
            changed = true;
        }

        if (c > 0 && name[c-1] == '_' && name[c] == '_') {
            // TODO: cleanliness: want to only say:  hasDoubleUnderscore = true;
            // but, for now change things because not all compilers accept "__".
            // Use "_" when possible, because it is much more readable.
            name[c] = 'u';
        }
    }

    // use this once "__" is accepted everywhere
    //if (changed && ! hasDoubleUnderscore)
    //    name.append("_goo");
}

void MakeNonbuiltinName(std::string& name)
{
    // TODO: cleanliness: switch to using "__" when all compilers accept it.

    if (name.compare(0, 3, "gl_") == 0)
        name[0] = 'L';
}

// Whether the given intrinsic's specified operand is the same as the passed
// value, and its type is a vector.
bool IsSameSource(llvm::Value *source, const llvm::IntrinsicInst *inst, int operand)
{
    return (inst->getOperand(operand) == source)
        && (source->getType()->getTypeID() == llvm::Type::VectorTyID);
}

// Returns a pointer to the common source of the multiinsert if they're all
// the same, otherwise returns null.
llvm::Value* GetCommonSourceMultiInsert(const llvm::IntrinsicInst* inst)
{
    llvm::Value* source = NULL;
    bool sameSource = true;
    int wmask = GetConstantInt(inst->getOperand(1));

    for (int i = 0; i < 4; ++i) {
        if (wmask & (1 << i)) {
            int operandIndex = (i+1) * 2;
            if (source)
                sameSource = sameSource && IsSameSource(source, inst, operandIndex);
            else
                source = inst->getOperand(operandIndex);
        }
    }

    return sameSource ? source : NULL;
}

//
// Figure out how many I/O slots 'type' would fill up.
//
int CountSlots(const llvm::Type* type)
{
    if (type->getTypeID() == llvm::Type::VectorTyID)
        return 1;
    else if (type->getTypeID() == llvm::Type::ArrayTyID) {
        const llvm::ArrayType* arrayType = llvm::dyn_cast<const llvm::ArrayType>(type);
        return (int)arrayType->getNumElements() * CountSlots(arrayType->getContainedType(0));
    } else if (type->getTypeID() == llvm::Type::StructTyID) {
        const llvm::StructType* structType = llvm::dyn_cast<const llvm::StructType>(type);
        int slots = 0;
        for (unsigned int f = 0; f < structType->getStructNumElements(); ++f)
            slots += CountSlots(structType->getContainedType(f));

        return slots;
    }

    return 1;
}

//
// *Textually* dereference a name string down to a single I/O slot.
//
void DereferenceName(std::string& name, const llvm::Type* type, const llvm::MDNode* mdAggregate, int slotOffset, EMdTypeLayout& mdTypeLayout)
{
    // Operates recursively...

    if (type->getTypeID() == llvm::Type::PointerTyID) {
        type = type->getContainedType(0);

        DereferenceName(name, type, mdAggregate, slotOffset, mdTypeLayout);
    } else if (type->getTypeID() == llvm::Type::StructTyID) {
        int field = 0;
        int operand;
        const llvm::StructType* structType = llvm::dyn_cast<const llvm::StructType>(type);
        const llvm::Type* fieldType;
        do {
            operand = GetAggregateMdSubAggregateOp(field);
            if (operand >= (int)mdAggregate->getNumOperands()) {
                assert(operand < (int)mdAggregate->getNumOperands());
                return;
            }
            fieldType = structType->getContainedType(field);
            int fieldSize = CountSlots(fieldType);
            if (fieldSize > slotOffset)
                break;
            slotOffset -= fieldSize;
            ++field;
        } while (true);
        if (name.size() > 0)
            name = name + ".";
        name = name + std::string(mdAggregate->getOperand(GetAggregateMdNameOp(field))->getName());
        const llvm::MDNode* subMdAggregate = llvm::dyn_cast<const llvm::MDNode>(mdAggregate->getOperand(operand));
        DereferenceName(name, fieldType, subMdAggregate, slotOffset, mdTypeLayout);
    } else if (type->getTypeID() == llvm::Type::ArrayTyID) {
        const llvm::ArrayType* arrayType = llvm::dyn_cast<const llvm::ArrayType>(type);
        int elementSize = CountSlots(arrayType->getContainedType(0));
        int element = slotOffset / elementSize;
        slotOffset = slotOffset % elementSize;

        char buf[11];
        snprintf(buf, sizeof(buf), "%d", element);
        name = name + "[" + buf + "]";
        
        DereferenceName(name, arrayType->getContainedType(0), mdAggregate, slotOffset, mdTypeLayout);
    } else if (mdAggregate)
        mdTypeLayout = GetMdTypeLayout(mdAggregate);
}

void StripSuffix(std::string& name, const char* suffix)
{
    int newSize = name.length() - strlen(suffix);
    if (newSize < 0)
        return;

    if (name.compare(newSize, strlen(suffix), suffix) == 0)
        name.resize(newSize);
}

}; // end anonymous namespace

//
// Implement the GLSL Target
//

// For regular globals:  map their LLVM Value* to their GLSL name.
void gla::GlslTarget::addGlobal(const llvm::GlobalVariable* global)
{
    // IO should already be mapped, and this will filter it out.
    // (Uniforms were declared in addIoDeclaration().)
    std::string name = global->getName();
    if (globallyDeclared.find(name) != globallyDeclared.end())
        return;

    EVariableQualifier qualifier = MapGlaAddressSpace(global);

    mapVariableName(global, name);
    const llvm::PointerType* pointer = llvm::dyn_cast<llvm::PointerType>(global->getType());
    llvm::Type* type = pointer->getContainedType(0);

    emitVariableDeclaration(EMpNone, type, name, qualifier);
    if (global->hasInitializer()) {
        const llvm::Constant* constant = global->getInitializer();
        emitInitializeAggregate(globalInitializers, name, constant);
    }
}

// Map global "const xxxxx = { ..... }; \n"
void gla::GlslTarget::addGlobalConst(const llvm::GlobalVariable* global)
{
    const llvm::PointerType* pointer = llvm::dyn_cast<llvm::PointerType>(global->getType());
    llvm::Type* type = pointer->getContainedType(0);

    // Better not declare a "const" unless we really have an initializer
    const llvm::Constant* constant = global->getInitializer();
    if (constant == 0 || ! AreAllDefined(constant)) {
        std::string name = global->getName();
        mapVariableName(global, name);
        emitVariableDeclaration(EMpNone, type, name, EVQGlobal);
        emitInitializeAggregate(globalInitializers, name, constant);
    } else
        emitNamelessConstDeclaration(global, constant);
}

void gla::GlslTarget::addIoDeclaration(gla::EVariableQualifier qualifier, const llvm::MDNode* mdNode)
{
    std::string instanceName = mdNode->getOperand(0)->getName();

    if (filteringIoNode(mdNode))
        return;

    bool declarationAllowed = true;

    llvm::Type* type = 0;
    CrackIOMdType(mdNode, type);
    if (type->getTypeID() == llvm::Type::PointerTyID)
        type = type->getContainedType(0);

    std::string mappingName;

    // Names starting "gl_" are
    //  - an error to declare explicitly, as they are built-in, or
    //  - okay to declare (e.g., gl_ClipDistance) if something is added, like array size ('invariant' is handled elsewhere), or
    //  - required to be declared, if the shader declared them, to make SSO work
    // Basically, the rules are ad hoc, and so listed here.
    if (instanceName.size() == 0) {
        // This is an anonymous block, we'll want to decide name-based skipping based
        // on the name of the block, not the name of the instance.

        if (type->getTypeID() == llvm::Type::StructTyID) {
            const llvm::StructType* structType = llvm::dyn_cast<const llvm::StructType>(type);

            llvm::StringRef blockName = structType->isLiteral() ? "" : structType->getName();

            if (blockName.substr(0,3) == std::string("gl_") && ! usingSso)
                declarationAllowed = false;
        }
        // Strip off the "_typeProxy" to get the shader name:
        // TODO: formalize a better way of getting this; this results from a design change.
        mappingName = mdNode->getOperand(2)->getName();
        StripSuffix(mappingName, "_typeProxy");
    } else {
        if (instanceName.substr(0,3) == std::string("gl_")) {
            declarationAllowed = false;
            if (instanceName == "gl_ClipDistance" || instanceName == "gl_TexCoord")
                declarationAllowed = true;
            else if (usingSso && (instanceName == "gl_in" ||
                                  instanceName == "gl_out"))
                declarationAllowed = true;
        }
        mappingName = instanceName;
    }

    mdMap[mappingName] = mdNode;
    // This will prevent the IO globals from being declared again later.
    globallyDeclared.insert(mappingName);
    if (canonMap.find(mappingName) == canonMap.end())
        canonMap[mappingName] = 0;

    if (! declarationAllowed) {
        if ((type->getTypeID() == llvm::Type::StructTyID || type->getTypeID() == llvm::Type::ArrayTyID) && mdNode->getNumOperands() >= 5) {
            // We don't want to spit out a declaration (below), but we do want to establish the mapping
            // between LLVM's type and the mdAggregate.
            // Arrays will be dereferenced before lookup, so dereference the key now too.
            const llvm::Type* aggType = type;
            while (aggType->getTypeID() == llvm::Type::ArrayTyID)
                aggType = aggType->getContainedType(0);
            typeMdAggregateMap[aggType] = llvm::dyn_cast<llvm::MDNode>(mdNode->getOperand(4));
        }

        return;
    }

    int arraySize = emitGlaType(globalDeclarations, EMpCount, qualifier, 0, true, mdNode);
    globalDeclarations << " " << std::string(instanceName);
    emitGlaArraySize(globalDeclarations, arraySize);
    globalDeclarations << ";" << std::endl;
}

void gla::GlslTarget::startFunctionDeclaration(const llvm::Type* type, llvm::StringRef name)
{
    newLine();
    int arraySize = emitGlaType(shader, EMpNone, EVQNone, type->getContainedType(0));        
    emitGlaArraySize(shader, arraySize);
    // TODO: ES functionality: how do we know the precision or unsignedness of a function declaration?
    shader << " " << name.str() << "(";

    if (name == std::string("main"))
        appendInitializers = true;
}

void gla::GlslTarget::addArgument(const llvm::Value* value, bool last)
{
    emitGlaOperand(shader, value);
    if (! last)
        shader << ", ";
}

void gla::GlslTarget::endFunctionDeclaration()
{
    shader << ")";
}

void gla::GlslTarget::startFunctionBody()
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

void gla::GlslTarget::endFunctionBody()
{
    leaveScope();
    newLine();
}

void gla::GlslTarget::addInstruction(const llvm::Instruction* llvmInstruction, bool lastBlock, bool referencedOutsideScope)
{
    std::string charOp;
    int unaryOperand = -1;

    // TODO: loops: This loop will disappear when conditional loops in BottomToGLSL properly updates valueMap
    for (llvm::Instruction::const_op_iterator i = llvmInstruction->op_begin(), e = llvmInstruction->op_end(); i != e; ++i) {
        llvm::Instruction* inst = llvm::dyn_cast<llvm::Instruction>(*i);
        if (inst) {
            if (valueMap.find(*i) == valueMap.end())
                addInstruction(inst, lastBlock);
        }
    }

    // If the instruction is referenced outside of the current scope
    // (e.g. inside a loop body), then add a (global) declaration for it.
    if (referencedOutsideScope)
        emitGlaValueDeclaration(llvmInstruction, 0, referencedOutsideScope);

    bool nested;
    bool emulateBitwise;
    bool unsignedOp;
    GetOp(llvmInstruction, version >= 130, charOp, unaryOperand, nested, emulateBitwise, unsignedOp);

    Assignment assignment(this, llvmInstruction);

    // Handle the binary ops
    if (! charOp.empty() && unaryOperand == -1) {
        bool swapOperands = needCanonicalSwap(llvmInstruction);


        llvm::Value* operand = llvmInstruction->getOperand(swapOperands ? 1 : 0);

        // reverse conversion for the whole expression
        if (unsignedOp)
            ConversionStart(assignment, llvmInstruction->getType(), false);

        // forward conversion for the operand
        if (unsignedOp)
            ConversionStart(assignment, operand->getType(), true);
        emitGlaOperand(assignment, operand);
        if (unsignedOp) 
            ConversionStop(assignment, operand->getType());

        // special case << to multiply, etc., for early versions
        int multiplier = -1;
        if (emulateBitwise) {
            switch (llvmInstruction->getOpcode()) {
            case llvm::Instruction::Shl:
            case llvm::Instruction::AShr:
            case llvm::Instruction::LShr:
            {
                int shift = GetConstantInt(llvmInstruction->getOperand(1));
                if (shift == 0)
                    UnsupportedFunctionality("shift for version ", version, EATContinue);
                multiplier = 1;
                for (int i = 0; i < shift; ++i)
                    multiplier *= 2;

                break;
            } 
            case llvm::Instruction::Or:
                UnsupportedFunctionality("bit-wise OR in version ", version, EATContinue);
                break;
            case llvm::Instruction::And:
                UnsupportedFunctionality("bit-wise AND in version ", version, EATContinue);
                break;
            default:
                break;
            }
        }
        
        assignment << " " << charOp << " ";
        if (multiplier == -1) {
            llvm::Value* operand = llvmInstruction->getOperand(swapOperands ? 0 : 1);
            if (unsignedOp)
                ConversionStart(assignment, operand->getType(), true);
            emitGlaOperand(assignment, operand);
            if (unsignedOp) 
                ConversionStop(assignment, operand->getType());
        } else
            assignment << multiplier;

        // reverse conversion
        if (unsignedOp) 
            ConversionStop(assignment, llvmInstruction->getType());

        assignment.mapOrEmit(false, true);

        return;
    }

    // Handle the unary ops
    if (! charOp.empty()) {
        assignment << charOp << "(";
        emitGlaOperand(assignment, llvmInstruction->getOperand(unaryOperand));
        assignment << ")";
        if (nested)
            assignment << ")";

        assignment.mapOrEmit(true, ! nested);

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
            emitGlaOperand(shader, llvmInstruction->getOperand(0));
            shader << ";";
        }
        
        return;

    case llvm::Instruction::Call: // includes intrinsics...
        // Sometimes instrinsics seem to invalidly cast to an intrinsic that has
        // no valid intrinsic ID.  This seems like a bug somewhere, but protect
        // against it here.
        if (const llvm::IntrinsicInst* intrinsic = llvm::dyn_cast<llvm::IntrinsicInst>(llvmInstruction)) {
            if (intrinsic->getIntrinsicID() == llvm::Intrinsic::not_intrinsic)
                UnsupportedFunctionality("intrinsic without valid intrinsic ID", EATContinue);
            else
                emitGlaIntrinsic(shader, intrinsic);
        } else {
            const llvm::CallInst* call = llvm::dyn_cast<llvm::CallInst>(llvmInstruction);
            assert(call);
            emitGlaCall(shader, call);
        }

        return;

    case llvm::Instruction::FRem:
        assignment << "mod(";
        emitGlaOperand(assignment, llvmInstruction->getOperand(0));
        assignment << ", ";
        emitGlaOperand(assignment, llvmInstruction->getOperand(1));
        assignment << ")";
        assignment.emit();

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
                break;
            }
        } else {
            assert(! "Cmp vector instruction found that cannot dyncast to CmpInst");
        }

        assignment << charOp << "(";
        emitGlaOperand(assignment, llvmInstruction->getOperand(0));
        assignment << ", ";
        emitGlaOperand(assignment, llvmInstruction->getOperand(1));
        assignment << ")";

        assignment.mapOrEmit(false, false);

        return;
    }

    case llvm::Instruction::GetElementPtr:
    {
        // Make a single GLSL string for the whole expression represented by the GEP, and map it
        // to this instruction.  This includes gep-chain traversal, but not conversion wrapping.

        mapPointerExpression(llvmInstruction);

        return;
    }
    case llvm::Instruction::Load:
    {
        // We want phis to use the same variable name created during phi declaration
        if (llvm::isa<llvm::PHINode>(llvmInstruction->getOperand(0))) {
            mapExpressionString(llvmInstruction, *valueMap[llvmInstruction->getOperand(0)]);

            return;
        }

        // Lookup whether this load is using the results of a GEP instruction (or constantExpr GEP), 
        // and just pick up the previously computed result.
        std::string expression;
        if (! getExpressionString(llvmInstruction->getOperand(0), expression)) {
            // This might be an embedded GEP, so never seen before, or it could be a simple pointer.
            mapPointerExpression(llvmInstruction->getOperand(0), llvmInstruction);
        } else {
            // If the already existing mapping existed, propogate that
            // association to this load as well, including non-conversion view.
            mapExpressionString(llvmInstruction, expression);
            propagateNonconvertedGlaValue(llvmInstruction, llvmInstruction->getOperand(0));
        }

        return;
    }
    case llvm::Instruction::Store:
    {
        const llvm::Value* target = llvmInstruction->getOperand(1);
        assert(llvm::isa<llvm::PointerType>(target->getType()));

        const llvm::GetElementPtrInst* gepInstr = getGepAsInst(target);
        if (gepInstr)
            target = gepInstr;
        std::string dummyExpression;  // The expression could be conversion wrapped, so we won't actually use it
        if (! getExpressionString(target, dummyExpression)) {
            // it could be an embedded GEP
            mapPointerExpression(target);
        }

        std::ostringstream expression;
        std::map<const llvm::Value*, std::string*>::const_iterator it = nonConvertedMap.find(target);
        if (it != nonConvertedMap.end())
            ConversionStart(expression, target->getType()->getContainedType(0), true);
        emitGlaOperand(expression, llvmInstruction->getOperand(0));
        if (it != nonConvertedMap.end())
            ConversionStop(expression, target->getType()->getContainedType(0));

        if (writeOnceAlloca(target))
            mapExpressionString(target, expression.str());
        else {
            newLine();
            // If uint/matrix IO conversions are needed, they actually have to have the
            // opposite conversion applied to the rhs.
            if (it != nonConvertedMap.end())
                shader << it->second->c_str();
            else
                emitGlaValue(shader, target, 0);

            shader << " = " << expression.str() << ";";
        }

        return;
    }

    case llvm::Instruction::Alloca:
        newLine();
        emitGlaValue(shader, llvmInstruction, 0);
        shader << ";";

        return;

    case llvm::Instruction::ExtractElement:
    {
        emitGlaValueDeclaration(llvmInstruction->getOperand(0), 0);

        // copy propagate, by name string, the extracted component
        std::string swizzled;
        makeExtractElementStr(llvmInstruction, swizzled);
        mapExpressionString(llvmInstruction, swizzled);

        return;
    }

    case llvm::Instruction::InsertElement:
    {
        bool sourceHasOtherUse = true; // TODO run-time optimization

        if (sourceHasOtherUse) {
            // first, copy the whole vector "inserted into" to the resulting "value" of the insert
            Assignment copy(this, llvmInstruction);
            emitGlaOperand(copy, llvmInstruction->getOperand(0));
            copy.emit();

            // second, overwrite the element being inserted
            std::ostringstream member;
            llvm::Value* element = llvmInstruction->getOperand(2);
            if (llvm::isa<llvm::Constant>(element)) {
                member << ".";
                emitComponentToSwizzle(member, GetConstantInt(element));
            } else {
                member << "[";
                emitGlaOperand(member, element);
                member << "]";
            }
            assignment.setMember(member.str().c_str());
            emitGlaOperand(assignment, llvmInstruction->getOperand(1));
            assignment.emit();
        } else {
            newLine();
            if (valueMap.find(llvmInstruction->getOperand(0)) == valueMap.end()) {
                emitGlaValueDeclaration(llvmInstruction->getOperand(0), 0);
                shader << ";";
                newLine();
            }
            emitNonconvertedGlaValue(shader, llvmInstruction->getOperand(0));

            llvm::Value* element = llvmInstruction->getOperand(2);
            if (llvm::isa<llvm::Constant>(element)) {
                shader << ".";
                emitComponentToSwizzle(shader, GetConstantInt(element));
            } else {
                shader << "[";
                emitGlaOperand(shader, element);
                shader << "]";
            }

            shader << " = ";
            emitGlaOperand(shader, llvmInstruction->getOperand(1));
            shader << ";";

            mapExpressionString(llvmInstruction, *valueMap[llvmInstruction->getOperand(0)]);
        }

        return;
    }
    case llvm::Instruction::Select:
    {
        // Using ?: is okay for single component, but need to use 
        // mix(false-vector, true-vector, condition-vector) for non-scalars.

        bool needsParens;
        const llvm::SelectInst* si = llvm::dyn_cast<const llvm::SelectInst>(llvmInstruction);
        assert(si);
        if (GetComponentCount(si->getCondition()) == 1) {
            emitGlaOperand(assignment, si->getCondition());
            assignment << " ? ";
            emitGlaOperand(assignment, si->getTrueValue());
            assignment << " : ";
            emitGlaOperand(assignment, si->getFalseValue());
            needsParens = true;
        } else {
            assignment << "mix(";
            emitGlaOperand(assignment, si->getFalseValue());
            assignment << ",";
            emitGlaOperand(assignment, si->getTrueValue());
            assignment << ",";
            emitGlaOperand(assignment, si->getCondition());
            assignment << ")";
            needsParens = false;
        }

        assignment.mapOrEmit(false, needsParens);

        return;
    }
    case llvm::Instruction::ExtractValue:
    {
        // emit base
        const llvm::ExtractValueInst* extractValueInst = llvm::dyn_cast<const llvm::ExtractValueInst>(llvmInstruction);
        // Optimization: since we are dereferencing, if the base was a converted matrix,
        // we can start with the non-converted form, knowing we are deferencing it
        emitNonconvertedGlaValue(assignment, extractValueInst->getAggregateOperand());

        // emit chain
        assignment << traverseGep(extractValueInst);

        assignment.mapOrEmit(true, false);

        return;
    }
    case llvm::Instruction::InsertValue:
    {
        newLine();

        //emit base
        const llvm::InsertValueInst* insertValueInst = llvm::dyn_cast<const llvm::InsertValueInst>(llvmInstruction);
        emitNonconvertedGlaValue(shader, insertValueInst->getAggregateOperand());

        // emit chain
        shader << traverseGep(insertValueInst);

        shader << " = ";
        emitGlaOperand(shader, insertValueInst->getInsertedValueOperand());
        shader << ";";

        // propagate aggregate name
        // TODO: generated code correctness: probably not safe, if the partial struct has another use elsewhere
        mapExpressionString(llvmInstruction, *valueMap[llvmInstruction->getOperand(0)]);

        return;
    }
    case llvm::Instruction::ShuffleVector:
    {
        emitGlaConstructor(assignment, llvmInstruction->getType());
        assignment << "(";

        int sourceWidth = gla::GetComponentCount(llvmInstruction->getOperand(0));
        int resultWidth = gla::GetComponentCount(llvmInstruction);

        const llvm::Constant* mask = llvm::dyn_cast<const llvm::Constant>(llvmInstruction->getOperand(2));
        assert(llvm::isa<llvm::ConstantVector>(mask) || llvm::isa<llvm::ConstantAggregateZero>(mask));

        llvm::SmallVector<llvm::Constant*,4> elts;
        gla_llvm::GetElements(mask, elts);

        for (int i = 0; i < resultWidth; ++i) {
            if (i != 0)
                assignment << ", ";

            // If we're undef, then use ourselves
            if (! IsDefined(elts[i])) {
                emitGlaOperand(assignment, llvmInstruction);
                assignment << ".";
                emitComponentToSwizzle(assignment, i);
                continue;
            }

            int comp = GetConstantInt(elts[i]);
            bool useSecond = comp > sourceWidth-1;

            if (useSecond)
                comp -= sourceWidth;

            emitGlaOperand(assignment, llvmInstruction->getOperand(useSecond ? 1 : 0));
            assignment << ".";
            emitComponentToSwizzle(assignment, comp);
        }
        assignment << ")";

        assignment.mapOrEmit(true, false);

        return;
    }

    case llvm::Instruction::BitCast:
        // This is getting added by LLVM to change a pointer's type; something like this:
        //    %0 = bitcast [13 x <3 x float>]* %indexable14.i to i8*
        //    call void @llvm.lifetime.start(i64 -1, i8* %0)
        // where the lifetime intrinsic is the only use of %0.  For these uses,
        // there is nothing to translate, as 'lifetime' is also ignored.
        if (llvmInstruction->getOperand(0)->getType()->getTypeID() != llvm::Type::PointerTyID)
            UnsupportedFunctionality("BitCast applied to a non-pointer", EATContinue);
        return;

    default:
        UnsupportedFunctionality("Opcode in Bottom IR: ", llvmInstruction->getOpcode(), EATContinue);
        break;
    }
}

// See if this is an instruction with a symmetric operator that could
// randomly get its operands swapped.
//
// Return true if swapping the operands is okay and puts the into 
// alphabetical order.
bool gla::GlslTarget::needCanonicalSwap(const llvm::Instruction* instr) const
{
    switch (instr->getOpcode()) {
    case llvm::Instruction:: Add:
    case llvm::Instruction::FAdd:
    case llvm::Instruction:: Mul:
    case llvm::Instruction::FMul:
        break;
    default:
        return false;
    }

    std::map<const llvm::Value*, std::string*>::const_iterator it0 = valueMap.find(instr->getOperand(0));
    std::map<const llvm::Value*, std::string*>::const_iterator it1 = valueMap.find(instr->getOperand(1));

    if (it0 == valueMap.end() || it1 == valueMap.end())
        return false;

    return *it0->second > *it1->second;
}

void gla::GlslTarget::declarePhiCopy(const llvm::Value* dst)
{
    newLine();
    emitGlaValue(shader, dst, 0);
    shader << ";";
}

// TODO: would be nice to have dst not forget it was an instruction
void gla::GlslTarget::addPhiCopy(const llvm::Value* dst, const llvm::Value* src)
{
    Assignment copy(this, llvm::dyn_cast<llvm::Instruction>(dst));
    emitGlaOperand(copy, src);
    copy.emit();
}

void gla::GlslTarget::addPhiAlias(const llvm::Value* dst, const llvm::Value* src)
{
    // The phi node is combining identical operations, so don't issue the copies, just
    // make the result be the same as the source.
    std::string name;
    if (getExpressionString(src, name))
        mapExpressionString(dst, name);
    else
        UnsupportedFunctionality("GLSL back end phi alias of non-mapped source", EATContinue);
}

void gla::GlslTarget::addIf(const llvm::Value* cond, bool invert)
{
    newLine();
    shader << "if (";

    if (invert)
        shader << "! ";

    emitGlaOperand(shader, cond);
    shader << ") ";
    newScope();
}

void gla::GlslTarget::addElse()
{
    leaveScope();
    shader << " else ";
    newScope();
}

void gla::GlslTarget::addEndif()
{
    leaveScope();
    newLine();
}

void gla::GlslTarget::beginConditionalLoop()
{
    UnsupportedFunctionality("conditional loops");
}

void gla::GlslTarget::beginSimpleConditionalLoop(const llvm::CmpInst* cmp, const llvm::Value* op1, const llvm::Value* op2, bool invert)
{
    newLine();
    std::string str;
    std::string opStr;
    int pos = -1;
    bool nested;
    bool emulateBitwise; // TODO: loops
    bool unsignedOp;
    GetOp(cmp, version >= 130, opStr, pos, nested, emulateBitwise, unsignedOp);

    bool binOp = false;
    if (pos == -1)
        binOp = true;

    // TODO: loops: add support for unary ops (and xor)

    if (! binOp)
        UnsupportedFunctionality("unary op for simple conditional loops");

    shader << "while (";

    if (invert)
        InvertOp(opStr);

    if (const llvm::Instruction* opInst1 = llvm::dyn_cast<llvm::Instruction>(op1)) {
        makeExtractElementStr(opInst1, str);
        if (! str.empty())
            shader << str;
        else
            emitGlaValue(shader, op1, 0);
    } else
        emitGlaValue(shader, op1, 0);
    str.clear();

    shader << " " << opStr << " ";

    if (const llvm::Instruction* opInst2 = llvm::dyn_cast<llvm::Instruction>(op2)) {
        makeExtractElementStr(opInst2, str);
        if (! str.empty())
            shader << str;
        else
            emitGlaOperand(shader, op2);
    } else
        emitGlaOperand(shader, op2);
    str.clear();

    shader << ") ";

    newScope();
}

void gla::GlslTarget::beginForLoop(const llvm::PHINode* phi, llvm::ICmpInst::Predicate predicate, unsigned bound, unsigned increment)
{
    newLine();

    shader << "for ( ; ";

    emitGlaOperand(shader, phi);
    shader << " ";
    switch (predicate) {
    default:
        UnsupportedFunctionality("loop predicate");
    case llvm::CmpInst::ICMP_NE:   shader << "!=";  break;
    case llvm::CmpInst::ICMP_SLE:  shader << "<=";  break;
    case llvm::CmpInst::ICMP_SLT:  shader << "<";   break;
    }
    shader << " " << bound << "; ";

    if (increment == 1) {
        shader << "++";
        emitGlaOperand(shader, phi);
    } else {
        emitGlaOperand(shader, phi);
        shader << " += " << increment;
    }
    shader << ") ";

    newScope();
}

void gla::GlslTarget::beginSimpleInductiveLoop(const llvm::PHINode* phi, const llvm::Value* count)
{
    newLine();

    shader << "for (";
    emitGlaValue(shader, phi, 0);

    shader << " = 0; ";

    emitGlaOperand(shader, phi);
    shader << " < ";
    emitGlaOperand(shader, count);

    shader << "; ++";
    emitGlaOperand(shader, phi);
    shader << ") ";

    newScope();
}

void gla::GlslTarget::beginInductiveLoop()
{
    UnsupportedFunctionality("inductive loops");
}

void gla::GlslTarget::beginLoop()
{
    newLine();
    shader << "while (true) ";

    newScope();
}

void gla::GlslTarget::endLoop()
{
    leaveScope();
    newLine();
}

void gla::GlslTarget::addLoopExit(const llvm::Value* condition, bool invert)
{
    if (condition)
        addIf(condition, invert);

    newLine();
    shader << "break;";

    if (condition)
        addEndif();
}

void gla::GlslTarget::addLoopBack(const llvm::Value* condition, bool invert)
{
    if (condition)
        addIf(condition, invert);

    newLine();
    shader << "continue;";

    if (condition)
        addEndif();
}

void gla::GlslTarget::addDiscard()
{
    newLine();
    shader << "discard;";
}

void gla::GlslTarget::end(llvm::Module& module)
{
    // Want 'invariant' decls after richer redeclarations, but still before shader code
    emitInvariantDeclarations(module);

    buildFullShader();
    delete generatedShader;
    generatedShader = new char[fullShader.str().size() + 1];
    strcpy(generatedShader, fullShader.str().c_str());

    delete indexShader;
    indexShader = new char[shader.str().size() + 1];
    strcpy(indexShader, shader.str().c_str());
}

void gla::GlslTarget::buildFullShader()
{
    // #version...
    fullShader << "#version " << version;
    if (version >= 150 && profile != ENoProfile) {
        switch (profile) {
        case ECoreProfile:          fullShader << " core";          break;
        case ECompatibilityProfile: fullShader << " compatibility"; break;
        case EEsProfile:            fullShader << " es";            break;
        default:
            UnsupportedFunctionality("profile");
            break;
        }
    }
    fullShader << std::endl;

    // Comment line about LunarGOO
    fullShader << "// LunarGOO output";
    // If we don't have the noRevision options
    // set, then output the revision.
    //if (! Options.noRevision)
    //    fullShader << " (r" << GLA_REVISION << ")", GLA_REVISION;
    if (obfuscate)
        fullShader << " obuscated";
    fullShader << std::endl;

    // Extensions
    for (std::set<std::string>::const_iterator extIt  = manager->getRequestedExtensions().begin(); 
                                               extIt != manager->getRequestedExtensions().end(); ++extIt)
           fullShader << "#extension " << *extIt << " : enable" << std::endl;

    // Default precision    
    if (stage == EShLangFragment && profile == EEsProfile)
        fullShader << "precision mediump float; // this will be almost entirely overridden by individual declarations" << std::endl;

    // Body of shader
    fullShader << globalStructures.str().c_str() << globalDeclarations.str().c_str() << shader.str().c_str();
}

void gla::GlslTarget::print()
{
    printf("%s", getGeneratedShader());
}

bool gla::GlslTarget::filteringIoNode(const llvm::MDNode* mdNode)
{
    return filterInactive && noStaticUseSet.find(mdNode) != noStaticUseSet.end();
}

void gla::GlslTarget::newLine()
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
            shader << indentString;
    }
}

void gla::GlslTarget::newScope()
{
    ++indentLevel;
    shader << "{";
}

void gla::GlslTarget::leaveScope()
{
    --indentLevel;
    newLine();
    shader << "}";
}

void gla::GlslTarget::addStructType(std::ostringstream& out, std::string& name, const llvm::Type* structType, const llvm::MDNode* mdAggregate, bool block)
{
    // this is mutually recursive with emitGlaType

    if (name.size() > 0) {
        // Track the mapping between LLVM's structure type and GLSL's name for it,
        // and only declare it once.
        // Note that a shadow for a block could be declared after it was declared as a block,
        // and we track that in shadowingBlock.
        if (block) {
            if (blockNameMap.find(structType) != blockNameMap.end())
                return;
            blockNameMap[structType] = name;
        } else {
            if (structNameMap.find(structType) != structNameMap.end())
                return;
            structNameMap[structType] = name;

            if (! UseLogicalIO && blockNameMap.find(structType) != blockNameMap.end())
                shadowingBlock.insert(structType);
        }
        
        // track the mapping between the type and its metadata type
        if (mdAggregate)
            typeMdAggregateMap[structType] = mdAggregate;
    }

    // For nested struct types, we have to output the nested one
    // before the containing one.  So, make the current on the side
    // and add it to the global results after its contents are
    // declared.
    std::ostringstream tempStructure;

    if (! block)
        tempStructure << "struct ";
    if (mdAggregate)
        tempStructure << std::string(mdAggregate->getOperand(0)->getName());
    else
        tempStructure << name;
    tempStructure << " {" << std::endl;

    for (int index = 0; index < (int)structType->getNumContainedTypes(); ++index) {
        tempStructure << indentString;
        if (mdAggregate) {
            const llvm::MDNode* subMdAggregate = llvm::dyn_cast<llvm::MDNode>(mdAggregate->getOperand(GetAggregateMdSubAggregateOp(index)));
            int arraySize = emitGlaType(tempStructure, EMpNone, EVQNone, structType->getContainedType(index), false, subMdAggregate);
            tempStructure << " " << std::string(mdAggregate->getOperand(GetAggregateMdNameOp(index))->getName());
            emitGlaArraySize(tempStructure, arraySize);
        } else {
            int arraySize = emitGlaType(tempStructure, EMpNone, EVQNone, structType->getContainedType(index));
            tempStructure << " " << MapGlaStructField(structType, index);
            emitGlaArraySize(tempStructure, arraySize);
        }
        tempStructure << ";" << std::endl;
    }

    tempStructure << "}";
    if (! block && name.size() > 0)
        tempStructure << ";" << std::endl;

    if (block)
        globalDeclarations << tempStructure.str();
    else {
        if (name.size() > 0)
            globalStructures << tempStructure.str();
        else
            out << tempStructure.str();
    }
}

// Add mapping for LLVM-Value -> variable-name.  The name must be a legal
// variable name, but illegal characters are allowed in 'name' and are fixed.
void gla::GlslTarget::mapVariableName(const llvm::Value* value, std::string& name)
{
    MakeParseable(name);
    mapExpressionString(value, name);
}

// Add mapping for LLVM-Value -> expression-string.  This expression can
// contain non-variable characters like expression operators, so is not
// expected to be a legal variable name.
void gla::GlslTarget::mapExpressionString(const llvm::Value* value, const std::string& name)
{
    std::map<const llvm::Value*, std::string*>::const_iterator it = valueMap.find(value);
    if (it != valueMap.end()) {
        // If the mapping is already established, don't do anything
        if (*it->second == name)
            return;

        // There are a few places that want to replace the mapping
        delete it->second;
    }

    // Make a copy so caller can pass in temporary variables
    valueMap[value] = new std::string(name);
}

// Look up a previous (supposedly) mapping from Value to string.
// Substitute a guess if not found.
// Return true if found, false if guessing.
bool gla::GlslTarget::getExpressionString(const llvm::Value* value, std::string& name) const
{
    std::map<const llvm::Value*, std::string*>::const_iterator it = valueMap.find(value);
    if (it != valueMap.end()) {
        name = *it->second;
        return true;
    }

    // Emulate the missing name.
    name = value->getName();
    MakeParseable(name);

    return false;
}

bool GlslTarget::samplerIsUint(llvm::Value* sampler) const
{
    // TODO: uint functionality: nested uint samplers: this only works for non-nested sampler types, need a pretty different way
    // for nested types
    if (llvm::Instruction* samplerInst = llvm::dyn_cast<llvm::Instruction>(sampler)) {

        // If the sampler was phi'd, go to the source.  Just go one level deep for now.
        // TODO: Can phi's get chained such that multiple jumps need to be made, and 
        // can that be done recursively without causing infinite loop?
        if (samplerInst->getOpcode() == llvm::Instruction::PHI)
            samplerInst = llvm::dyn_cast<llvm::Instruction>(samplerInst->getOperand(0));

        const llvm::MDNode* md = samplerInst->getMetadata(UniformMdName);
        if (! md) {
            // See if we can find it by name.  With disappearing metadata, this might be the proper way anyway.
            llvm::StringRef name = samplerInst->getOperand(0)->getName();
            std::map<std::string, const llvm::MDNode*>::const_iterator it = mdMap.find(name);
            if (it != mdMap.end())
                md = it->second;
        }

        if (md)
            return GetMdSamplerBaseType(md) == EMsbUint;
        else
            samplerInst->dump();
    }

    UnsupportedFunctionality("missing sampler base type", EATContinue);

    return false;
}

void gla::GlslTarget::makeNewVariableName(const llvm::Value* value, std::string& name, const char* rhs)
{
    if (obfuscate)
        makeObfuscatedName(name);
    else {
        if (IsTempName(value->getName())) {
            if (rhs && strlen(rhs) > 0)
                makeHashName("H_", rhs, name);
            else {                
                name.append("Lg_");
                IntToString(++lastVariable, name);
            }
        } else {
            name = value->getName();
            canonicalizeName(name);
        }

        // Variables starting with gl_ are illegal in GLSL
        if (name.substr(0,3) == std::string("gl_"))
            name[0] = 'L';
    }
}

void gla::GlslTarget::makeNewVariableName(const char* base, std::string& name)
{
    const size_t bufSize = 20;
    char buf[bufSize];
    if (obfuscate)
        makeObfuscatedName(name);
    else {
        name.append(base);
        snprintf(buf, bufSize, "%x", ++lastVariable);
        name.append(buf);
    }
}

void gla::GlslTarget::makeHashName(const char* prefix, const char* key, std::string& name)
{
    name.append(prefix);
    IntToString(_Hash_seq((const unsigned char*)key, strlen(key)), name);
    while (hashedNames.find(name) != hashedNames.end())
        name.append("r");
    hashedNames.insert(name);
} 

void gla::GlslTarget::makeObfuscatedName(std::string& name)
{
    int i;
    for (i = 0; i <= lastVariable-4; i += 4) {
        switch ((i/4) % 4) {
        case 0:   name.append("x"); break;
        case 1:   name.append("y"); break;
        case 2:   name.append("z"); break;
        case 3:   name.append("w"); break;
        default:                    break;
        }
    }
    switch (lastVariable - i) {
    case 0:   name.append("x"); break;
    case 1:   name.append("y"); break;
    case 2:   name.append("z"); break;
    case 3:   name.append("w"); break;
    default:                    break;
    }
}

// Make variable names more predictable across runs, compromising
// between preserving the original name (which might have included a number)
// and always getting the same name now (replacing numbers added by LLVM).
void gla::GlslTarget::canonicalizeName(std::string& name)
{
    // throw away starting from the first '.'
    int dotPos = name.find('.');
    if (dotPos != std::string::npos)
        name.resize(dotPos);

    // remove any existing end counting and .i type things
    int pos = name.size() - 1;
    while (pos > 0 && name[pos] >= '0' && name[pos] <= '9')
        --pos;

    name.resize(pos + 1);
    if (name.size() == 0)
        name = "_L";

    std::map<std::string, int>::iterator it = canonMap.find(name);
    if (it  == canonMap.end())
        canonMap[name] = 0;
    else {
        ++it->second;
        IntToString(it->second, name);
    }
}

// Makes a string representation for the given swizzling ExtractElement
// instruction, and sets str to it. Does nothing if passed something that
// isn't an ExtractElement.
void gla::GlslTarget::makeExtractElementStr(const llvm::Instruction* llvmInstruction, std::string& str)
{
    if (! llvm::isa<llvm::ExtractElementInst>(llvmInstruction))
        return;

    str.assign(*valueMap[llvmInstruction->getOperand(0)]);
    llvm::Value* element = llvmInstruction->getOperand(1);
    if (llvm::isa<llvm::Constant>(element))
        str.append(".").append(MapComponentToSwizzleChar(GetConstantInt(llvmInstruction->getOperand(1))));
    else
        str.append("[").append(*valueMap[element]).append("]");
}

// Turn a gep instruction or pointer for load operand into a full GLSL expression
// representing the dereference of it.
// 'ptr' is the thing getting an expression built for it and mapped to
// 'additionalToMap' is a secondary value to also map to the same results
void gla::GlslTarget::mapPointerExpression(const llvm::Value* ptr, const llvm::Value* additionalToMap)
{
    // There is extra processing if it represents an external (interface) access,
    // based on the metadata associated with this globally visible name.
    const llvm::MDNode* mdNode;
    EMdTypeLayout mdType;
    EMdTypeLayout* mdTypePointer;

    // TODO: correctness: make sure this doesn't pick up local variable names that coincidentally match a global name
    const llvm::GetElementPtrInst* gepInst = getGepAsInst(ptr);
    llvm::StringRef name;
    if (gepInst)
        name = gepInst->getOperand(0)->getName();
    else
        name = ptr->getName();
    std::string expression;
    if (mdMap.find(name) != mdMap.end()) {
        mdNode = mdMap[name];
        mdType = GetMdTypeLayout(mdNode);
        mdTypePointer = &mdType;
        expression = mdNode->getOperand(0)->getName();
    } else {
        if (MapGlaAddressSpace(ptr) == EVQGlobal) {
            // This could be a path for a hoisted "undef" aggregate.  See hoistUndefOps().
            // (Normally, everything should be in registers.)
            // TODO: output code quality: Find a way to eliminate global undefs so there are not extra "var = global-aggregete" statements
            //       in the created output.
            mdNode = 0;
            mdType = EMtlNone;
            mdTypePointer = 0;   // This is not a uniform, so don't process any metadata for it.
            if (gepInst && ! getExpressionString(gepInst->getOperand(0), expression))
                UnsupportedFunctionality("GLSL back end gep missing value->string mapping", 0, expression.c_str(), EATContinue);
        } else {
            UnsupportedFunctionality("missing metadata for makePointerExpression", MapGlaAddressSpace(ptr), name.str().c_str());
        }
    }

    // traverse the dereference chain and store it.
    if (gepInst)
        expression.append(traverseGep(gepInst, mdTypePointer));

    // an anonymous block will leave an expression starting with "."; remove it
    if (expression[0] == '.')
        expression = expression.substr(1, std::string::npos);

    // Conversion-wrap it and make the whole thing the expression of a variable
    // (correct for r-values, but not for l-values, hence the need for nonConvertedMap).
    if (mdType == EMtlUnsigned || mdType == EMtlRowMajorMatrix || mdType == EMtlColMajorMatrix) {
        // Keep the non-converted version.  L-values need this.  Also useful as an optimization if
        // at a future point we can tell the non-converted one is okay; e.g., a matrix
        // dereference never had to be constructed into an array of arrays
        nonConvertedMap[ptr] = new std::string(expression);
        if (additionalToMap)
            nonConvertedMap[additionalToMap] = new std::string(expression);
        ConversionWrap(expression, ptr->getType()->getContainedType(0), false);
    }

    mapExpressionString(ptr, expression);
    if (additionalToMap)
        mapExpressionString(additionalToMap, expression);
}

//
// Handle the subcase of an LLVM instruction being an intrinsic call.
//
void gla::GlslTarget::emitGlaIntrinsic(std::ostringstream& out, const llvm::IntrinsicInst* llvmInstruction)
{
    // Handle pipeline read/write and non-gla intrinsics
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_writeData:
    case llvm::Intrinsic::gla_fWriteData:
        emitMapGlaIOIntrinsic(llvmInstruction, false);
        return;

    case llvm::Intrinsic::gla_readData:
    case llvm::Intrinsic::gla_fReadData:
    case llvm::Intrinsic::gla_fReadInterpolant:
        emitMapGlaIOIntrinsic(llvmInstruction, true);
        return;

    case llvm::Intrinsic::invariant_end:
    case llvm::Intrinsic::invariant_start:
    case llvm::Intrinsic::lifetime_end:
    case llvm::Intrinsic::lifetime_start:
        return;

    case llvm::Intrinsic::stackprotector:
    case llvm::Intrinsic::stackprotectorcheck:
    case llvm::Intrinsic::stackrestore:
    case llvm::Intrinsic::stacksave:
        return;

    default:
        // fall through
        break;
    }

    EMdPrecision precision = GetPrecision(llvmInstruction);
    Assignment assignment(this, llvmInstruction);

    // Handle texturing
    bool gather = false;
    bool refZemitted = false;
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_queryTextureSize:
    case llvm::Intrinsic::gla_queryTextureSizeNoLod:

        newLine();
        emitGlaValue(out, llvmInstruction, 0);
        out << " = textureSize(";
        emitGlaOperand(out, llvmInstruction->getOperand(GetTextureOpIndex(ETOSamplerLoc)));
        if (llvmInstruction->getNumArgOperands() > 2) {
            out << ", ";
            emitGlaOperand(out, llvmInstruction->getOperand(2));
        }
        out << ");";
        return;

    case llvm::Intrinsic::gla_fQueryTextureLod:

        newLine();
        emitGlaValue(out, llvmInstruction, 0);
        out << " = textureQueryLod(";
        emitGlaOperand(out, llvmInstruction->getOperand(GetTextureOpIndex(ETOSamplerLoc)));
        out << ", ";
        emitGlaOperand(out, llvmInstruction->getOperand(2));
        out << ");";
        return;

    //case llvm::Intrinsic::gla_queryTextureLevels:
    // TODO: 430 Functionality: textureQueryLevels()

    case llvm::Intrinsic::gla_texelGather:
    case llvm::Intrinsic::gla_fTexelGather:
    case llvm::Intrinsic::gla_texelGatherOffset:
    case llvm::Intrinsic::gla_fTexelGatherOffset:
    case llvm::Intrinsic::gla_texelGatherOffsets:
    case llvm::Intrinsic::gla_fTexelGatherOffsets:
        gather = true;
        // fall through
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
    {
        bool needConversion = samplerIsUint(llvmInstruction->getOperand(GetTextureOpIndex(ETOSamplerLoc)));
        if (needConversion)
            ConversionStart(assignment, llvmInstruction->getType(), false);
        emitGlaSamplerFunction(assignment, llvmInstruction, GetConstantInt(llvmInstruction->getOperand(GetTextureOpIndex(ETOFlag))));
        assignment << "(";
        emitGlaOperand(assignment, llvmInstruction->getOperand(GetTextureOpIndex(ETOSamplerLoc)));
        assignment << ", ";

        if(NeedsShadowRefZArg(llvmInstruction) && ! gather) {

            // Construct a new vector of size coords+1 to hold coords and shadow ref
            int coordWidth = gla::GetComponentCount(llvmInstruction->getOperand(GetTextureOpIndex(ETOCoord)));
            assert(coordWidth < 4);

            llvm::Type* coordType = llvmInstruction->getOperand(GetTextureOpIndex(ETOCoord))->getType();

            if (coordType->isVectorTy())
                coordType = coordType->getContainedType(0);

            // RefZ must reside in 3rd component or higher, so detect single component case
            int buffer = (coordWidth == 1) ? 1 : 0;

            llvm::Type* vecType = llvm::VectorType::get(coordType, coordWidth + buffer + 1);

            emitGlaType(assignment, precision, EVQNone, vecType);

            assignment << "(";

            // Texcoords first
            emitGlaOperand(assignment, llvmInstruction->getOperand(GetTextureOpIndex(ETOCoord)));
            assignment << ", ";

            // Insert unused 2nd channel for 1D coordinate
            if (buffer > 0)
                assignment << "0, ";

            // Followed by scalar shadow ref
            assert(gla::IsScalar(llvmInstruction->getOperand(GetTextureOpIndex(ETORefZ))));
            emitGlaOperand(assignment, llvmInstruction->getOperand(GetTextureOpIndex(ETORefZ)));

            assignment << ")";

            refZemitted = true;
        } else {
            emitGlaOperand(assignment, llvmInstruction->getOperand(GetTextureOpIndex(ETOCoord)));
        }
        
        if ((! refZemitted) && NeedsShadowRefZArg(llvmInstruction)) {
            assignment << ", ";
            emitGlaOperand(assignment, llvmInstruction->getOperand(GetTextureOpIndex(ETORefZ)));
        }

        if(NeedsLodArg(llvmInstruction)) {
            assignment << ", ";
            emitGlaOperand(assignment, llvmInstruction->getOperand(GetTextureOpIndex(ETOBiasLod)));
        }

        if(IsGradientTexInst(llvmInstruction)) {
            assignment << ", ";
            emitGlaOperand(assignment, llvmInstruction->getOperand(GetTextureOpIndex(ETODPdx)));
            assignment << ", ";
            emitGlaOperand(assignment, llvmInstruction->getOperand(GetTextureOpIndex(ETODPdy)));
        }

        bool offsets;
        if (NeedsOffsetArg(llvmInstruction, offsets)) {
            if (offsets) {

                // declare a new const to hold the array (the array was lost when setting the intrinsic arguments)
                std::string name;
                makeNewVariableName("offsets_", name);
                globalDeclarations << "const ivec2[4] " << name << " = ivec2[4](";
                for (int i = 0; i < 4; ++i) {
                    if (i > 0)
                        globalDeclarations << ", ";
                    llvm::Constant* offset = llvm::dyn_cast<llvm::Constant>(llvmInstruction->getOperand(GetTextureOpIndex(ETOOffset) + i));
                    emitConstantInitializer(globalDeclarations, offset, offset->getType());
                }
                globalDeclarations << ");" << std::endl;

                // consume the new const
                assignment << ", ";
                assignment << name;
            } else {
                assignment << ", ";
                emitGlaOperand(assignment, llvmInstruction->getOperand(GetTextureOpIndex(ETOOffset)));
            }
        }

        if(NeedsBiasArg(llvmInstruction) || NeedsComponentArg(llvmInstruction)) {
            assignment << ", ";
            emitGlaOperand(assignment, llvmInstruction->getOperand(GetTextureOpIndex(ETOBiasLod)));
        }

        if (needConversion)
            ConversionStop(assignment, llvmInstruction->getType());
        assignment << ")";

        assignment.emit();

        return;
    }
    default:
        break;
    }

    // Handle swizzles
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_swizzle:
    case llvm::Intrinsic::gla_fSwizzle:
    {
        llvm::Constant* mask = llvm::dyn_cast<llvm::Constant>(llvmInstruction->getOperand(1));
        assert(mask);

        llvm::SmallVector<llvm::Constant*, 8> elts;
        gla_llvm::GetElements(mask, elts);

        int dstVectorWidth = 0;
        if (! AreAllDefined(mask)) {
            std::ostringstream mask;
            emitGlaWriteMask(mask, elts);
            assignment.setMember(mask.str().c_str());
            dstVectorWidth = getDefinedCount(elts);
        } else {
            dstVectorWidth = GetComponentCount(mask);
            assert(dstVectorWidth == GetComponentCount(llvmInstruction));
        }

        llvm::Value* src = llvmInstruction->getOperand(0);
        int srcVectorWidth = GetComponentCount(src);

        // Case 0:  it's scalar making a scalar.
        // use nothing, just copy
        if (srcVectorWidth == 1 && dstVectorWidth == 1) {
            emitGlaOperand(assignment, src);
            assignment.mapOrEmit(true, false);

            return;
        }

        // Case 1:  it's a scalar with multiple ".x" to expand it to a vector.
        // use a constructor to turn a scalar into a vector
        if (srcVectorWidth == 1 && dstVectorWidth > 1) {
            emitGlaType(assignment, precision, EVQNone, llvmInstruction->getType());
            assignment << "(";
            emitGlaOperand(assignment, src);
            assignment << ")";
            assignment.mapOrEmit(true, false);

            return;
        }

        // Case 2:  it's a subsetting of a vector.
        // use GLSL swizzles
        assert(srcVectorWidth > 1);
        emitGlaOperand(assignment, src);
        emitGlaSwizzle(assignment, elts);
        assignment.mapOrEmit(true, false);
        return;
    }
    default:
        break;
    }

    // Handle multiInserts
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_fMultiInsert:
    case llvm::Intrinsic::gla_multiInsert:
        emitGlaMultiInsert(out, llvmInstruction);
        return;
    default:
        break;
    }

    // Handle matrix * vector and vector * matrix intrinsics
    int numCols = 0;
    bool matLeft;
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_fMatrix2TimesVector: numCols = 2; matLeft = true;  break;
    case llvm::Intrinsic::gla_fMatrix3TimesVector: numCols = 3; matLeft = true;  break;
    case llvm::Intrinsic::gla_fMatrix4TimesVector: numCols = 4; matLeft = true;  break;
    case llvm::Intrinsic::gla_fVectorTimesMatrix2: numCols = 2; matLeft = false; break;
    case llvm::Intrinsic::gla_fVectorTimesMatrix3: numCols = 3; matLeft = false; break;
    case llvm::Intrinsic::gla_fVectorTimesMatrix4: numCols = 4; matLeft = false; break;
    default: break;
    }
    if (numCols) {
        int argBias = 0;
        if (! matLeft) {
            emitGlaOperand(assignment, llvmInstruction->getOperand(0));
            assignment << " * ";
            argBias = 1;
        }

        assignment << "mat" << numCols << "x" << GetComponentCount(llvmInstruction->getOperand(1)) << "(";
        for (int col = 0; col < numCols; ++col) {
            if (col > 0)
                assignment << ", ";
            emitGlaOperand(assignment, llvmInstruction->getOperand(col + argBias));
        }
        assignment << ")";

        if (matLeft) {
            assignment << " * ";
            emitGlaOperand(assignment, llvmInstruction->getOperand(numCols));
        }
        assignment.emit();
        return;
    }

    // Handle matrix * matrix intrinsics
    int numLeftCols = 0;
    int numRightCols = 0;
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_fMatrix2TimesMatrix2: numLeftCols = 2; numRightCols = 2; break;
    case llvm::Intrinsic::gla_fMatrix2TimesMatrix3: numLeftCols = 2; numRightCols = 3; break;
    case llvm::Intrinsic::gla_fMatrix2TimesMatrix4: numLeftCols = 2; numRightCols = 4; break;
    case llvm::Intrinsic::gla_fMatrix3TimesMatrix2: numLeftCols = 3; numRightCols = 2; break;
    case llvm::Intrinsic::gla_fMatrix3TimesMatrix3: numLeftCols = 3; numRightCols = 3; break;
    case llvm::Intrinsic::gla_fMatrix3TimesMatrix4: numLeftCols = 3; numRightCols = 4; break;
    case llvm::Intrinsic::gla_fMatrix4TimesMatrix2: numLeftCols = 4; numRightCols = 2; break;
    case llvm::Intrinsic::gla_fMatrix4TimesMatrix3: numLeftCols = 4; numRightCols = 3; break;
    case llvm::Intrinsic::gla_fMatrix4TimesMatrix4: numLeftCols = 4; numRightCols = 4; break;
    default: break;
    }
    if (numLeftCols) {
        // First, we have to make a temp. matrix, because LLVM is making a struct: TODO: matrix intrinsics
//        assignment << "mat" << numLeftCols << " ";
//        assignment << "StructMat = ";

        assignment << "mat" << numLeftCols << "x" << numRightCols << "(";
        for (int col = 0; col < numLeftCols; ++col) {
            if (col > 0)
                assignment << ", ";
            emitGlaOperand(assignment, llvmInstruction->getOperand(col));
        }
        assignment << ") * mat" << numRightCols << "x" << numLeftCols << "(";
        for (int col = 0; col < numRightCols; ++col) {
            if (col > 0)
                assignment << ", ";
            emitGlaOperand(assignment, llvmInstruction->getOperand(col + numLeftCols));
        }
        assignment << ")";
        assignment.emit();

        // Now we can set up the original instruction's value
        //newLine();
        //emitGlaValue(assignment, llvmInstruction);
        //assignment << 

        return;
    }

    // Handle fixedTransform
    if (llvmInstruction->getIntrinsicID() == llvm::Intrinsic::gla_fFixedTransform) {
        assignment << "ftransform()";
        assignment.emit();
        return;
    }

    // Handle the one-to-one mappings
    const char* callString = 0;
    int forceWidth = 0;
    
    // see if the result will need to be converted to int
    bool convertResultToInt;
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_fPackUnorm2x16:
    case llvm::Intrinsic::gla_fPackSnorm2x16:
    case llvm::Intrinsic::gla_fPackHalf2x16:
        convertResultToInt = true;
        break;
    default:
        convertResultToInt = false;
        break;
    }

    // see if the arguments need to be converted to uint
    bool convertArgsToUint;
    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_uMin:
    case llvm::Intrinsic::gla_uMax:
    case llvm::Intrinsic::gla_uClamp:
    case llvm::Intrinsic::gla_umulExtended:
    case llvm::Intrinsic::gla_uBitFieldExtract:
    case llvm::Intrinsic::gla_uFindMSB:
    case llvm::Intrinsic::gla_fUnpackUnorm2x16:
    case llvm::Intrinsic::gla_fUnpackSnorm2x16:
    case llvm::Intrinsic::gla_fUnpackHalf2x16:
        convertArgsToUint = true;
        break;
    default:
        convertArgsToUint = false;
        break;
    }

    // map the instrinsic to the right GLSL built-in
    switch (llvmInstruction->getIntrinsicID()) {

    // Floating-Point and Integer Operations
    case llvm::Intrinsic::gla_abs:
    case llvm::Intrinsic::gla_fAbs:         callString = "abs";   break;

    case llvm::Intrinsic::gla_sMin:
    case llvm::Intrinsic::gla_uMin:
    case llvm::Intrinsic::gla_fMin:         callString = "min";   break;

    case llvm::Intrinsic::gla_sMax:
    case llvm::Intrinsic::gla_uMax:
    case llvm::Intrinsic::gla_fMax:         callString = "max";   break;

    case llvm::Intrinsic::gla_sClamp:
    case llvm::Intrinsic::gla_uClamp:
    case llvm::Intrinsic::gla_fClamp:
    case llvm::Intrinsic::gla_fSaturate:    callString = "clamp"; break;

    case llvm::Intrinsic::gla_fRadians:     callString = "radians";     break;
    case llvm::Intrinsic::gla_fDegrees:     callString = "degrees";     break;
    case llvm::Intrinsic::gla_fSin:         callString = "sin";         break;
    case llvm::Intrinsic::gla_fCos:         callString = "cos";         break;
    case llvm::Intrinsic::gla_fTan:         callString = "tan";         break;
    case llvm::Intrinsic::gla_fAsin:        callString = "asin";        break;
    case llvm::Intrinsic::gla_fAcos:        callString = "acos";        break;
    case llvm::Intrinsic::gla_fAtan:        callString = "atan";        break;
    case llvm::Intrinsic::gla_fAtan2:       callString = "atan";        break;
    case llvm::Intrinsic::gla_fSinh:        callString = "sinh";        break;
    case llvm::Intrinsic::gla_fCosh:        callString = "cosh";        break;
    case llvm::Intrinsic::gla_fTanh:        callString = "tanh";        break;
    case llvm::Intrinsic::gla_fAsinh:       callString = "asinh";       break;
    case llvm::Intrinsic::gla_fAcosh:       callString = "acosh";       break;
    case llvm::Intrinsic::gla_fAtanh:       callString = "atanh";       break;
    case llvm::Intrinsic::gla_fPow:         callString = "pow";         break;
    //case llvm::Intrinsic::gla_fPowi:        callString = "powi";        break;
    case llvm::Intrinsic::gla_fExp:         callString = "exp";         break;
    case llvm::Intrinsic::gla_fLog:         callString = "log";         break;
    case llvm::Intrinsic::gla_fExp2:        callString = "exp2";        break;
    case llvm::Intrinsic::gla_fLog2:        callString = "log2";        break;
    //case llvm::Intrinsic::gla_fExp10:       callString = "exp10";       break;
    //case llvm::Intrinsic::gla_fLog10:       callString = "log10";       break;
    case llvm::Intrinsic::gla_fSqrt:        callString = "sqrt";        break;
    case llvm::Intrinsic::gla_fInverseSqrt: callString = "inversesqrt"; break;
    case llvm::Intrinsic::gla_fSign:        callString = "sign";        break;
    case llvm::Intrinsic::gla_sign:         callString = "sign";        break;
    case llvm::Intrinsic::gla_fFloor:       callString = "floor";       break;
    case llvm::Intrinsic::gla_fCeiling:     callString = "ceil";        break;
    case llvm::Intrinsic::gla_fRoundEven:   callString = "roundEven";   break;
    case llvm::Intrinsic::gla_fRoundZero:   callString = "trunc";       break;
    case llvm::Intrinsic::gla_fRoundFast:   callString = "round";       break;
    case llvm::Intrinsic::gla_fFraction:    callString = "fract";       break;
    case llvm::Intrinsic::gla_fModF:        callString = "modf";        break;
    case llvm::Intrinsic::gla_fMix:         callString = "mix";         break;
    case llvm::Intrinsic::gla_fbMix:        callString = "mix";         break;
    case llvm::Intrinsic::gla_fStep:        callString = "step";        break;
    case llvm::Intrinsic::gla_fSmoothStep:  callString = "smoothstep";  break;
    case llvm::Intrinsic::gla_fIsNan:       callString = "isnan";       break;
    case llvm::Intrinsic::gla_fIsInf:       callString = "isinf";       break;
    case llvm::Intrinsic::gla_fFma:         callString = "fma";         break;

    // Integer-Only Operations
    case llvm::Intrinsic::gla_addCarry:     callString = "addCarry";     break;
    case llvm::Intrinsic::gla_subBorrow:    callString = "subBorrow";    break;
    case llvm::Intrinsic::gla_umulExtended: callString = "umulExtended"; break;
    case llvm::Intrinsic::gla_smulExtended: callString = "smulExtended"; break;

    // Bit Operations
    case llvm::Intrinsic::gla_fFloatBitsToInt:  callString = "floatBitsToInt";   break;
    case llvm::Intrinsic::gla_fIntBitsTofloat:  callString = "intBitsToFloat";   break;
    case llvm::Intrinsic::gla_sBitFieldExtract:
    case llvm::Intrinsic::gla_uBitFieldExtract: callString = "bitFieldExtract";  break;
    case llvm::Intrinsic::gla_bitFieldInsert:   callString = "bitFieldInsert";   break;
    case llvm::Intrinsic::gla_bitReverse:       callString = "bitFieldReverse";  break;
    case llvm::Intrinsic::gla_bitCount:         callString = "bitCount";         break;
    case llvm::Intrinsic::gla_findLSB:          callString = "findLSB";          break;
    case llvm::Intrinsic::gla_sFindMSB:
    case llvm::Intrinsic::gla_uFindMSB:         callString = "findMSB";          break;

    // Pack and Unpack
    //case llvm::Intrinsic::gla_fFrexp:            callString = "frexp";              break;
    //case llvm::Intrinsic::gla_fLdexp:            callString = "ldexp";              break;
    case llvm::Intrinsic::gla_fPackUnorm2x16:    callString = "packUnorm2x16";      break;
    case llvm::Intrinsic::gla_fUnpackUnorm2x16:  callString = "unpackUnorm2x16";    break;

    case llvm::Intrinsic::gla_fPackSnorm2x16:    callString = "packSnorm2x16";      break;
    case llvm::Intrinsic::gla_fUnpackSnorm2x16:  callString = "unpackSnorm2x16";    break;

    case llvm::Intrinsic::gla_fPackHalf2x16:     callString = "packHalf2x16";       break;
    case llvm::Intrinsic::gla_fUnpackHalf2x16:   callString = "unpackHalf2x16";     break;

    case llvm::Intrinsic::gla_fPackUnorm4x8:     callString = "packUnorm4x8";       break;
    case llvm::Intrinsic::gla_fPackSnorm4x8:     callString = "packSnorm4x8";       break;

    case llvm::Intrinsic::gla_fUnpackUnorm4x8:   callString = "unpackUnorm4x8";     break;
    case llvm::Intrinsic::gla_fUnpackSnorm4x8:   callString = "unpackSnorm4x8";     break;

    case llvm::Intrinsic::gla_fPackDouble2x32:   callString = "packDouble2x32";     break;
    case llvm::Intrinsic::gla_fUnpackDouble2x32: callString = "unpackDouble2x32";   break;

    // Geometry
    case llvm::Intrinsic::gla_fLength:      callString = "length";      break;
    case llvm::Intrinsic::gla_fDistance:    callString = "distance";    break;
    case llvm::Intrinsic::gla_fDot2:        
    case llvm::Intrinsic::gla_fDot3:        
    case llvm::Intrinsic::gla_fDot4:        callString = "dot";         break;
    case llvm::Intrinsic::gla_fCross:       callString = "cross";       break;
    case llvm::Intrinsic::gla_fNormalize:   callString = "normalize";   break;
    case llvm::Intrinsic::gla_fNormalize3D:                             break;
    case llvm::Intrinsic::gla_fLit:                                     break;
    case llvm::Intrinsic::gla_fFaceForward: callString = "faceforward"; break;
    case llvm::Intrinsic::gla_fReflect:     callString = "reflect";     break;
    case llvm::Intrinsic::gla_fRefract:     callString = "refract";     break;

    // Derivative and Transform
    case llvm::Intrinsic::gla_fDFdx:        callString = "dFdx";        break;
    case llvm::Intrinsic::gla_fDFdy:        callString = "dFdy";        break;
    case llvm::Intrinsic::gla_fFilterWidth: callString = "fwidth";      break;

    // Vector Logical
    case llvm::Intrinsic::gla_not:          callString = "not";         break;
    case llvm::Intrinsic::gla_any:          callString = "any";         break;
    case llvm::Intrinsic::gla_all:          callString = "all";         break;

    // Control
    case llvm::Intrinsic::gla_barrier:                    callString = "barrier";                    break;
    case llvm::Intrinsic::gla_memoryBarrier:              callString = "memoryBarrier";              break;
    case llvm::Intrinsic::gla_memoryBarrierAtomicCounter: callString = "memoryBarrierAtomicCounter"; break;
    case llvm::Intrinsic::gla_memoryBarrierBuffer:        callString = "memoryBarrierBuffer";        break;
    case llvm::Intrinsic::gla_memoryBarrierImage:         callString = "memoryBarrierImage";         break;
    case llvm::Intrinsic::gla_memoryBarrierShared:        callString = "memoryBarrierShared";        break;
    case llvm::Intrinsic::gla_groupMemoryBarrier:         callString = "groupMemoryBarrier";         break;

    // Geometry
    case llvm::Intrinsic::gla_emitVertex:                 callString = "EmitVertex";                 break;
    case llvm::Intrinsic::gla_endPrimitive:               callString = "EndPrimitive";               break;
    case llvm::Intrinsic::gla_emitStreamVertex:           callString = "EmitStreamVertex";           break;
    case llvm::Intrinsic::gla_endStreamPrimitive:         callString = "EmitStreamVertex";           break;

    default: break;
    }

    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_fDot2:  forceWidth = 2;  break;
    case llvm::Intrinsic::gla_fDot3:  forceWidth = 3;  break;
    case llvm::Intrinsic::gla_fDot4:  forceWidth = 4;  break;
    default: break;
    }

    // deal specially with fmodf(), then treat everything else about the same.
    if (llvmInstruction->getIntrinsicID() == llvm::Intrinsic::gla_fModF) {
        newLine();
        emitGlaValue(out, llvmInstruction, 0);
        out << "; ";
        out << valueMap[llvmInstruction]->c_str() << ".member0";
        out << " = " << callString << "(";
        emitGlaOperand(out, llvmInstruction->getOperand(0));
        out << ", " << valueMap[llvmInstruction]->c_str() << ".member1" << ");";

        return;
    }

    if (callString == 0)
        UnsupportedFunctionality("Intrinsic in Bottom IR", EATContinue);

    if (llvmInstruction->getType()->getTypeID() == llvm::Type::VoidTyID)
        assignment.setNoLvalue();

    if (convertResultToInt)
        ConversionStart(assignment, llvmInstruction->getType(), false);

    if (callString)
        assignment << callString << "(";
    else
        assignment << "unknownIntrinsic(";

    // The arguments coming across from LLVM into GLSL
    for (unsigned int arg = 0; arg < llvmInstruction->getNumArgOperands(); ++arg) {
        if (arg > 0)
            assignment << ", ";
        if (convertArgsToUint)
            ConversionStart(assignment, llvmInstruction->getOperand(arg)->getType(), true);
        emitGlaOperand(assignment, llvmInstruction->getOperand(arg));
        if (forceWidth && forceWidth < GetComponentCount(llvmInstruction->getOperand(arg)))
            emitComponentCountToSwizzle(assignment, forceWidth);
        if (convertArgsToUint) 
            ConversionStop(assignment, llvmInstruction->getOperand(arg)->getType());
    }

    // Some special-case arguments
    if (llvmInstruction->getIntrinsicID() == llvm::Intrinsic::gla_fSaturate)
        assignment << ", 0.0, 1.0";

    // Finish it off
    if (convertResultToInt)
        ConversionStop(assignment, llvmInstruction->getType());
    assignment << ")";

    // If amount of data is getting smaller, it's better to emit it now
    bool increasesData = llvmInstruction->getNumArgOperands() <= 1 && 
                         (llvmInstruction->getNumArgOperands() != 1 || GetComponentCount(llvmInstruction->getOperand(0)) <= 
                                                                       GetComponentCount(llvmInstruction));
    assignment.mapOrEmit(increasesData, false);
}

//
// Handle real function calls.
//
void gla::GlslTarget::emitGlaCall(std::ostringstream& out, const llvm::CallInst* call)
{
    newLine();
    emitGlaValue(out, call, 0);
    out << " = " << std::string(call->getCalledFunction()->getName()) << "(";
    for (int arg = 0; arg < (int)call->getNumArgOperands(); ++arg) {
        emitGlaOperand(out, call->getArgOperand(arg));
        if (arg + 1 < (int)call->getNumArgOperands())
            out << ", ";
    }

    out << ");";
}

void gla::GlslTarget::emitGlaPrecision(std::ostringstream& out, EMdPrecision precision)
{
    switch (precision) {
    case EMpLow:
    case EMpMedium:
    case EMpHigh:
        out << MapGlaToPrecisionString(precision) << " ";
        break;

    case EMpNone:
        break;

    default:
        out << "badp ";
        break;
    }   
}

void gla::GlslTarget::emitComponentCountToSwizzle(std::ostringstream& out, int numComponents)
{
    out << ".";

    switch (numComponents) {
    case 1:   out << "x";     break;
    case 2:   out << "xy";    break;
    case 3:   out << "xyz";   break;
    case 4:   out << "xyzw";  break;
    default:
              assert(! "Vector too large");
              out << "xyzw";  break;
    }
}

void gla::GlslTarget::emitComponentToSwizzle(std::ostringstream& out, int component)
{
    out << MapComponentToSwizzleChar(component);
}

void gla::GlslTarget::emitMaskToSwizzle(std::ostringstream& out, int mask)
{
    out << ".";

    for (int component = 0; component < 4; ++component)
        if (mask & (1 << component))
            out << MapComponentToSwizzleChar(component);
}

void gla::GlslTarget::emitGlaSamplerFunction(std::ostringstream& out, const llvm::IntrinsicInst* llvmInstruction, int texFlags)
{
    const llvm::Value* samplerType = llvmInstruction->getOperand(0);

    // TODO: uint functionality: See if it's a uint sampler, requiring a constructor to convert it

    // Original style shadowing returns vec4 while 2nd generation returns float,
    // so, have to stick to old-style for those cases.
    bool forceOldStyle = IsVector(llvmInstruction->getType()) && (texFlags & ETFShadow) && ((texFlags & ETFGather) == 0);

    if (version >= 130 && ! forceOldStyle) {
        if (texFlags & ETFFetch)
            out << "texelFetch";
        else
            out << "texture";
    } else {
        if (texFlags & ETFShadow)
            out << "shadow";
        else
            out << "texture";

        int sampler = GetConstantInt(samplerType);

        switch (sampler) {
        case ESampler1D:        out << "1D";     break;
        case ESampler2D:        out << "2D";     break;
        case ESampler3D:        out << "3D";     break;
        case ESamplerCube:      out << "Cube";   break;
        case ESampler2DRect:    out << "2DRect"; break;
        default:
            UnsupportedFunctionality("Texturing in Bottom IR: ", sampler, EATContinue);
            break;
        }
    }
        
    if (texFlags & ETFProjected)
        out << "Proj";
    if (texFlags & ETFLod)
        out << "Lod";
    if (IsGradientTexInst(llvmInstruction))
        out << "Grad";
    if (texFlags & ETFGather)
        out << "Gather";
    if (texFlags & ETFOffsetArg) {
        if (texFlags & ETFOffsets)
            out << "Offsets";
        else
            out << "Offset";
    }

    if (forceOldStyle && IsGradientTexInst(llvmInstruction) && (texFlags & ETFProjected))
        out << "ARB";
}

void gla::GlslTarget::emitNamelessConstDeclaration(const llvm::Value* value, const llvm::Constant* constant)
{
    std::ostringstream constString;
    emitConstantInitializer(constString, constant, constant->getType());
    
    std::string name;
    if (! getExpressionString(value, name)) {
        name = "";
        std::map<std::string, const std::string*>::const_iterator it = constMap.find(constString.str());
        if (it != constMap.end()) {
            // duplicate of some already declared constant 
            mapExpressionString(value, *it->second);
            return;
        } else {
            if (constString.str().size() < 12) {
                name = "C_";
                name.append(constString.str());
                for (int i = 0; i < (int)name.size(); ++i) {
                    if (! ValidIdentChar(name[i])) {
                        switch (name[i]) {
                        case ',': name[i] = 'c'; break;
                        case '.': name[i] = 'd'; break;
                        case '(': name[i] = 'p'; break;
                        case ')': name[i] = 'p'; break;
                        default:  name[i] = 'a'; break;
                        }
                    }
                }
            } else
                makeHashName("C_", constString.str().c_str(), name);
            constMap[constString.str()] = new std::string(name);
        }
        mapExpressionString(value, name);
    }

    int arraySize = emitGlaType(globalDeclarations, EMpNone, EVQConstant, value->getType());
    globalDeclarations << " " << name;
    emitGlaArraySize(globalDeclarations, arraySize);
    globalDeclarations << " = ";
    globalDeclarations << constString.str();
    globalDeclarations << ";" << std::endl;
}

void gla::GlslTarget::emitVariableDeclaration(EMdPrecision precision, llvm::Type* type, const std::string& name, EVariableQualifier qualifier, 
                                              const llvm::Constant* constant, const llvm::MDNode* mdIoNode)
{
    if (name.compare(0, 3, "gl_") == 0)
        return;

    int arraySize;

    if (canonMap.find(name) == canonMap.end())
        canonMap[name] = 0;

    // If it has an initializer (is a constant and not an undef)
    if (constant && ! AreAllUndefined(constant)) {
        arraySize = emitGlaType(globalDeclarations, precision, qualifier, type);
        globalDeclarations << " " << name;
        emitGlaArraySize(globalDeclarations, arraySize);
        globalDeclarations << " = ";
        emitConstantInitializer(globalDeclarations, constant, constant->getType());
        globalDeclarations << ";" << std::endl;

        return;
    }

    // no initializer
    switch (qualifier) {
    default:
        UnsupportedFunctionality("variable declaration qualifier\n", EATContinue);
        // fall through
    case EVQGlobal:
        // Make sure we only declare globals once.
        if (globallyDeclared.find(name) != globallyDeclared.end())
            return;
        else
            globallyDeclared.insert(name);

        arraySize = emitGlaType(globalDeclarations, precision, qualifier, type);
        globalDeclarations << " " << name;
        emitGlaArraySize(globalDeclarations, arraySize);
        globalDeclarations << ";" << std::endl;
        break;
    case EVQTemporary:
        arraySize = emitGlaType(shader, precision, qualifier, type);
        emitGlaArraySize(shader, arraySize);
        shader << " ";
        break;
    case EVQUndef:
        arraySize = emitGlaType(globalDeclarations, precision, qualifier, type);
        globalDeclarations << " " << name;
        emitGlaArraySize(globalDeclarations, arraySize);
        globalDeclarations << ";" << std::endl;
        break;
    }
}

// Emits the type.  Done recursively, either directly or indirectly through addStructType().
// Returns the array size of the type.
int gla::GlslTarget::emitGlaType(std::ostringstream& out, EMdPrecision precision, EVariableQualifier qualifier, llvm::Type* type, 
                                 bool ioRoot, const llvm::MDNode* mdNode, int count, bool arrayChild)
{
    MetaType metaType;

    if (mdNode) {
        metaType.precision = precision;
        if (! decodeMdTypesEmitMdQualifiers(out, ioRoot, mdNode, type, arrayChild, metaType))
            return 0;
        precision = metaType.precision;
    }

    const char* qualifierString = MapGlaToQualifierString(version, stage, qualifier);
    if (*qualifierString)
        out << qualifierString << " ";

    if (type->getTypeID() == llvm::Type::PointerTyID)
        type = type->getContainedType(0);

    int arraySize = 0;

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
            if (metaType.notSigned)
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
        std::string structName = structType->isLiteral() ? "" : structType->getName();
        // If this is not a block, but a built-in name, then it must be for a shadow of a built-in block,
        // so should not have a gl_ name.
        if (! metaType.block)
            MakeNonbuiltinName(structName);
        MakeParseable(structName);
        addStructType(out, structName, structType, metaType.mdAggregate, metaType.block);
        if (! metaType.block) {
            if (metaType.mdAggregate)
                out << std::string(metaType.mdAggregate->getOperand(0)->getName());
            else
                out << structNameMap[structType];
        }
    } else if (type->getTypeID() == llvm::Type::ArrayTyID) {
        const llvm::ArrayType* arrayType = llvm::dyn_cast<const llvm::ArrayType>(type);
            
        if (metaType.matrix && arrayType->getNumContainedTypes() > 0 && arrayType->getContainedType(0)->isVectorTy()) {
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
            //
            // We need to recurse the next level with the LLVM type dereferenced, but not
            // the GLSL type (metadata) which combines arrayness at the same level as other typeness
            // (that is, don't pass on mdAggregate, use the original input mdNode).
            //
            // Also, set that we are an arrayChild, so that layouts/qualifiers can be emitted only once
            // for both levels of the type.  (I.e., structs have nested syntax but arrays don't.)
            emitGlaType(out, precision, EVQNone, arrayType->getContainedType(0), ioRoot, mdNode, -1, true);
            arraySize = (int)arrayType->getNumElements();
        }
    } else {
        // just output a scalar
        emitGlaPrecision(out, precision);
        if (metaType.mdSampler)
            emitGlaSamplerType(out, metaType.mdSampler);
        else {
            if (type == type->getFloatTy(type->getContext()))
                out << "float";
            else if (type == type->getInt1Ty(type->getContext()))
                out << "bool";
            else if (type == type->getInt32Ty(type->getContext())) {
                if (metaType.notSigned)
                    out << "uint";
                else
                    out << "int";
            } else if (type == type->getVoidTy(type->getContext()))
                out << "void";
            else
                UnsupportedFunctionality("Basic Type in Bottom IR");
        }
    }

    return arraySize;
}

// Process the mdNode, decoding all type information and emitting qualifiers.
// Returning false means there was a problem.
bool gla::GlslTarget::decodeMdTypesEmitMdQualifiers(std::ostringstream& out, bool ioRoot, const llvm::MDNode* mdNode, llvm::Type*& type, bool arrayChild, MetaType& metaType)
{
    EMdTypeLayout typeLayout;
    int location;

    if (ioRoot) {
        EMdInputOutput ioKind;
        llvm::Type* proxyType;
        int interpMode;
        if (! CrackIOMd(mdNode, metaType.name, ioKind, proxyType, typeLayout, metaType.precision, location, metaType.mdSampler, metaType.mdAggregate, interpMode)) {
            UnsupportedFunctionality("IO metadata for type");
            return false;
        }

        // See whether it's a block.
        switch (ioKind) {
        case EMioUniformBlockMember:
        case EMioBufferBlockMember:
        case EMioPipeInBlock:
        case EMioPipeOutBlock:
            metaType.block = true;
            break;
        default:
            metaType.block = false;
            break;
        }

        if (type == 0)
            type = proxyType;

        // emit interpolation qualifier, if appropriate
        EVariableQualifier qualifier;
        switch (ioKind) {
        case EMioPipeIn:   qualifier = EVQInput;   break;
        case EMioPipeOut:  qualifier = EVQOutput;  break;
        default:           qualifier = EVQUndef;   break;
        }
        if (qualifier != EVQUndef) {
            EInterpolationMethod interpMethod;
            EInterpolationLocation interpLocation;
            CrackInterpolationMode(interpMode, interpMethod, interpLocation);
            emitGlaInterpolationQualifier(qualifier, interpMethod, interpLocation);
        }
    } else {
        if (! CrackAggregateMd(mdNode, metaType.name, typeLayout, metaType.precision, location, metaType.mdSampler)) {
            UnsupportedFunctionality("aggregate metadata for type");
            return false;
        }
        metaType.mdAggregate = mdNode;
    }

    metaType.matrix = typeLayout == EMtlRowMajorMatrix || typeLayout == EMtlColMajorMatrix;
    metaType.notSigned = typeLayout == EMtlUnsigned;

    if (! arrayChild)
        emitGlaLayout(out, typeLayout, location, metaType.block || metaType.mdSampler != 0);

    return true;
}

void gla::GlslTarget::emitGlaArraySize(std::ostringstream& out, int arraySize)
{
    if (arraySize > 0)
        out << "[" << arraySize << "]";
}

void gla::GlslTarget::emitGlaSamplerType(std::ostringstream& out, const llvm::MDNode* mdSamplerNode)
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
        case EMsdRect:     out << "2DRect";  break;
        case EMsdBuffer:   out << "Buffer";  break;
        default:           UnsupportedFunctionality("kind of sampler");  break;
        }
        if (isArray)
            out << "Array";
        if (isShadow)
            out << "Shadow";
    } else
        UnsupportedFunctionality("sampler metadata", EATContinue);
}

void gla::GlslTarget::emitGlaInterpolationQualifier(EVariableQualifier qualifier, EInterpolationMethod interpMethod, EInterpolationLocation interpLocation)
{
    if (interpLocation != EILLast) {
        if (interpMethod != EIMNone) {
            switch (interpLocation) {
            case EILSample:        globalDeclarations << "sample ";        break;
            case EILCentroid:      globalDeclarations << "centroid ";      break;
            default:                                                       break;
            }
        }

        if (version >= 130) {
            if ((stage == EShLangVertex   && qualifier == EVQOutput) ||
                (stage == EShLangFragment && qualifier == EVQInput)) {
                switch (interpMethod) {
                case EIMNone:          globalDeclarations << "flat ";          break;
                //case EIMSmooth:        globalDeclarations << "smooth ";        break;
                case EIMNoperspective: globalDeclarations << "noperspective "; break;
                default:                                                       break;
                }
            } else {
                switch (interpMethod) {
                case EIMNone:                                                  break;
                case EIMPatch:         globalDeclarations << "patch ";         break;
                default:
                    UnsupportedFunctionality("unknown interpolation method", EATContinue);
                    break;
                }
            }
        }
    }
}

void gla::GlslTarget::emitGlaLayout(std::ostringstream& out, gla::EMdTypeLayout layout, int location, bool binding)
{
    const char* layoutStr = 0;

    switch (layout) {
    // members
    case EMtlRowMajorMatrix:  layoutStr = "row_major";   break;
    //case EMtlColMajorMatrix:  layoutStr = "col_major";   break;
    // non-block members will always be col_major, and are not allowed such layout declarations,
    // rely on that and the fact that we don't deal with overridden defaults at this level to omit col_major
        
    // blocks
    case EMtlStd140:  layoutStr = "std140";   break;
    case EMtlStd430:  layoutStr = "std430";   break;
    case EMtlPacked:  layoutStr = "std430";   break;

    default:  break;
    }

    int set = (unsigned)location >> 16;
    location &= 0xFFFF;

    // Unbias set, which was biased by 1 to distinguish between "set=0" and nothing.
    bool setPresent = (set != 0);
    if (setPresent)
        --set;

    if (! setPresent && location >= gla::MaxUserLayoutLocation && layoutStr == 0)
        return;

    // TODO: remove the following two lines when it is safe to correctly emit bindings
    if (binding && layoutStr == 0)
        return;

    bool comma = false;
    out << "layout(";
    if (layoutStr) {
        out << layoutStr;
        comma = true;
    }
    // TODO: remove this if test when it is safe to correctly emit bindings
    if (! binding) {
        if (location < gla::MaxUserLayoutLocation) {
            if (comma)
                out << ", ";
        
            if (binding) {
                if (setPresent)
                    out << "set=" << set << ",";
                out << "binding=";
            } else
                out << "location=";
            out << location;
            comma = true;
        }
    }
    out << ") ";
}

void gla::GlslTarget::emitGlaConstructor(std::ostringstream& out, llvm::Type* type, int count)
{
    int arraySize = emitGlaType(out, EMpNone, EVQNone, type, false, 0, count);
    emitGlaArraySize(out, arraySize);
}

// If valueMap has no entry for value, generate a name and declaration.
// If forceGlobal is true, then it will make the declaration occur as a global.
void gla::GlslTarget::emitGlaValueDeclaration(const llvm::Value* value, const char* rhs, bool forceGlobal)
{
    if (valueMap.find(value) != valueMap.end())
        return;

    // Figure out where our declaration should go
    EVariableQualifier evq;
    if (forceGlobal)
        evq = gla::EVQGlobal;
    else if (llvm::isa<llvm::PointerType>(value->getType()))
        evq = gla::EVQTemporary;
    else
        evq = MapGlaAddressSpace(value);

    const llvm::Constant* constant = llvm::dyn_cast<llvm::Constant>(value);
    if (constant) {
        emitNamelessConstDeclaration(value, constant);

        return;
    }

    std::string newName;
    makeNewVariableName(value, newName, rhs);
    mapVariableName(value, newName);
    EMdPrecision precision = GetPrecision(value);

    // Integer and floating-point constants have no precision, use highp to avoid precision loss across translations
    if (precision == gla::EMpNone && constant && profile == EEsProfile)
        if (! gla::IsBoolean(value->getType()))
            precision = gla::EMpHigh;

    if (const llvm::PointerType* pointerType = llvm::dyn_cast<llvm::PointerType>(value->getType())) {
        emitVariableDeclaration(precision, pointerType->getContainedType(0), newName, evq);
    } else {
        emitVariableDeclaration(precision, value->getType(), newName, evq, constant);        
    }
}

void gla::GlslTarget::emitGlaValue(std::ostringstream& out, const llvm::Value* value, const char* rhs)
{
    assert(! llvm::isa<llvm::ConstantExpr>(value));
    emitGlaValueDeclaration(value, rhs);
    out << valueMap[value]->c_str();
}

void gla::GlslTarget::emitGlaOperand(std::ostringstream& out, const llvm::Value* value)
{
    // If an operand needs a declaration, it can only be something declared elsewhere,
    // not in line here.
    emitGlaValueDeclaration(value, 0);
    out << valueMap[value]->c_str();
}

// Called when it is known safe to emit a name that has not been converted
// (converted from I/O types to internal types).
// If there is a non-converted version, emit it, otherwise just emit
// the normal one.
void gla::GlslTarget::emitNonconvertedGlaValue(std::ostringstream& out, const llvm::Value* value)
{
    if (nonConvertedMap.find(value) != nonConvertedMap.end())
        out << nonConvertedMap[value]->c_str();
    else
        emitGlaValue(out, value, 0);
}

// Propagate a nonconverted form from one value to another
void gla::GlslTarget::propagateNonconvertedGlaValue(const llvm::Value* dst, const llvm::Value* src)
{
    if (nonConvertedMap.find(src) != nonConvertedMap.end())
        nonConvertedMap[dst] = new std::string(*nonConvertedMap[src]);  // rare enough to not worry about sharing the pointer (they all get deleted)
}

std::string* gla::GlslTarget::mapGlaValueAndEmitDeclaration(const llvm::Value* value)
{
    emitGlaValueDeclaration(value, 0);

    return valueMap[value];
}

void gla::GlslTarget::emitFloatConstant(std::ostringstream& out, float f)
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
void gla::GlslTarget::emitConstantInitializer(std::ostringstream& out, const llvm::Constant* constant, llvm::Type* type)
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
            // ConstantDataSequential handles both ConstantDataVector and ConstantDataArray,
            // except getSplatValue(), which is present for ConstantDataSequential,
            // only actually works for ConstantDataVector.
            const llvm::ConstantDataSequential* dataSequential = 0;
            if (! isZero && constant) {
                dataSequential = llvm::dyn_cast<llvm::ConstantDataSequential>(constant);
                if (dataSequential && llvm::isa<llvm::ConstantDataVector>(dataSequential))
                    splatValue = dataSequential->getSplatValue();
            }

            if (const llvm::VectorType* vectorType = llvm::dyn_cast<llvm::VectorType>(type)) {
                if (! isZero && ! llvm::isa<llvm::ConstantDataVector>(constant)) {
                    // If all vector elements are equal, we only need to emit one
                    bool same = true;
                    for (int op = 1; op < (int)vectorType->getNumElements(); ++op) {
                        if (llvm::dyn_cast<const llvm::Constant>(constant->getOperand(0)) != llvm::dyn_cast<const llvm::Constant>(constant->getOperand(op))) {
                            same = false;
                            break;
                        }
                    }

                    if (same)
                        splatValue = llvm::dyn_cast<llvm::Constant>(constant->getOperand(0));
                }
                numElements = vectorType->getNumElements();
            } else if (const llvm::ArrayType*  arrayType = llvm::dyn_cast<llvm::ArrayType>(type)) {
                numElements = (int)arrayType->getNumElements();
            } else if (const llvm::StructType* structType = llvm::dyn_cast<llvm::StructType>(type))
                numElements = (int)structType->getNumElements();
            else
                assert(0 && "Constant aggregate type");

            if (isZero || splatValue)
                emitConstantInitializer(out, isZero ? 0 : splatValue, type->getContainedType(0));
            else {
                for (int op = 0; op < numElements; ++op) {
                    if (op > 0)
                        out << ", ";

                    const llvm::Constant* constElement;
                    if (dataSequential)
                        constElement = dataSequential->getElementAsConstant(op);
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
        break;
    }
}

void gla::GlslTarget::emitInitializeAggregate(std::ostringstream& out, std::string name, const llvm::Constant* constant)
{
    if (constant && IsDefined(constant) && ! IsScalar(constant) && ! AreAllDefined(constant)) {
        // For a vector or array with undefined elements, propagate the defined elements
        if (const llvm::ConstantVector* constVec = llvm::dyn_cast<llvm::ConstantVector>(constant)) {
            for (int op = 0; op < (int)constVec->getNumOperands(); ++op) {
                if (IsDefined(constVec->getOperand(op))) {
                    out << std::endl << indentString << name;
                    out << "." << MapComponentToSwizzleChar(op) << " = ";
                    out << *mapGlaValueAndEmitDeclaration(constVec->getOperand(op));
                    out << ";";
                }
            }
        } else if (const llvm::ConstantArray* constArray = llvm::dyn_cast<llvm::ConstantArray>(constant)) {
            for (int op = 0; op < (int)constArray->getNumOperands(); ++op) {
                if (IsDefined(constArray->getOperand(op))) {
                    out << std::endl << indentString << name;
                    out << "[" << op << "] = ";
                    out << *mapGlaValueAndEmitDeclaration(constArray->getOperand(op));
                    out << ";";
                }
            }
        } else if (const llvm::ConstantStruct* constStruct = llvm::dyn_cast<llvm::ConstantStruct>(constant)) {
            for (int op = 0; op < (int)constStruct->getNumOperands(); ++op) {
                if (IsDefined(constStruct->getOperand(op))) {
                    out << std::endl << indentString << name;
                    out << "." << MapGlaStructField(constant->getType(), op) << " = ";
                    out << *mapGlaValueAndEmitDeclaration(constStruct->getOperand(op));
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

void gla::GlslTarget::emitGlaSwizzle(std::ostringstream& out, int glaSwizzle, int width, llvm::Value* source)
{
    if (source && gla::IsScalar(source))
        return;

    out << ".";
    // Pull each two bit channel out of the integer
    for(int i = 0; i < width; i++)
        emitComponentToSwizzle(out, GetSwizzle(glaSwizzle, i));
}

// Emit the swizzle represented by the vector of channel selections
void gla::GlslTarget::emitGlaSwizzle(std::ostringstream& out, const llvm::SmallVectorImpl<llvm::Constant*>& elts)
{
    out << ".";

    // Output the components for all defined channels
    for (int i = 0; i < (int)elts.size(); ++i) {
        if (! IsDefined(elts[i]))
            continue;

        emitComponentToSwizzle(out, GetConstantInt(elts[i]));
    }
}

// Emit a writemask. Emits a component for each defined element of the
// passed vector.
void gla::GlslTarget::emitGlaWriteMask(std::ostringstream& out, const llvm::SmallVectorImpl<llvm::Constant*>& elts)
{
    out << ".";

    // Output the components for all defined channels
    for (int i = 0; i < (int)elts.size(); ++i) {
        if (! IsDefined(elts[i]))
            continue;
        emitComponentToSwizzle(out, i);
    }
}

// Returns the number of defined components.
int gla::GlslTarget::getDefinedCount(const llvm::SmallVectorImpl<llvm::Constant*>& elts)
{
    int definedCount = 0;
    for (int i = 0; i < (int)elts.size(); ++i) {
        if (! IsDefined(elts[i]))
            continue;
        ++definedCount;
    }

    return definedCount;
}

// Writes out the vector arguments for the RHS of a multiInsert. Sets its
// first argument to false upon first execution
void gla::GlslTarget::emitVectorArguments(std::ostringstream& out, bool &firstArg, const llvm::IntrinsicInst *inst, int operand)
{
    if (firstArg)
        firstArg = false;
    else
        out << ", ";

    emitGlaOperand(out, inst->getOperand(operand));

    // If it's a vector, extract the value
    if (inst->getOperand(operand)->getType()->getTypeID() == llvm::Type::VectorTyID) {
        out << ".";
        emitComponentToSwizzle(out, GetConstantInt(inst->getOperand(operand+1)));
    }
}

void gla::GlslTarget::emitGlaMultiInsertRHS(std::ostringstream& out, const llvm::IntrinsicInst* inst)
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
    llvm::Value* source = GetCommonSourceMultiInsert(inst);
    if (source) {
        emitGlaOperand(out, source);

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
        emitGlaSwizzle(out, singleSourceMask, argCount, source);
    } else {
        emitGlaConstructor(out, inst->getType(), argCount);
        out << "(";
        bool firstArg = true;

        for (int i = 0; i < 4; ++i) {
            if (wmask & (1 << i)) {
                int operandIndex = (i+1) * 2;
                emitVectorArguments(out, firstArg, inst, operandIndex);
            }
        }

        out << ")";
    }
}

void gla::GlslTarget::emitGlaMultiInsert(std::ostringstream& out, const llvm::IntrinsicInst* inst)
{
    int wmask = GetConstantInt(inst->getOperand(1));

    Assignment initExpression(this, inst);

    // If the writemask is full, then just initialize it, and we're done.
    // Will make a set of contiguous 1s by subracting 1 from a power of 2.
    int comps = GetComponentCount(inst);
    if (wmask == (1 << comps) - 1) {
        emitGlaMultiInsertRHS(initExpression, inst);
        initExpression.emit();
        return;
    }

    // If the origin is defined, initialize the new instruction to be the
    // origin. If undefined, leave it declared and uninitialized.
    if (IsDefined(inst->getOperand(0)))
        emitGlaOperand(initExpression, inst->getOperand(0));
    initExpression.emit();

    Assignment expression(this, inst);

    // Add the lhs swizzle    
    std::ostringstream lhsSwizzle;
    emitMaskToSwizzle(lhsSwizzle, wmask);
    expression.setMember(lhsSwizzle.str().c_str());

    // Add the rhs swizzle
    emitGlaMultiInsertRHS(expression, inst);
    
    expression.emit();
}

void gla::GlslTarget::emitMapGlaIOIntrinsic(const llvm::IntrinsicInst* llvmInstruction, bool input)
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

    int interpMode;
    if (! mdNode || ! gla::CrackIOMd(mdNode, name, mdQual, type, mdLayout, mdPrecision, layoutLocation, dummySampler, mdAggregate, interpMode)) {
        // This path should not exist; it is a backup path for missing metadata.
        UnsupportedFunctionality("couldn't get metadata for input instruction", EATContinue);

        // emulate (through imperfect guessing) the missing metadata
        name = llvmInstruction->getName();
        type = llvmInstruction->getType();
        mdLayout = EMtlNone;
        mdPrecision = EMpNone;
        layoutLocation = gla::MaxUserLayoutLocation;
        mdAggregate = 0;
    }

    // add the dereference syntax
    // TODO: output code correctness: outputs don't yet have layout slot bases, so indexing into big things will be incorrect
    std::string derefName = name;
    int slotOffset = GetConstantInt(llvmInstruction->getOperand(0)) - layoutLocation;
    DereferenceName(derefName, type, mdAggregate, slotOffset, mdLayout);
    bool notSigned = mdLayout == EMtlUnsigned;

    switch (llvmInstruction->getIntrinsicID()) {
    case llvm::Intrinsic::gla_writeData:
    case llvm::Intrinsic::gla_fWriteData:
    {
        llvm::Value* value = llvmInstruction->getOperand(2);
        emitGlaValueDeclaration(value, 0);
        std::string wrapped = *valueMap[value];
        if (notSigned)
            ConversionWrap(wrapped, value->getType(), true);
        newLine();
        shader << derefName << " = " << wrapped << ";";
        break;
    }
    case llvm::Intrinsic::gla_readData:
    case llvm::Intrinsic::gla_fReadData:
    case llvm::Intrinsic::gla_fReadInterpolant:
        // A pipeline read only creates the mapping to the name, not the use.
        // The use will come later by looking up the mapping between Value* and
        // the name associated with it.
        //
        // NOTE!  The mapped name will include the deferences, like "foo[3].v", *in the name*.
            
        // Add an int-based constructor around it, if needed.
        // It will never be a matrix, we dereferenced down to a slot already
        if (notSigned)
            ConversionWrap(derefName, llvmInstruction->getType(), false);

        mapExpressionString(llvmInstruction, derefName);
        break;
    default:
        UnsupportedFunctionality("IO Intrinsic");
        break;
    }
}

void gla::GlslTarget::emitInvariantDeclarations(llvm::Module& module)
{
    const llvm::NamedMDNode* mdList = module.getNamedMetadata(gla::InvariantListMdName);

    // first, see if we have any
    bool found = false;
    if (mdList && mdList->getNumOperands() > 0) {
        for (unsigned int m = 0; m < mdList->getNumOperands(); ++m) {
            if (! filteringIoNode(mdList->getOperand(m))) {
                found = true;
                break;
            }
        }
    }
    if (! found)
        return;

    // we've got some; declare them...
    globalDeclarations << "invariant ";
    for (unsigned int m = 0; m < mdList->getNumOperands(); ++m) {
        const llvm::MDNode* mdNode = mdList->getOperand(m);
        if (! filteringIoNode(mdNode))
            globalDeclarations << mdNode->getOperand(0)->getName().str().c_str() << " ";
    }
    globalDeclarations << ";";
}

// Traverse the indices used in either GEP or Insert/ExtractValue, and return a string representing
// it, not including the base.
std::string gla::GlslTarget::traverseGep(const llvm::Instruction* instr, EMdTypeLayout* mdTypeLayout)
{
    std::string gepName;

    // Figure out any metadata description we have for this type of aggregate
    llvm::Type* aggregateType = instr->getOperand(0)->getType();
    while (aggregateType->getTypeID() == llvm::Type::PointerTyID || aggregateType->getTypeID() == llvm::Type::ArrayTyID)
        aggregateType = aggregateType->getContainedType(0);
    const llvm::MDNode* mdAggregate = 0;
    if (UseLogicalIO || shadowingBlock.find(aggregateType) == shadowingBlock.end())
        mdAggregate = typeMdAggregateMap[aggregateType];

    if (const llvm::GetElementPtrInst* gepInst = llvm::dyn_cast<llvm::GetElementPtrInst>(instr)) {
        // Start at operand 2 since indices 0 and 1 give you the base and are handled before traverseGep
        const llvm::Type* gepType = gepInst->getPointerOperandType()->getContainedType(0);
        for (unsigned int op = 2; op < gepInst->getNumOperands(); ++op)
            dereferenceGep(gepType, gepName, gepInst->getOperand(op), -1, mdAggregate, mdTypeLayout);

    } else if (const llvm::InsertValueInst* insertValueInst = llvm::dyn_cast<const llvm::InsertValueInst>(instr)) {
        const llvm::Type* gepType = insertValueInst->getAggregateOperand()->getType();            
        for (llvm::InsertValueInst::idx_iterator iter = insertValueInst->idx_begin(), end = insertValueInst->idx_end();  iter != end; ++iter)
            dereferenceGep(gepType, gepName, 0, *iter, mdAggregate);

    } else if (const llvm::ExtractValueInst* extractValueInst = llvm::dyn_cast<const llvm::ExtractValueInst>(instr)) {
        const llvm::Type* gepType = extractValueInst->getAggregateOperand()->getType();  
        for (llvm::ExtractValueInst::idx_iterator iter = extractValueInst->idx_begin(), end = extractValueInst->idx_end();  iter != end; ++iter)
            dereferenceGep(gepType, gepName, 0, *iter, mdAggregate);

    } else {
        assert(0 && "non-GEP in traverseGEP");
    }

    return gepName;
}

// Traverse one step of a dereference chain and append to a string
// For constant indices, pass it in index.  Otherwise, provide it through 'operand' (index will not be used, unless 'operand' itself is a constant)
void gla::GlslTarget::dereferenceGep(const llvm::Type*& type, std::string& name, llvm::Value* operand, int index, const llvm::MDNode*& mdAggregate, EMdTypeLayout* mdTypeLayout)
{
    if (operand) {
        if (llvm::isa<llvm::UndefValue>(operand)) {
            // bad index (from bad shader), substitute 0 for the index
            index = 0;
            operand = 0;
        } else if (llvm::isa<const llvm::ConstantInt>(operand)) {
            // substitute the actual constant
            index = GetConstantInt(operand);
            operand = 0;
        } else
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
            name.append(*mapGlaValueAndEmitDeclaration(operand));

        name.append("]");

        type = type->getContainedType(0);
        break;
    case llvm::Type::StructTyID:
        assert(index >= 0);

        name.append(".");
        name.append(MapGlaStructField(type, index, mdAggregate));

        // Deference the metadata aggregate 
        if (mdAggregate) {
            int aggOp = GetAggregateMdSubAggregateOp(index);
            if ((int)mdAggregate->getNumOperands() <= aggOp) {
                UnsupportedFunctionality("not enough mdAggregate operands", EATContinue);
                mdAggregate = 0;
            } else 
                mdAggregate = llvm::dyn_cast<llvm::MDNode>(mdAggregate->getOperand(aggOp));

            if (mdAggregate && mdTypeLayout)
                *mdTypeLayout = GetMdTypeLayout(mdAggregate);
        }

        type = type->getContainedType(index);
        break;
    case llvm::Type::VectorTyID:
        if (index >= 0) {
            name.append(".");
            const size_t bufSize = 10;
            char buf[bufSize];
            snprintf(buf, bufSize, "%c", "xyzw"[index]);
            name.append(buf);
        } else {
            name.append("[");
            name.append(*mapGlaValueAndEmitDeclaration(operand));
            name.append("]");
        }
        break;
    default:
        assert(0 && "Dereferencing non array/struct");
        break;
    }
}

// See if an expression string does not contain any operations
// that would be expensive to replicate, to aid in identifying what
// can be forward substituted.
bool gla::GlslTarget::cheapExpression(const std::string& expression)
{
    int c = expression[0];

    // Skip over unary stuff...
    int pos;
    for (pos = 0; pos < (int)expression.size(); ++pos) {
        int c = expression[pos];
        bool breakLoop = false;
        switch (c) {
        case '+':
        case '-':
        case '(':
        case '_':
            break;
        default:
            breakLoop = true;
            break;
        }
        if (breakLoop)
            break;
    }

    int startPos;
    do {
        startPos = pos;

        // Skip over name
        for (; pos < (int)expression.size(); ++pos) {
            int c = expression[pos];
            if (ValidIdentChar(c))
                continue;
            else
                break;
        }

        // Skip over indexes/members/etc., but not more opening expressions (constructors, etc.)
        for (; pos < (int)expression.size(); ++pos) {
            int c = expression[pos];
            if (c == '.' || c == '[' || c == ']' || c == ')' ||
                (c > '0' && c < '9'))
                continue;
            else
                break;
        }
    } while (pos > startPos);

    return pos == expression.size();
}

// Heuristically decide if this is something that should be 
// substituted, based on amount of usage in the current block and 
// in other blocks.
bool gla::GlslTarget::shouldSubstitute(const llvm::Instruction* instruction)
{
    const llvm::BasicBlock* bb = instruction->getParent();

    int usesInSameBlock = 0;
    int usesInDifferentBlock = 0;
    int usesInPhi = 0;
    int otherUses = 0;
    for (llvm::Value::const_use_iterator it = instruction->use_begin(); it != instruction->use_end(); ++it) {
        const llvm::Instruction* use = llvm::dyn_cast<const llvm::Instruction>(*it);
        if (use) {
            if (llvm::isa<llvm::PHINode>(use))
                ++usesInPhi;
            else {
                if (use->getParent() == bb)
                    ++usesInSameBlock;
                else
                    ++usesInDifferentBlock;
            }
        } else
            ++otherUses;
    }

    return usesInDifferentBlock == 0 && otherUses == 0 && usesInSameBlock <= 1;
}

// Of the set of ES precision qualifiers present, they must match the instruction's,
// otherwise, the instruction modifies precision that would be deduced from the operands.
bool gla::GlslTarget::modifiesPrecision(const llvm::Instruction* instruction)
{
    EMdPrecision precision = EMpNone;

    EMdPrecision instPrec;
    CrackPrecisionMd(instruction, instPrec);
    if (instPrec == EMpNone)
        return false;

    for (int op = 0; op < (int)instruction->getNumOperands(); ++op) {
        const llvm::Instruction* opInst = llvm::dyn_cast<const llvm::Instruction>(instruction->getOperand(op));
        if (! opInst)
            return true;
        EMdPrecision opPrec;
        CrackPrecisionMd(opInst, opPrec);
        if (opPrec != EMpNone && opPrec != instPrec)
            return true;
    }

    return false;
}

// See if target is an alloca instruction that only has one store to it,
// or store through a GEP to it.  This means it could be treated as an
// alias for the one thing that is stored to it.
bool gla::GlslTarget::writeOnceAlloca(const llvm::Value* target)
{
    const llvm::Instruction* targetInst = llvm::dyn_cast<const llvm::Instruction>(target);
    if (! targetInst || targetInst->getOpcode() != llvm::Instruction::Alloca)
        return false;

    const llvm::AllocaInst* alloca = llvm::dyn_cast<const llvm::AllocaInst>(targetInst);

    int stores = 0;
    for (llvm::Value::const_use_iterator it = alloca->use_begin(); it != alloca->use_end(); ++it) {
        const llvm::Instruction* use = llvm::dyn_cast<const llvm::Instruction>(*it);

        // Check basic store use
        if (use && use->getOpcode() == llvm::Instruction::Store)
            ++stores;
        if (stores > 1)
            return false;

        // Check out GEP use too
        if (use && use->getOpcode() == llvm::Instruction::GetElementPtr) {
            for (llvm::Value::const_use_iterator gepIt = use->use_begin(); gepIt != use->use_end(); ++gepIt) {
                const llvm::Instruction* gepUse = llvm::dyn_cast<const llvm::Instruction>(*gepIt);
                if (gepUse && gepUse->getOpcode() == llvm::Instruction::Store)
                    ++stores;
            }
        }
        if (stores > 1)
            return false;
    }

    return true;
}
