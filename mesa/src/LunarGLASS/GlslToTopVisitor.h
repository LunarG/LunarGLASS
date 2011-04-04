//===- GlslToTopVisitor.h - Header for GlslToTopVisitor.cpp ---------------===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright © 2011, LunarG, Inc.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice (including the next
// paragraph) shall be included in all copies or substantial portions of the
// Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//
//===----------------------------------------------------------------------===//
//
// Author:  John Kessenich, LunarG
// Author:  Cody Northrop, LunarG
//
//===----------------------------------------------------------------------===//

// LLVM includes
#include "llvm/DerivedTypes.h"
#include "llvm/Intrinsics.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/IRBuilder.h"
#include <cstdio>
#include <string>
#include <map>
#include <list>
#include <vector>

#include "ir.h"
#include "ir_hierarchical_visitor.h"
#include "glsl_types.h"
#include "glsl_parser_extras.h"

#include "LunarGLASSTopIR.h"
#include "LunarGLASSLlvmInterface.h"

void GlslToTop(struct gl_shader*, llvm::Module*);

//
// LunarGLASS implementation of abstract base class from
// ir_hierarchical_visitor.h.
//
class GlslToTopVisitor : public ir_hierarchical_visitor {
public:
    GlslToTopVisitor(struct gl_shader*, llvm::Module*);

    virtual ~GlslToTopVisitor();

	virtual ir_visitor_status visit(class ir_variable *);
	virtual ir_visitor_status visit(class ir_constant *);
	virtual ir_visitor_status visit(class ir_loop_jump *);
	virtual ir_visitor_status visit(class ir_dereference_variable *);
	virtual ir_visitor_status visit_enter(class ir_loop *);
	virtual ir_visitor_status visit_leave(class ir_loop *);
	virtual ir_visitor_status visit_enter(class ir_function_signature *);
	virtual ir_visitor_status visit_leave(class ir_function_signature *);
	virtual ir_visitor_status visit_enter(class ir_function *);
	virtual ir_visitor_status visit_leave(class ir_function *);
	virtual ir_visitor_status visit_enter(class ir_expression *);
	virtual ir_visitor_status visit_leave(class ir_expression *);
	virtual ir_visitor_status visit_enter(class ir_texture *);
	virtual ir_visitor_status visit_leave(class ir_texture *);
	virtual ir_visitor_status visit_enter(class ir_swizzle *);
	virtual ir_visitor_status visit_leave(class ir_swizzle *);
	virtual ir_visitor_status visit_enter(class ir_dereference_array *);
	virtual ir_visitor_status visit_leave(class ir_dereference_array *);
	virtual ir_visitor_status visit_enter(class ir_dereference_record *);
	virtual ir_visitor_status visit_leave(class ir_dereference_record *);
	virtual ir_visitor_status visit_enter(class ir_assignment *);
	virtual ir_visitor_status visit_leave(class ir_assignment *);
	virtual ir_visitor_status visit_enter(class ir_call *);
	virtual ir_visitor_status visit_leave(class ir_call *);
	virtual ir_visitor_status visit_enter(class ir_return *);
	virtual ir_visitor_status visit_leave(class ir_return *);
	virtual ir_visitor_status visit_enter(class ir_discard *);
	virtual ir_visitor_status visit_leave(class ir_discard *);
	virtual ir_visitor_status visit_enter(class ir_if *);
	virtual ir_visitor_status visit_leave(class ir_if *);

    // help functions to build LLVM
    llvm::Value* createLLVMVariable(ir_variable*);
    const char* getSamplerDeclaration(ir_variable*);
    gla::Builder::SuperValue expandGLSLOp(ir_expression_operation, gla::Builder::SuperValue*);
    llvm::Value* expandGLSLSwizzle(ir_swizzle*);
    llvm::Value* createLLVMIntrinsic(ir_call*, gla::Builder::SuperValue*, int);
    llvm::Value* createPipelineRead(ir_variable*, int);
    llvm::Constant* createLLVMConstant(ir_constant*);
    llvm::Type*  convertGLSLToLLVMType(const glsl_type*);
    llvm::Function* getLLVMIntrinsicFunction1(llvm::Intrinsic::ID, const llvm::Type*);
    llvm::Function* getLLVMIntrinsicFunction2(llvm::Intrinsic::ID, const llvm::Type*, const llvm::Type*);
    llvm::Function* getLLVMIntrinsicFunction3(llvm::Intrinsic::ID, const llvm::Type*, const llvm::Type*, const llvm::Type*);
    llvm::Function* getLLVMIntrinsicFunction4(llvm::Intrinsic::ID, const llvm::Type*, const llvm::Type*, const llvm::Type*, const llvm::Type*);

    void createLLVMTextureIntrinsic(llvm::Function* &, int &, gla::Builder::SuperValue*, gla::Builder::SuperValue*, llvm::Type*, llvm::Intrinsic::ID,  gla::ESamplerType, gla::ETextureFlags);
    void findAndSmearScalars(gla::Builder::SuperValue*, int);
    void writePipelineOuts(void);
    void appendArrayIndexToName(std::string &, int);

    llvm::Type::TypeID getLLVMBaseType(llvm::Value*);
    llvm::Type::TypeID getLLVMBaseType(const llvm::Type*);

    int getNextInterpIndex(std::string);

protected:    
    llvm::BasicBlock* getShaderEntry();

    llvm::LLVMContext &context;
    llvm::IRBuilder<> builder;
    llvm::Module* module;

    struct gl_shader* glShader;

    std::map<ir_variable*, llvm::Value*> namedValues;
    std::map<std::string, int> interpMap;
    std::map<ir_function_signature *, llvm::Function*> functionMap;

    std::list<llvm::Value*> glslOuts;

    gla::Builder::SuperValue lastValue;
    gla::Builder::SuperValue lValue;

    int interpIndex;
    bool inMain;
    bool localScope;

    llvm::BasicBlock* shaderEntry;
};
