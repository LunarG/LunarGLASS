//===- GlslToTopVisitor.h - Header for GlslToTopVisitor.cpp ---------------===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright � 2011, LunarG, Inc.
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
#include <stack>

#include "ir.h"
#include "ir_hierarchical_visitor.h"
#include "glsl_types.h"
#include "glsl_parser_extras.h"

#include "LunarGLASSTopIR.h"
#include "LunarGLASSManager.h"
#include "Util.h"
#include "TopBuilder.h"

void GlslToTop(struct gl_shader*, gla::Manager*);

//
// LunarGLASS implementation of abstract base class from
// ir_hierarchical_visitor.h.
//
class GlslToTopVisitor : public ir_hierarchical_visitor {
public:
    GlslToTopVisitor(struct gl_shader*, gla::Manager*);

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

protected:
    // help functions to build LLVM
    gla::Builder::SuperValue createLLVMVariable(ir_variable*);
    const char* getSamplerTypeName(ir_variable*);
    gla::Builder::SuperValue createUnaryMatrixOperation(ir_expression_operation, gla::Builder::SuperValue);
    gla::Builder::SuperValue createBinaryMatrixOperation(ir_expression_operation, gla::Builder::SuperValue, gla::Builder::SuperValue);
    gla::Builder::SuperValue createUnaryOperation(ir_expression_operation, const glsl_type*, gla::Builder::SuperValue, bool isFloat, bool isSigned);
    gla::Builder::SuperValue createBinaryOperation(ir_expression_operation, gla::Builder::SuperValue, gla::Builder::SuperValue, bool isFloat, bool isSigned);
    gla::Builder::SuperValue createUnaryIntrinsic(ir_expression_operation, gla::Builder::SuperValue, bool isFloat, bool isSigned);
    gla::Builder::SuperValue createBinaryIntrinsic(ir_expression_operation, gla::Builder::SuperValue, gla::Builder::SuperValue, bool isFloat, bool isSigned);
    llvm::Value* expandGLSLSwizzle(ir_swizzle*);
    llvm::Value* createLLVMIntrinsic(ir_call*, gla::Builder::SuperValue*, int);
    llvm::Value* createPipelineRead(ir_variable*, int);
    llvm::Value* collapseIndexChain(llvm::Value*);
    llvm::Constant* createLLVMConstant(ir_constant*);
    const llvm::Type* convertGlslToGlaType(const glsl_type*);
    llvm::Function* getLLVMIntrinsicFunction1(llvm::Intrinsic::ID, const llvm::Type*);
    llvm::Function* getLLVMIntrinsicFunction2(llvm::Intrinsic::ID, const llvm::Type*, const llvm::Type*);
    llvm::Function* getLLVMIntrinsicFunction3(llvm::Intrinsic::ID, const llvm::Type*, const llvm::Type*, const llvm::Type*);
    llvm::Function* getLLVMIntrinsicFunction4(llvm::Intrinsic::ID, const llvm::Type*, const llvm::Type*, const llvm::Type*, const llvm::Type*);

    void writePipelineOuts(void);
    void appendArrayIndexToName(std::string &, int);
    bool convertValuesToUnsigned(unsigned*, int &, std::vector<llvm::Value*>);

    int getNextInterpIndex(std::string);

    llvm::LLVMContext &context;
    llvm::IRBuilder<> llvmBuilder;
    llvm::Module* module;

    struct gl_shader* glShader;

    std::map<ir_variable*, gla::Builder::SuperValue> namedValues;
    std::map<std::string, int> interpMap;
    std::map<std::string, llvm::StructType*> structMap;
    std::map<ir_function_signature *, llvm::Function*> functionMap;

    std::stack<std::vector<llvm::Value*> > gepIndexChainStack;

    gla::Builder::SuperValue lastValue;

    struct {
        gla::Builder::SuperValue base;
        std::vector<llvm::Value*> indexChain;
    } lValue;

    int interpIndex;
    bool inMain;
    bool localScope;

    // Used to init arrays of constant indices for ExtractValue/InsertValue
    static const int maxGepIndices = 16;

    llvm::BasicBlock* shaderEntry;

    gla::Builder* glaBuilder;

    // In non-linking IR, the front end puts the increment inside the body of
    // the loop. In linking IR, the increment inside the body of the loop does
    // not have the same address as the declaration in the loop header, thus we
    // have to insert it ourselves. Adding the increment in the case of
    // non-linking IR is erroneous, and not adding it in the case of linking IR
    // is also erroneous.
    static const bool haveBuilderIncrementInductiveVariable = true;
};
