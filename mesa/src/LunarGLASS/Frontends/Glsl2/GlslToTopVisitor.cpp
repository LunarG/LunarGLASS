//===- GlslToTopVisitor.cpp - Help Translate GLSL IR to LunarGLASS Top IR -===//
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
// Author: John Kessenich, LunarG
// Author: Cody Northrop, LunarG
//
//===----------------------------------------------------------------------===//

#include "GlslToTopVisitor.h"
#include "LunarGLASSTopIR.h"
#include "Util.h"
#include "mtypes.h"
#include "Exceptions.h"
#include "Options.h"

#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"


void GlslToTop(struct gl_shader* glShader, llvm::Module* module)
{
    GlslToTopVisitor v(glShader, module);

    foreach_iter(exec_list_iterator, iter, *glShader->ir) {
        ir_instruction *ir = (ir_instruction *)iter.get();

        ir->accept(&v);
    }
}

GlslToTopVisitor::GlslToTopVisitor(struct gl_shader* s, llvm::Module* m)
    : context(llvm::getGlobalContext()), llvmBuilder(context), module(m), glShader(s), interpIndex(0), shaderEntry(0), inMain(false),
      localScope(false)
{
    // Init the first GEP index chain
    std::vector<llvm::Value*> firstChain;
    gepIndexChainStack.push(firstChain);

    // do this after the builder knows the module
    glaBuilder = new gla::Builder(llvmBuilder, module);

    std::vector<const llvm::Type*> mainParams;
    glaBuilder->makeFunctionEntry(gla::GetVoidType(context), "main", mainParams, shaderEntry);
    llvmBuilder.SetInsertPoint(shaderEntry);
}

GlslToTopVisitor::~GlslToTopVisitor()
{
    delete glaBuilder;
}

ir_visitor_status
    GlslToTopVisitor::visit(ir_variable *variable)
{
    // If it's an auto or temporary, we allocate it now, at declaration time
    // to get the scoping correct.
    //
    // Otherwise, we do deferred evaluation, so that we don't deal with things
    // that are never used.

    if (variable->mode == ir_var_auto || variable->mode == ir_var_temporary)
        namedValues[variable] = createLLVMVariable(variable);

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit(ir_constant *constant)
{
    lastValue = createLLVMConstant(constant);

    return visit_continue;
}

llvm::Constant* GlslToTopVisitor::createLLVMConstant(ir_constant* constant)
{
    if (constant->type->matrix_columns > 1)
        gla::UnsupportedFunctionality("Matrix constants");

    // vector of constants for LLVM
    std::vector<llvm::Constant*> vals;

    if (constant->type->vector_elements) {
        for (unsigned int i = 0; i < constant->type->vector_elements; ++i) {
            switch(constant->type->base_type)
            {
            case GLSL_TYPE_UINT:
                vals.push_back(gla::MakeUnsignedConstant(context, constant->value.i[i]));
                break;
            case GLSL_TYPE_INT:
                vals.push_back(gla::MakeIntConstant(context, constant->value.u[i]));
                break;
            case GLSL_TYPE_FLOAT:
                vals.push_back(gla::MakeFloatConstant(context, constant->value.f[i]));
                break;
            case GLSL_TYPE_BOOL:
                vals.push_back(gla::MakeBoolConstant(context, constant->value.i[i]));
                break;
            }
        }
    } else {
        switch(constant->type->base_type)
        {
        case GLSL_TYPE_ARRAY:
        case GLSL_TYPE_STRUCT:
            gla::UnsupportedFunctionality("array or struct constant");
            break;

        case GLSL_TYPE_SAMPLER:
        case GLSL_TYPE_VOID:
        case GLSL_TYPE_ERROR:
            assert(! "Bad vector constant type");
            break;
        }
    }

    return glaBuilder->getConstant(vals);
}

// TODO: abstract into TopBuilder

// If the loop is an inductive loop (to the front-end), set up the increment and
// the condition test. Returns whether it did anything. For now, the front end inserts the increment into the
// loop body, so don't do that yet.
bool GlslToTopVisitor::setUpLatch()
{
    LoopData ld = loops.top();

    // TODO: do the test outside this function

    if (ld.isInductive) {
        assert(ld.counter && ld.counter->getType()->isPointerTy() && ld.increment && ld.finish);

        llvm::Value* iPrev = llvmBuilder.CreateLoad(ld.counter);
        llvm::Value* cmp   = NULL;

        // TODO: add in bool for when a front end increments it itself

        // In non-linking IR, the front end puts the increment inside the body of the
        // loop. In linking IR, the increment inside the body of the loop does
        // not have the same address as the declaration in the loop header, thus
        // we have to insert it ourselves. Adding the increment in the case of
        // non-linking IR is erroneous, and not adding it in the case of
        // linking IR is also erroneous.
        // TODO: Resolve this problem

        llvm::Value* iNext = NULL;
        switch(ld.counter->getType()->getContainedType(0)->getTypeID()) {
        case llvm::Type::FloatTyID:       iNext = llvmBuilder.CreateFAdd(iPrev, ld.increment);     break;
        case llvm::Type::IntegerTyID:     iNext = llvmBuilder.CreateAdd( iPrev, ld.increment);     break;

        default: gla::UnsupportedFunctionality("unknown type in inductive variable");
        }

        llvmBuilder.CreateStore(iNext, ld.counter);

        // TODO: If we do the increment ourselves, change iPrev to be iNext
        switch(ld.counter->getType()->getContainedType(0)->getTypeID()) {
        case llvm::Type::FloatTyID:       cmp = llvmBuilder.CreateFCmpOGE(iPrev, ld.finish);     break;
        case llvm::Type::IntegerTyID:     cmp = llvmBuilder.CreateICmpSGE(iPrev, ld.finish);     break;

        default: gla::UnsupportedFunctionality("unknown type in inductive variable [2]");
        }

        // If iNext exceeds ld.finish, exit the loop, else branch back to
        // the header
        llvmBuilder.CreateCondBr(cmp, ld.exit, ld.header);

        // Not going to dreate dummy basic block, as our callers should see if
        // we set up the latch

        lastValue.clear();

        return true;
    }

    return false;
}

ir_visitor_status
    GlslToTopVisitor::visit(ir_loop_jump *ir)
{
    // Create a block for the parent to continue inserting stuff into (e.g. this
    // break/continue is inside an if-then-else)
    llvm::Function *function = llvmBuilder.GetInsertBlock()->getParent();
    llvm::BasicBlock* postLoopJump = llvm::BasicBlock::Create(context, "post-loopjump", function);

    if (ir->is_break()) {
        llvmBuilder.CreateBr(loops.top().exit);
    }

    if (ir->is_continue()) {
        bool done = setUpLatch();
        if (! done)
            llvmBuilder.CreateBr(loops.top().header);
    }

    llvmBuilder.SetInsertPoint(postLoopJump);

    lastValue.clear();

    // Continue on with the parent (any further statements discarded)
    return visit_continue_with_parent;

}

int GlslToTopVisitor::getNextInterpIndex(std::string name)
{
    // Get the index for this interpolant, or create a new unique one
    std::map<std::string, int>::iterator iter;
    iter = interpMap.find(name);

    if (interpMap.end() == iter) {
        interpMap[name] = interpIndex++;
    }

    return interpMap[name];
}

ir_visitor_status
    GlslToTopVisitor::visit(ir_dereference_variable *derefVariable)
{
    bool isPipelineInput = false;

    // Grab the actual variable
    ir_variable *var = derefVariable->variable_referenced();

    // Search our value map for existing entry
    std::map<ir_variable*, gla::Builder::SuperValue>::iterator iter;
    iter = namedValues.find(var);

    if (namedValues.end() == iter) {

        // If we don't find the variable, and it is considered input
        // it is a pipeline read
        if (var->mode != ir_var_in) {
            // it was not found, create it
            namedValues[var] = createLLVMVariable(var);
        } else {
            isPipelineInput = true;
        }
    }

    // Track the current value
    if(! isPipelineInput)
        lastValue = namedValues[var];

    if (in_assignee) {
        // Track the base and chain so we can store later
        lValue.base = lastValue;
        lValue.indexChain = gepIndexChainStack.top();
        gepIndexChainStack.top().clear();
    }
    else
    {
        glsl_base_type baseType = var->type->base_type;

        if (isPipelineInput) {
            lastValue = createPipelineRead(var, 0);
        }
        else if (GLSL_TYPE_ARRAY == baseType || GLSL_TYPE_STRUCT == baseType) {
            assert(gepIndexChainStack.top().size() >= 0);
            unsigned indices[maxGepIndices];
            int indexCount = 0;

            switch (var->mode) {
            case ir_var_uniform:

                // Since we're using GEP for uniforms, but not supporting pointers, we can hard code the
                // first index to zero.
                gepIndexChainStack.top().push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0));

                std::reverse(gepIndexChainStack.top().begin(), gepIndexChainStack.top().end());

                // Use the indices we've built up to finally dereference the base type
                lastValue = llvmBuilder.CreateGEP(lastValue,
                                              gepIndexChainStack.top().begin(),
                                              gepIndexChainStack.top().end());

                lastValue = llvmBuilder.CreateLoad(lastValue);
                break;

            case ir_var_auto:
            case ir_var_temporary:

                std::reverse(gepIndexChainStack.top().begin(), gepIndexChainStack.top().end());

                if (convertValuesToUnsigned(indices, indexCount, gepIndexChainStack.top())) {
                    // If we're dereferencing an element in a local array or structure, we must
                    // load the entire aggregate and extract the element, otherwise
                    // promoteMemToReg will not drop the pointer references.
                    lastValue = llvmBuilder.CreateLoad(lastValue);
                    if (indexCount > 0)
                        lastValue = llvmBuilder.CreateExtractValue(lastValue, indices, indices + indexCount);
                } else {
                    gla::UnsupportedFunctionality("non-constant dereference in local array");
                }

                break;

            default:
                gla::UnsupportedFunctionality("struct or array dereference mode");
            }

            gepIndexChainStack.top().clear();
        }
        else if (var->mode != ir_var_in) {
            // Don't load inputs again... just use them
            if (lastValue.isMatrix()) {
                llvm::Value* newValue = llvmBuilder.CreateLoad(lastValue.getMatrix()->getMatrixValue(), "__matrix");
                gla::Builder::Matrix* loadedMatrix = new gla::Builder::Matrix(newValue);
                lastValue = gla::Builder::SuperValue(loadedMatrix);
            } else
                lastValue = llvmBuilder.CreateLoad(lastValue);
        }
    }

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_loop *ir)
{
    llvm::Function* function = llvmBuilder.GetInsertBlock()->getParent();

    llvm::BasicBlock *headerBB = llvm::BasicBlock::Create(context, "loop-header", function);
    llvm::BasicBlock *mergeBB  = llvm::BasicBlock::Create(context, "loop-merge");

    // Remember the blocks, so that breaks/continues inside the loop know where
    // to go
    LoopData ld = { };
    ld.exit   = mergeBB;
    ld.header = headerBB;

    if (ir->counter) {
        // I've not seen the compare field used, only from+to+increment
        assert(ir->from && ir->to && ir->increment && "partially undefined static inductive loop");
        assert(ir->from->as_constant() && ir->to->as_constant() && ir->increment->as_constant()
               && "non-constant fields for static inductive loop");

        ld.finish      = createLLVMConstant(ir->to->as_constant());
        ld.increment   = createLLVMConstant(ir->increment->as_constant());
        ld.counter     = createLLVMVariable(ir->counter);
        ld.isInductive = true;

        namedValues[ir->counter] = ld.counter;

        llvm::Constant* from = createLLVMConstant(ir->from->as_constant());

        llvmBuilder.CreateStore(from, ld.counter);
    }

    loops.push(ld);

    // Branch into the loop
    llvmBuilder.CreateBr(headerBB);

    // Set ourselves inside the loop
    llvmBuilder.SetInsertPoint(headerBB);

    visit_list_elements(this, &ir->body_instructions);

    // Branch back through the loop
    bool done = setUpLatch();
    if (! done)
        llvmBuilder.CreateBr(headerBB);

    // Now, create a new block for the rest of the post-loop program
    function->getBasicBlockList().push_back(mergeBB);
    llvmBuilder.SetInsertPoint(mergeBB);

    // Remove ourselves from the stacks
    loops.pop();

    // lastValue may not be up-to-date, and we shouldn't be referenced
    // anyways
    lastValue.clear();

    return visit_continue_with_parent;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_loop *ir)
{
    (void) ir;
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_function_signature *sig)
{
    localScope = true;

    if (!sig->is_builtin) {

        // We already made main for code outside of functions
        if (strcmp(sig->function_name(), "main") == 0) {
            inMain = true;
            llvmBuilder.SetInsertPoint(shaderEntry);

            return visit_continue;
        }

        std::vector<const llvm::Type*> paramTypes;
        ir_variable* parameter;

        exec_list_iterator iterParam = sig->parameters.iterator();

        while (iterParam.has_next()) {
            parameter = (ir_variable *) iterParam.get();
            paramTypes.push_back(convertGlslToGlaType(parameter->type));
            iterParam.next();
        }

        llvm::BasicBlock* functionBlock;
        llvm::Function *function = glaBuilder->makeFunctionEntry(convertGlslToGlaType(sig->return_type), sig->function_name(), paramTypes, functionBlock);
        llvmBuilder.SetInsertPoint(functionBlock);

        // Visit parameter list again to create local variables
        iterParam = sig->parameters.iterator();

        llvm::Function::arg_iterator arg = function->arg_begin();
        llvm::Function::arg_iterator endArg = function->arg_end();

        while (iterParam.has_next() && arg != endArg) {
            // Create a variable for our formal parameter
            parameter = (ir_variable *) iterParam.get();
            namedValues[parameter] = &(*arg);
            ++arg;
            iterParam.next();
        }

        // Track our user function to call later
        functionMap[sig] = function;

        return visit_continue;
    } else {

        // Don't traverse the bodies of built-in functions
        return visit_continue_with_parent;
    }
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_function_signature *sig)
{
    // Ignore builtins for now
    if (!sig->is_builtin) {

        llvm::BasicBlock* BB = llvmBuilder.GetInsertBlock();
        llvm::Function* F = llvmBuilder.GetInsertBlock()->getParent();
        assert(BB && F);

        // If our function did not contain a return,
        // return void now
        if (0 == BB->getTerminator()) {

            // Whether we're in an unreachable (non-entry) block
            bool unreachable = &*F->begin() != BB && pred_begin(BB) == pred_end(BB);

            if (inMain && !unreachable) {
                // If we're leaving main and it is not terminated,
                // generate our pipeline writes
                glaBuilder->copyOutPipeline(llvmBuilder);
                inMain = false;
            }

            // If we're not the entry block, and we have no predecessors, we're
            // unreachable, so don't bother adding a return instruction in
            // (e.g. we're in a post-return block). Otherwise add a ret void.
            if (unreachable)
                llvmBuilder.CreateUnreachable();
            else
                llvmBuilder.CreateRet(0);

        }
    }

    localScope = false;
    llvmBuilder.SetInsertPoint(shaderEntry);

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_function *f)
{
    // In GLSL2 tree, functions are a collection of signatures
    // For LunarGLASS, we treat each signature as a function
    // Thus, nothing to do for visiting an ir_function

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_function *f)
{
    // Nothing to do here.
    // See comment in visit_enter(ir_function *f)

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_expression *expression)
{
    int numOperands = expression->get_num_operands();
    assert(numOperands <= 2);
    gla::Builder::SuperValue operands[2];

    for (int i = 0; i < numOperands; ++i) {
        expression->operands[i]->accept(this);
        operands[i] = lastValue;
    }

    lastValue = expandGLSLOp(expression->operation, operands);

    return visit_continue_with_parent;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_expression *ir)
{
    (void) ir;
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_texture *ir)
{
    // Build up an array of dynamic texture parameters to pass to TopBuilder
    gla::Builder::TextureParameters textureParameters;

    // Traverse coordinates
    ir->coordinate->accept(this);
    textureParameters.ETPCoords = lastValue;

    // Select texture op and traverse required info, set some flags
    gla::ETextureFlags texFlags = {0};
    switch (ir->op) {
    case ir_tex:
        break;
    case ir_txb:
        ir->lod_info.bias->accept(this);
        textureParameters.ETPBiasLod = lastValue;
        texFlags.EBias = true;
        break;
    case ir_txl:
        ir->lod_info.lod->accept(this);
        textureParameters.ETPBiasLod = lastValue;
        texFlags.ELod = true;
        break;
    case ir_txd:
    case ir_txf:
        gla::UnsupportedFunctionality("texture opcode", (int)ir->op, ir->opcode_string());
        break;
    }

    // Detect and traverse projection
    if (ir->projector) {
        ir->projector->accept(this);
        textureParameters.ETPProj = lastValue;
        texFlags.EProjected = true;
    }

    // Detect and traverse shadow comparison
    if (ir->shadow_comparitor) {
        // TODO:  Renable shadow samples when ref and coords are reunited.
        gla::UnsupportedFunctionality("shadow comparison in texture sample", gla::EATContinue);
        //ir->shadow_comparitor->accept(this);
        //textureParameters.ETPShadowRef = lastValue;
        //texFlags.EShadow = true;
    }

    // Detect array index
    if (ir->sampler->type->sampler_array)
        gla::UnsupportedFunctionality("arrayed sampler index");

    // Traverse the sampler
    ir->sampler->accept(this);
    textureParameters.ETPSampler = lastValue;

    // Convert the sampler type to gla enum
    gla::ESamplerType samplerType;
    switch (ir->sampler->type->sampler_dimensionality) {
    case GLSL_SAMPLER_DIM_1D:
        samplerType = gla::ESampler1D;
        break;
    case GLSL_SAMPLER_DIM_2D:
        samplerType = gla::ESampler2D;
        break;
    case GLSL_SAMPLER_DIM_3D:
        samplerType = gla::ESampler3D;
        break;
    case GLSL_SAMPLER_DIM_CUBE:
        samplerType = gla::ESamplerCube;
        break;
    case GLSL_SAMPLER_DIM_RECT:
        samplerType = gla::ESampler2DRect;
        break;
    case GLSL_SAMPLER_DIM_BUF:
        samplerType = gla::ESamplerBuffer;
        break;
    default:
        gla::UnsupportedFunctionality("sampler type", ir->sampler->type->sampler_dimensionality);
    }

    // Pass all the compile time and run time info to TopBuilder to select the correct intrinsic and make the call
    lastValue = glaBuilder->createTextureCall(convertGlslToGlaType(ir->type), samplerType, texFlags, textureParameters);

    return visit_continue_with_parent;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_texture*)
{
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_swizzle *swiz)
{
    lastValue = expandGLSLSwizzle(swiz);

    return visit_continue_with_parent;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_swizzle *ir)
{
    (void) ir;
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_dereference_array *ir)
{
    // Derefences of array indexes could multiple base variables that
    // require multiple GEP instructions.
    // To support this, we create a new GEP index chain for each index.
    // The algorithm to support this is as follows:
    // - Startup: Init top of stack with empty chain
    // - Struct field dereference: Visit struct field and add to chain on top of stack
    // - Before evaluating array index:  Push new chain onto stack.
    // - After evaluating array index:  Pop a chain from the stack and push lastValue
    //   to the new top chain
    // - Visit base:  collapse and clear current top GEP chain

    int indexInt;
    llvm::Value* indexVal;
    std::vector<llvm::Value*> newChain;

    switch (ir->variable_referenced()->mode) {
    case ir_var_in:
        if (0 == ir->array_index->constant_expression_value())
            gla::UnsupportedFunctionality("input using non-constant array index");

        indexInt = ir->array_index->constant_expression_value()->value.u[0];
        lastValue = createPipelineRead(ir->variable_referenced(), indexInt);
        break;
    case ir_var_uniform:
    case ir_var_auto:
    case ir_var_temporary:

        // The index could be a nested chain, so start a new one
        gepIndexChainStack.push(newChain);

        // Traverse the index, which could be a complicated expression
        if (ir->array_index->constant_expression_value()) {
            indexInt = ir->array_index->constant_expression_value()->value.u[0];
            indexVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), indexInt);
        } else {
            // Turn off in_assignee for the l-value index
            const bool was_in_assignee = in_assignee;
            in_assignee = false;
            ir->array_index->accept(this);
            in_assignee = was_in_assignee;

            assert(llvm::isa<llvm::IntegerType>(lastValue->getType()));
            indexVal = lastValue;
        }

        // Add the result of previous chain to current one
        gepIndexChainStack.pop();
        assert(gepIndexChainStack.size());
        gepIndexChainStack.top().push_back(indexVal);

        ir->array->accept(this);

        break;
    default:
        gla::UnsupportedFunctionality("unsupported array dereference");
    }

    // Continue to parent or parser will revisit the array
    return visit_continue_with_parent;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_dereference_array *ir)
{
    (void) ir;
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_dereference_record *ir)
{
    const glsl_type *struct_type = ir->record->type;
    int offset = 0;

    // Find and push the index that matches the requested field
    for (int i = 0; i < struct_type->length; i++) {
        if (strcmp(struct_type->fields.structure[i].name, ir->field) == 0)
            break;
        offset++;
    }

    assert(offset < struct_type->length);

    // Track the offset for collapse during variable dereference
    gepIndexChainStack.top().push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), offset));

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_dereference_record*)
{
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_assignment *assignment)
{
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_assignment *assignment)
{
    llvm::Value* lValueAggregate = NULL;
    unsigned indices[maxGepIndices];
    int indexCount = 0;

    // If we're assigning to an element in a local array or structure, we must
    // load the entire aggregate, insert the new value, and store the whole thing.
    // Otherwise, promoteMemToReg will not drop the pointer references.
    // If we're assigning to an entire struct or array, we'll have no GEP indices
    // and we'll store directly to the l-value.
    if (lValue.indexChain.size()) {
        std::reverse(lValue.indexChain.begin(), lValue.indexChain.end());
        if (convertValuesToUnsigned(indices, indexCount, lValue.indexChain)) {
            lValueAggregate = llvmBuilder.CreateLoad(lValue.base);
            lValue.indexChain.clear();
        } else {
            gla::UnsupportedFunctionality("non-constant dereference in local array");
        }
    }

    // Handle writemask
    // Only execute the write mask code if we're assigning to a vector without a
    // full write mask.
    if (assignment->lhs->type->vector_elements > 1 && 0 == assignment->whole_variable_written()) {
        llvm::Value* targetVector;
        int sourceElement = 0;
        llvm::Type::TypeID sourceType = lastValue->getType()->getTypeID();

        // Load our target vector
        if (lValueAggregate)
            targetVector = llvmBuilder.CreateExtractValue(lValueAggregate, indices, indices + indexCount);
        else
            targetVector = llvmBuilder.CreateLoad(lValue.base);

        // Check each channel of the writemask
        for(int i = 0; i < 4; ++i) {
            if(assignment->write_mask & (1 << i)) {
                if(llvm::Type::VectorTyID == sourceType) {
                    // Extract an element to a scalar, then immediately insert to our target
                    targetVector = llvmBuilder.CreateInsertElement(targetVector,
                                            llvmBuilder.CreateExtractElement(lastValue,
                                                    llvm::ConstantInt::get(context, llvm::APInt(32, sourceElement++, false))),
                                            llvm::ConstantInt::get(context, llvm::APInt(32, i, false)));
                } else {
                    // Insert the scalar target
                    targetVector = llvmBuilder.CreateInsertElement(targetVector,
                                            lastValue,
                                            llvm::ConstantInt::get(context, llvm::APInt(32, i, false)));
                }
            }
        }
        // Track the last use of our extract/insert location to be stored
        lastValue = targetVector;
    }

    // Retroactively change the name of the last-value temp to the name of the
    // l-value, to help debuggability, if it's just an llvm temp name.
    if (lastValue->getNameStr().length() < 2 || (lastValue->getNameStr()[1] >= '0' && lastValue->getNameStr()[1] <= '9'))
        lastValue->setName(lValue.base->getName());

    // If we loaded the whole l-value, then insert our r-value
    if (lValueAggregate)
        lastValue = llvmBuilder.CreateInsertValue(lValueAggregate, lastValue, indices, indices + indexCount);

    // Store the last value into the l-value, using dest we track in base class.
    llvm::StoreInst *store = llvmBuilder.CreateStore(lastValue, lValue.base);

    lastValue = store;

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_call *call)
{
    exec_list_iterator iter = call->actual_parameters.iterator();

    // Stick this somewhere that makes sense, with a real value
    #define GLA_MAX_PARAMETERS 10

    gla::Builder::SuperValue llvmParams[GLA_MAX_PARAMETERS];
    ir_rvalue *param = NULL;
    int paramCount = 0;

    // Build a list of actual parameters
    while(iter.has_next())
    {
        param = (ir_rvalue *) iter.get();
        param->accept(this);
        llvmParams[paramCount] = lastValue;
        paramCount++;
        iter.next();
    }

    assert(paramCount < GLA_MAX_PARAMETERS);

    if(call->get_callee()->function()->has_user_signature()) {
        const char *name = call->callee_name();
        llvm::CallInst *callInst = 0;

       // Grab the pointer from the previous created function
        llvm::Function* function = functionMap[call->get_callee()];
        assert(function);

       // Create a call to it
        switch(paramCount) {
        case 5:     callInst = llvmBuilder.CreateCall5(function, llvmParams[0], llvmParams[1], llvmParams[2], llvmParams[3], llvmParams[4]);      break;
        case 4:     callInst = llvmBuilder.CreateCall4(function, llvmParams[0], llvmParams[1], llvmParams[2], llvmParams[3]);                     break;
        case 3:     callInst = llvmBuilder.CreateCall3(function, llvmParams[0], llvmParams[1], llvmParams[2]);                                    break;
        case 2:     callInst = llvmBuilder.CreateCall2(function, llvmParams[0], llvmParams[1]);                                                   break;
        case 1:     callInst = llvmBuilder.CreateCall (function, llvmParams[0]);                                                                  break;
        case 0:     callInst = llvmBuilder.CreateCall (function);                                                                                 break;
        default:    assert(! "Unsupported parameter count");
        }

        // Track the return value for to be consumed by next instruction
        lastValue = callInst;

    } else {

        gla::Builder::SuperValue returnValue;

        if(!strcmp(call->callee_name(), "mod")) {
            returnValue = expandGLSLOp(ir_binop_mod, llvmParams);
        }
        else if(!strcmp(call->callee_name(), "mix")) {
            if(llvm::Type::IntegerTyID == gla::GetBasicType(llvmParams[0]))
                returnValue = llvmBuilder.CreateSelect(llvmParams[2], llvmParams[0], llvmParams[1]);
        }
        else if(!strcmp(call->callee_name(), "lessThan")) {
            returnValue = expandGLSLOp(ir_binop_less, llvmParams);
        }
        else if(!strcmp(call->callee_name(), "lessThanEqual")) {
            returnValue = expandGLSLOp(ir_binop_lequal, llvmParams);
        }
        else if(!strcmp(call->callee_name(), "greaterThan")) {
            returnValue = expandGLSLOp(ir_binop_greater, llvmParams);
        }
        else if(!strcmp(call->callee_name(), "greaterThanEqual")) {
            returnValue = expandGLSLOp(ir_binop_gequal, llvmParams);
        }
        else if(!strcmp(call->callee_name(), "equal")) {
            returnValue = expandGLSLOp(ir_binop_equal, llvmParams);
        }
        else if(!strcmp(call->callee_name(), "notEqual")) {
            returnValue = expandGLSLOp(ir_binop_nequal, llvmParams);
        }
        else if(!strcmp(call->callee_name(), "not")) {
            returnValue = expandGLSLOp(ir_unop_logic_not, llvmParams);
        }

        // matrix built-ins that get decomposed into operations
        // rather than translated to an intrinsic
        else if (!strcmp(call->callee_name(), "matrixCompMult")) {
            returnValue = glaBuilder->createMatrixOp(llvm::BinaryOperator::FMul, llvmParams[0], llvmParams[1]);
        }
        else if (!strcmp(call->callee_name(), "outerProduct")) {
            returnValue = glaBuilder->createMatrixMultiply(llvmParams[0], llvmParams[1]);
        }
        else if (!strcmp(call->callee_name(), "transpose")) {
            returnValue = glaBuilder->createMatrixTranspose(llvmParams[0].getMatrix());
        }
        else if (!strcmp(call->callee_name(), "inverse")) {
            returnValue = glaBuilder->createMatrixInverse(llvmParams[0].getMatrix());
        }
        else if (!strcmp(call->callee_name(), "determinant")) {
            returnValue = glaBuilder->createMatrixDeterminant(llvmParams[0].getMatrix());
        }

        // If this call requires an intrinsic
        if(returnValue.getValue() == 0)
            returnValue = createLLVMIntrinsic(call, llvmParams, paramCount);

        // Track the return value for to be consumed by next instruction
        lastValue = returnValue;
    }

    return visit_continue_with_parent;
}

llvm::Value* GlslToTopVisitor::createLLVMIntrinsic(ir_call *call, gla::Builder::SuperValue* llvmParams, int paramCount)
{
    llvm::Function *intrinsicName = 0;
    const llvm::Type* resultType = convertGlslToGlaType(call->type);

    #define GLA_MAX_PARAMETER 5
    gla::Builder::SuperValue outParams[GLA_MAX_PARAMETER];
    for(int i = 0; i < GLA_MAX_PARAMETER; i++)
        outParams[i] = llvmParams[i];

    // Based on the name of the callee, create the appropriate intrinsicID
    if(!strcmp(call->callee_name(), "radians"))                 { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fRadians, resultType, llvmParams[0]->getType());  }
    else if(!strcmp(call->callee_name(), "degrees"))            { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fDegrees, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "sin"))                { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fSin, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "cos"))                { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fCos, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "tan"))                { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fTan, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "asin"))               { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fAsin, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "acos"))               { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fAcos, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "atan"))               { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fAtan, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "sinh"))               { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fSinh, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "cosh"))               { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fCosh, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "tanh"))               { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fTanh, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "asinh"))              { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fAsinh, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "acosh"))              { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fAcosh, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "atanh"))              { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fAtanh, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "pow"))                { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fPow, resultType, llvmParams[0]->getType(), llvmParams[1]->getType());  }
    else if(!strcmp(call->callee_name(), "exp"))                { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fExp, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "log"))                { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fLog, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "exp2"))               { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fExp2, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "log2"))               { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fLog2, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "sqrt"))               { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fSqrt, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "inversesqrt"))        { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fInverseSqrt, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "floor"))              { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fFloor, resultType, llvmParams[0]->getType()); }
    //else if(!strcmp(call->callee_name(), "trunc"))            { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::xxxx, resultType); }  // defect in glsl2
    else if(!strcmp(call->callee_name(), "round"))              { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fRoundFast, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "roundEven"))          { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fRoundEven, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "ceil"))               { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fCeiling, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "fract"))              { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fFraction, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "modf"))               { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fModF, resultType, llvmParams[0]->getType(), llvmParams[1]->getType());  }
    else if(!strcmp(call->callee_name(), "mix"))                { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fMix, resultType, llvmParams[0]->getType(), llvmParams[1]->getType(), llvmParams[2]->getType()); }
    else if(!strcmp(call->callee_name(), "step"))               { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fStep, resultType, llvmParams[0]->getType(), llvmParams[1]->getType());  }
    else if(!strcmp(call->callee_name(), "smoothstep"))         { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fSmoothStep, resultType, llvmParams[0]->getType(), llvmParams[1]->getType(), llvmParams[2]->getType()); }
    else if(!strcmp(call->callee_name(), "intBitsToFloat"))     { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fIntBitsTofloat, resultType); }
    else if(!strcmp(call->callee_name(), "uintBitsToFloat"))    { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fIntBitsTofloat, resultType); }
    else if(!strcmp(call->callee_name(), "fma"))                { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fFma, resultType); }
    else if(!strcmp(call->callee_name(), "frexp"))              { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fFrexp, resultType); }
    else if(!strcmp(call->callee_name(), "ldexp"))              { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fLdexp, resultType); }
    else if(!strcmp(call->callee_name(), "unpackUnorm2x16"))    { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fUnpackUnorm2x16, resultType); }
    else if(!strcmp(call->callee_name(), "unpackUnorm4x8"))     { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fUnpackUnorm4x8, resultType); }
    else if(!strcmp(call->callee_name(), "unpackSnorm4x8"))     { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fUnpackSnorm4x8, resultType); }
    else if(!strcmp(call->callee_name(), "length"))             { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fLength, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "distance"))           { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fDistance, llvmParams[0]->getType(), llvmParams[1]->getType());  }
    else if(!strcmp(call->callee_name(), "dot"))                { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fDot, llvmParams[0]->getType(), llvmParams[1]->getType());  }
    else if(!strcmp(call->callee_name(), "cross"))              { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fCross, resultType, llvmParams[0]->getType(), llvmParams[1]->getType());  }
    else if(!strcmp(call->callee_name(), "normalize"))          { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fNormalize, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "ftransform"))         { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fFixedTransform, resultType, llvmParams[0]->getType(), llvmParams[1]->getType());  }
    else if(!strcmp(call->callee_name(), "faceforward"))        { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fFaceForward, resultType, llvmParams[0]->getType(), llvmParams[1]->getType(), llvmParams[2]->getType()); }
    else if(!strcmp(call->callee_name(), "reflect"))            { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fReflect, resultType, llvmParams[0]->getType(), llvmParams[1]->getType());  }
    else if(!strcmp(call->callee_name(), "refract"))            { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fRefract, resultType, llvmParams[0]->getType(), llvmParams[1]->getType(), llvmParams[2]->getType()); }
    else if(!strcmp(call->callee_name(), "dFdx"))               { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fDFdx, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "dFdy"))               { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fDFdy, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "fwidth"))             { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fFilterWidth, resultType, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "any"))                { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_any, llvmParams[0]->getType()); }
    else if(!strcmp(call->callee_name(), "all"))                { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_all, llvmParams[0]->getType()); }

    // Select intrinsic based on parameter types
    else if(!strcmp(call->callee_name(), "abs"))                {
        switch(gla::GetBasicType(llvmParams[0]))                  {
        case llvm::Type::IntegerTyID:                           { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_abs, resultType, llvmParams[0]->getType()); break; }
        case llvm::Type::FloatTyID:                             { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fAbs, resultType, llvmParams[0]->getType()); break; }  }  }
    else if(!strcmp(call->callee_name(), "sign"))               {
        switch(gla::GetBasicType(llvmParams[0]))                  {
        case llvm::Type::IntegerTyID:                           { gla::UnsupportedFunctionality("Integer sign() ");  break;  }
        case llvm::Type::FloatTyID:                             { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fSign, resultType, llvmParams[0]->getType());  break; }  }  }
    else if(!strcmp(call->callee_name(), "min"))                {
        switch(gla::GetBasicType(llvmParams[0]))                  {
        case llvm::Type::IntegerTyID:                           { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_sMin, resultType, llvmParams[0]->getType(), llvmParams[1]->getType()); break; }
        case llvm::Type::FloatTyID:                             { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fMin, resultType, llvmParams[0]->getType(), llvmParams[1]->getType()); break; }  }  }
    else if(!strcmp(call->callee_name(), "max"))                {
        switch(gla::GetBasicType(llvmParams[0]))                  {
        case llvm::Type::IntegerTyID:                           { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_sMax, resultType, llvmParams[0]->getType(), llvmParams[1]->getType()); break; }
        case llvm::Type::FloatTyID:                             { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fMax, resultType, llvmParams[0]->getType(), llvmParams[1]->getType()); break; }  }  }
    else if(!strcmp(call->callee_name(), "clamp"))              {
        switch(gla::GetBasicType(llvmParams[0]))                  {
        case llvm::Type::IntegerTyID:                           { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_sClamp, resultType, llvmParams[0]->getType(), llvmParams[1]->getType(), llvmParams[2]->getType()); break; }
        case llvm::Type::FloatTyID:                             { intrinsicName = glaBuilder->getIntrinsic(llvm::Intrinsic::gla_fClamp, resultType, llvmParams[0]->getType(), llvmParams[1]->getType(), llvmParams[2]->getType()); break; }  }  }
    // Unsupported calls
    // i.e. noise
    else {
        gla::UnsupportedFunctionality("Built-in function: ", gla::EATContinue);
        gla::UnsupportedFunctionality(call->callee_name());
    }

    llvm::CallInst *callInst = 0;

    // Create a call to it
    switch(paramCount)
    {
    case 5:
        callInst = llvmBuilder.CreateCall5(intrinsicName, outParams[0] , outParams[1], outParams[2], outParams[3], outParams[4]);
        break;
    case 4:
        callInst = llvmBuilder.CreateCall4(intrinsicName, outParams[0] , outParams[1], outParams[2], outParams[3]);
        break;
    case 3:
        callInst = llvmBuilder.CreateCall3(intrinsicName, outParams[0] , outParams[1], outParams[2]);
        break;
    case 2:
        callInst = llvmBuilder.CreateCall2(intrinsicName, outParams[0], outParams[1]);
        break;
    case 1:
        callInst = llvmBuilder.CreateCall (intrinsicName, outParams[0]);
        break;
    default:
        assert(! "Unsupported parameter count");
    }

   return callInst;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_call *call)
{
    (void) call;
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_return *ir)
{
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_return *ir)
{
    // If we're traversing a return in main,
    // generate pipeline writes
    if (inMain) {
        glaBuilder->copyOutPipeline(llvmBuilder);
    }

    // Return the expression result, which is tracked in lastValue
    if (ir->get_value()) {
        llvmBuilder.CreateRet(lastValue);
    } else {
        llvmBuilder.CreateRet(0);
    }

    lastValue.clear();

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_discard *ir)
{
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_discard *ir)
{
    (void) ir;
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_if *ifNode)
{
    // emit condition into current block
    ifNode->condition->accept(this);
    gla::Builder::If ifBuilder(lastValue, ! ifNode->else_instructions.is_empty(), glaBuilder);

    // emit the then statement
    visit_list_elements(this, &(ifNode->then_instructions));
    ifBuilder.makeEndThen();

    // emit the else statement
    if (! ifNode->else_instructions.is_empty())
        visit_list_elements(this, &(ifNode->else_instructions));

    ifBuilder.makeEndIf();

    // The glsl "value" of an if-else should never be taken
    lastValue.clear();

    return visit_continue_with_parent;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_if *ir)
{
    (void) ir;
    return visit_continue;
}

gla::Builder::SuperValue GlslToTopVisitor::createLLVMVariable(ir_variable* var)
{
    if (strcmp(var->name, "gl_FragDepth") == 0)
        gla::UnsupportedFunctionality("gl_FragDepth");

    if (strcmp(var->name, "gl_FragData") == 0)
        gla::UnsupportedFunctionality("gl_FragData");

    llvm::Constant* initializer = 0;
    gla::Builder::EStorageQualifier storageQualifier;
    int constantBuffer = 0;

    switch (var->mode) {
    case ir_var_temporary:
    case ir_var_auto:
        if (localScope)
            storageQualifier = gla::Builder::ESQLocal;
        else
            storageQualifier = gla::Builder::ESQGlobal;
        if (var->read_only) {
            // The GLSL2 front-end confusingly writes to constants, so we can't
            // actually treat them as constants.  Instead, they are just
            // initialized variables.
            initializer = createLLVMConstant(var->constant_value);
        }
        break;

    case ir_var_uniform:
        storageQualifier = gla::Builder::ESQUniform;
        // ?? need to generalize to N objects (constant buffers) for higher shader models
        constantBuffer = 0;
        break;

    case ir_var_in:
        // inputs should all be pipeline reads or created at function creation time
        assert(! "no memory allocations for inputs");
        break;

    case ir_var_out:
        storageQualifier = gla::Builder::ESQOutput;
        break;

    default:
        assert(! "Unhandled var->mode");
    }

    std::string* annotationAddr = 0;
    std::string annotation;
    if (var->type->base_type == GLSL_TYPE_SAMPLER) {
        annotation = std::string(getSamplerTypeName(var));
        annotationAddr = &annotation;
    }

    //?? still need to consume the following
    // var->max_array_access;
    // var->centroid;
    // var->invariant;
    // var->interpolation;
    // var->origin_upper_left;
    // var->pixel_center_integer;
    // var->location;

    const llvm::Type *llvmType = convertGlslToGlaType(var->type);

    return glaBuilder->createVariable(storageQualifier, constantBuffer, llvmType, var->type->is_matrix(), initializer, annotationAddr, var->name);
}

const char* GlslToTopVisitor::getSamplerTypeName(ir_variable* var)
{
    // TODO:  Renable shadow samples when ref and coords are reunited.
    if (false /*var->type->sampler_shadow*/) {
        switch (var->type->sampler_dimensionality) {
        case GLSL_SAMPLER_DIM_1D:   return "sampler1DShadow";
        case GLSL_SAMPLER_DIM_2D:   return "sampler2DShadow";
        case GLSL_SAMPLER_DIM_3D:   return "sampler3DShadow";
        case GLSL_SAMPLER_DIM_CUBE: return "samplerCubeShadow";
        case GLSL_SAMPLER_DIM_RECT: return "sampler2DRectShadow";
        default:
            gla::UnsupportedFunctionality("shadow sampler type");
        }
    } else {
        switch (var->type->sampler_dimensionality) {
        case GLSL_SAMPLER_DIM_1D:   return "sampler1D";
        case GLSL_SAMPLER_DIM_2D:   return "sampler2D";
        case GLSL_SAMPLER_DIM_3D:   return "sampler3D";
        case GLSL_SAMPLER_DIM_CUBE: return "samplerCube";
        case GLSL_SAMPLER_DIM_RECT: return "sampler2DRect";
        default:
            gla::UnsupportedFunctionality("sampler type");
        }
    }

    return 0;
}

gla::Builder::SuperValue GlslToTopVisitor::expandGLSLOp(ir_expression_operation glslOp, gla::Builder::SuperValue* operands)
{
    gla::Builder::SuperValue result;
    const llvm::Type* varType;

    //
    // 0 or more operands
    //

    // no 0-only operand operations yet

    //
    // 1 or more operands
    //

    // Initialize result to first operand to pass through unsupported ops
    result = operands[0];

    bool haveMatrix = operands[0].isMatrix();
    const llvm::VectorType* vectorType =  haveMatrix ? 0 : llvm::dyn_cast<llvm::VectorType>(operands[0]->getType());

    if (haveMatrix) {
        switch(glslOp) {
        case ir_unop_f2i:
        case ir_unop_i2f:
        case ir_unop_f2b:
        case ir_unop_b2f:
        case ir_unop_i2b:
        case ir_unop_b2i:
        case ir_unop_u2f:
            assert(! "Can't change matrix type");
            break;
        case ir_unop_neg:
            gla::UnsupportedFunctionality("Matrix negation", gla::EATContinue);
            return result;
        }
    }

    switch(glslOp) {
    case ir_unop_f2i:
        if(vectorType)  varType = llvm::VectorType::get(llvm::Type::getInt32Ty(context), vectorType->getNumElements());
        else            varType = llvm::Type::getInt32Ty(context);
        return          llvmBuilder.CreateFPToUI(operands[0], varType);
    case ir_unop_i2f:
        if(vectorType)  varType = llvm::VectorType::get(llvm::Type::getFloatTy(context), vectorType->getNumElements());
        else            varType = llvm::Type::getFloatTy(context);
        return          llvmBuilder.CreateSIToFP(operands[0], varType);
    case ir_unop_f2b:
        if(vectorType)  varType = llvm::VectorType::get(llvm::Type::getInt1Ty(context), vectorType->getNumElements());
        else            varType = llvm::Type::getInt1Ty(context);
        return          llvmBuilder.CreateFPToUI(operands[0], varType);
    case ir_unop_b2f:
        if(vectorType)  varType = llvm::VectorType::get(llvm::Type::getFloatTy(context), vectorType->getNumElements());
        else            varType = llvm::Type::getFloatTy(context);
        return          llvmBuilder.CreateUIToFP(operands[0], varType);
    case ir_unop_i2b:
        if(vectorType)  varType = llvm::VectorType::get(llvm::Type::getInt1Ty(context), vectorType->getNumElements());
        else            varType = llvm::Type::getInt1Ty(context);
        return          llvmBuilder.CreateIntCast(operands[0], varType, false);
    case ir_unop_b2i:
        if(vectorType)  varType = llvm::VectorType::get(llvm::Type::getInt32Ty(context), vectorType->getNumElements());
        else            varType = llvm::Type::getInt32Ty(context);
        return          llvmBuilder.CreateIntCast(operands[0], varType, true);
    case ir_unop_u2f:
        if(vectorType)  varType = llvm::VectorType::get(llvm::Type::getFloatTy(context), vectorType->getNumElements());
        else            varType = llvm::Type::getFloatTy(context);
        return          llvmBuilder.CreateUIToFP(operands[0], varType);
    case ir_unop_neg:
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::FloatTyID:         return llvmBuilder.CreateFNeg(operands[0]);
        case llvm::Type::IntegerTyID:       return llvmBuilder.CreateNeg (operands[0]);
        }
    case ir_unop_rcp:
        return glaBuilder->createRecip(operands[0]);
    }

    // Unary ops that require an intrinsic
    llvm::Intrinsic::ID intrinsicID = llvm::Intrinsic::ID(0);
    bool fixedResultType = false;
    switch(glslOp) {
    case ir_unop_sin:
        intrinsicID = llvm::Intrinsic::gla_fSin;
        break;
    case ir_unop_cos:
        intrinsicID = llvm::Intrinsic::gla_fCos;
        break;
    case ir_unop_sqrt:
        intrinsicID = llvm::Intrinsic::gla_fSqrt;
        break;
    case ir_unop_rsq:
        intrinsicID = llvm::Intrinsic::gla_fInverseSqrt;
        break;
    case ir_unop_exp:
        intrinsicID = llvm::Intrinsic::gla_fExp;
        break;
    case ir_unop_log:
        intrinsicID = llvm::Intrinsic::gla_fLog;
        break;
    case ir_unop_exp2:
        intrinsicID = llvm::Intrinsic::gla_fExp2;
        break;
    case ir_unop_log2:
        intrinsicID = llvm::Intrinsic::gla_fLog2;
        break;
    case ir_unop_floor:
        intrinsicID = llvm::Intrinsic::gla_fFloor;
        break;
    case ir_unop_ceil:
        intrinsicID = llvm::Intrinsic::gla_fCeiling;
        break;
    case ir_unop_fract:
        intrinsicID = llvm::Intrinsic::gla_fFraction;
        break;
    case ir_unop_dFdx:
        intrinsicID = llvm::Intrinsic::gla_fDFdx;
        break;
    case ir_unop_dFdy:
        intrinsicID = llvm::Intrinsic::gla_fDFdy;
        break;
    case ir_unop_any:
        intrinsicID = llvm::Intrinsic::gla_any;
        fixedResultType = true;
        break;
    case ir_unop_abs:
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::IntegerTyID:
            intrinsicID = llvm::Intrinsic::gla_abs; break;
        case llvm::Type::FloatTyID:
            intrinsicID = llvm::Intrinsic::gla_fAbs; break;
        }
        break;
    case ir_unop_sign:
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::IntegerTyID:
            gla::UnsupportedFunctionality("Integer sign()"); break;
        case llvm::Type::FloatTyID:
            intrinsicID = llvm::Intrinsic::gla_fSign; break;
        }
        break;
    }

    // If intrinsic was assigned, then call the function and return
    if (intrinsicID != 0) {
        llvm::Value* llvmOperands[] = { operands[0].getValue() };
        llvm::Function* intrinsicName = fixedResultType ?
            glaBuilder->getIntrinsic(intrinsicID, llvmOperands[0]->getType()) :
            glaBuilder->getIntrinsic(intrinsicID,  result->getType(), llvmOperands[0]->getType());
        return llvmBuilder.CreateCall(intrinsicName, llvmOperands, llvmOperands + 1);
    }

    //
    // 2 or more operands
    //

    haveMatrix = operands[0].isMatrix() || operands[1].isMatrix();

    if (haveMatrix) {
        llvm::Instruction::BinaryOps llvmOp;
        bool componentWise = true;

        switch(glslOp) {
        case ir_binop_add:
            llvmOp = llvm::BinaryOperator::FAdd;
            break;
        case ir_binop_sub:
            llvmOp = llvm::BinaryOperator::FSub;
            break;
        case ir_binop_mul:
            componentWise = false;
            llvmOp = llvm::BinaryOperator::FMul;
            break;
        case ir_binop_div:
            llvmOp = llvm::BinaryOperator::FDiv;
            break;
        case ir_binop_all_equal:
            return glaBuilder->createMatrixCompare(operands[0], operands[1], true);
        case ir_binop_any_nequal:
            return glaBuilder->createMatrixCompare(operands[0], operands[1], false);
        default:
            gla::UnsupportedFunctionality("Matrix operation");
        }

        if (componentWise)
            return glaBuilder->createMatrixOp(llvmOp, operands[0], operands[1]);
        else
            return glaBuilder->createMatrixMultiply(operands[0], operands[1]);
    }

    // we now know we don't have a matrix
    assert(! haveMatrix);

    switch(glslOp) {
    case ir_binop_add:
        glaBuilder->promoteScalar(operands[0], operands[1]);
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::FloatTyID:         return llvmBuilder.CreateFAdd(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return llvmBuilder.CreateAdd (operands[0], operands[1]);
        }
    case ir_binop_sub:
        glaBuilder->promoteScalar(operands[0], operands[1]);
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::FloatTyID:         return llvmBuilder.CreateFSub(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return llvmBuilder.CreateSub (operands[0], operands[1]);
        }
    case ir_binop_mul:
        glaBuilder->promoteScalar(operands[0], operands[1]);
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::FloatTyID:         return llvmBuilder.CreateFMul(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return llvmBuilder.CreateMul (operands[0], operands[1]);
        }
    case ir_binop_div:
        glaBuilder->promoteScalar(operands[0], operands[1]);
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::FloatTyID:         return llvmBuilder.CreateFDiv(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return llvmBuilder.CreateSDiv(operands[0], operands[1]);
        }
    case ir_binop_less:
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::FloatTyID:         return llvmBuilder.CreateFCmpOLT(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return llvmBuilder.CreateICmpSLT(operands[0], operands[1]);
        }
    case ir_binop_greater:
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::FloatTyID:         return llvmBuilder.CreateFCmpOGT(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return llvmBuilder.CreateICmpSGT(operands[0], operands[1]);
        }
    case ir_binop_lequal:
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::FloatTyID:         return llvmBuilder.CreateFCmpOLE(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return llvmBuilder.CreateICmpSLE(operands[0], operands[1]);
        }
    case ir_binop_gequal:
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::FloatTyID:         return llvmBuilder.CreateFCmpOGE(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return llvmBuilder.CreateICmpSGE(operands[0], operands[1]);
        }
    case ir_binop_equal:
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::FloatTyID:         return llvmBuilder.CreateFCmpOEQ(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return llvmBuilder.CreateICmpEQ (operands[0], operands[1]);
        }
    case ir_binop_nequal:
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::FloatTyID:         return llvmBuilder.CreateFCmpONE(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return llvmBuilder.CreateICmpNE (operands[0], operands[1]);
        }

    case ir_binop_lshift:
        glaBuilder->promoteScalar(operands[0], operands[1]);
        return llvmBuilder.CreateShl (operands[0], operands[1]);
    case ir_binop_rshift:
        glaBuilder->promoteScalar(operands[0], operands[1]);
        return llvmBuilder.CreateLShr(operands[0], operands[1]);
    case ir_binop_bit_and:
        glaBuilder->promoteScalar(operands[0], operands[1]);
        return llvmBuilder.CreateAnd (operands[0], operands[1]);
    case ir_binop_bit_or:
        glaBuilder->promoteScalar(operands[0], operands[1]);
        return llvmBuilder.CreateOr  (operands[0], operands[1]);
    case ir_binop_logic_xor:
    case ir_binop_bit_xor:
        glaBuilder->promoteScalar(operands[0], operands[1]);
        return llvmBuilder.CreateXor (operands[0], operands[1]);
    case ir_unop_logic_not:
    case ir_unop_bit_not:
        return llvmBuilder.CreateNot (operands[0]);
    case ir_binop_logic_and:
        assert(gla::IsBoolean(operands[0]->getType()) && gla::IsScalar(operands[0]->getType()));
        assert(gla::IsBoolean(operands[1]->getType()) && gla::IsScalar(operands[1]->getType()));
        return llvmBuilder.CreateAnd (operands[0], operands[1]);

    case ir_binop_mod:
        glaBuilder->promoteScalar(operands[0], operands[1]);
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::FloatTyID:         return llvmBuilder.CreateFRem(operands[0], operands[1]);
        case llvm::Type::IntegerTyID:       return llvmBuilder.CreateSRem(operands[0], operands[1]);
        }
    case ir_binop_all_equal:
        // Returns single boolean for whether all components of operands[0] equal the
        // components of operands[1]
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::FloatTyID:         result = llvmBuilder.CreateFCmpOEQ(operands[0], operands[1]);  break;
        case llvm::Type::IntegerTyID:       result = llvmBuilder.CreateICmpEQ (operands[0], operands[1]);  break;
        }
        if(vectorType)  return llvmBuilder.CreateCall(glaBuilder->getIntrinsic(llvm::Intrinsic::gla_all, result->getType()), result);
        else            return result;

    case ir_binop_any_nequal:
        // Returns single boolean for whether any component of operands[0] is
        // not equal to the corresponding component of operands[1].
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::FloatTyID:         result = llvmBuilder.CreateFCmpONE(operands[0], operands[1]);  break;
        case llvm::Type::IntegerTyID:       result = llvmBuilder.CreateICmpNE (operands[0], operands[1]);  break;
        }
        if(vectorType)  return llvmBuilder.CreateCall(glaBuilder->getIntrinsic(llvm::Intrinsic::gla_any, result->getType()), result);
        else            return result;
    }

    // Binary ops that require an intrinsic
    switch(glslOp) {
    case ir_binop_min:
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::IntegerTyID:
            intrinsicID = llvm::Intrinsic::gla_sMin;
            break;
        case llvm::Type::FloatTyID:
            intrinsicID = llvm::Intrinsic::gla_fMin;
            break;
        }
        break;
    case ir_binop_max:
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::IntegerTyID:
            intrinsicID = llvm::Intrinsic::gla_sMax;
            break;
        case llvm::Type::FloatTyID:
            intrinsicID = llvm::Intrinsic::gla_fMax;
            break;
        }
        break;
    case ir_binop_pow:
        switch(gla::GetBasicType(operands[0])) {
        case llvm::Type::IntegerTyID:
            intrinsicID = llvm::Intrinsic::gla_fPowi;
            break;
        case llvm::Type::FloatTyID:
            intrinsicID = llvm::Intrinsic::gla_fPow;
            break;
        }
        break;
    case ir_binop_dot:
        intrinsicID = llvm::Intrinsic::gla_fDot;
        fixedResultType = true;
        break;
    default:
        gla::UnsupportedFunctionality("glslOp ", glslOp);
    }

    // If intrinsic was assigned, then call the function and return
    if (intrinsicID != 0) {
        llvm::Value* llvmOperands[] = { operands[0], operands[1] };
        llvm::Function* intrinsicName = fixedResultType ?
            glaBuilder->getIntrinsic(intrinsicID, llvmOperands[0]->getType(), llvmOperands[1]->getType()) :
            glaBuilder->getIntrinsic(intrinsicID, result->getType(), llvmOperands[0]->getType(), llvmOperands[1]->getType());
        return llvmBuilder.CreateCall(intrinsicName, llvmOperands, llvmOperands + 2);
    }

    return result;
}

llvm::Value* GlslToTopVisitor::expandGLSLSwizzle(ir_swizzle* swiz)
{
    // traverse the tree we're swizzling
    swiz->val->accept(this);

    // convert our ir mask to a gla mask
    int swizValMask = gla::MakeSwizzleMask(swiz->mask.x, swiz->mask.y, swiz->mask.z, swiz->mask.w);

    const llvm::Type* finalType = convertGlslToGlaType(swiz->type);

    return glaBuilder->createSwizzle(lastValue, swizValMask, finalType);
}

const llvm::Type* GlslToTopVisitor::convertGlslToGlaType(const glsl_type* type)
{
    const llvm::Type *glaType;

    switch(type->base_type) {
    case GLSL_TYPE_UINT:
    case GLSL_TYPE_INT:
    case GLSL_TYPE_SAMPLER:
        glaType = gla::GetIntType(context);
        break;
    case GLSL_TYPE_FLOAT:
        glaType = gla::GetFloatType(context);
        break;
    case GLSL_TYPE_BOOL:
        glaType = gla::GetBoolType(context);
        break;
    case GLSL_TYPE_VOID:
        glaType = gla::GetVoidType(context);
        break;
    case GLSL_TYPE_ARRAY:
        glaType = llvm::ArrayType::get(convertGlslToGlaType(type->fields.array), type->array_size());
        break;
    case GLSL_TYPE_STRUCT:
        {
            std::vector<const llvm::Type*> structFields;
            llvm::StructType* structType = structMap[type->name];
            if (structType) {
                // If we've seen this struct type, return it
                glaType = structType;
            } else {
                // Create a vector of struct types for LLVM to consume
                for (int i = 0; i < type->length; i++) {
                    structFields.push_back(convertGlslToGlaType(type->fields.structure[i].type));
                }
                structType = llvm::StructType::get(context, structFields, false);
                module->addTypeName(type->name, structType);
                structMap[type->name] = structType;
                glaType = structType;
            }
        }
        break;
    case GLSL_TYPE_ERROR:
        assert(! "type error");
    default:
        gla::UnsupportedFunctionality("basic type");
        break;
    }

    if (type->is_matrix())
        return gla::Builder::Matrix::getType(glaType, type->column_type()->vector_elements, type->row_type()->vector_elements);

    // If this variable has a vector element count greater than 1, create an LLVM vector
    unsigned vecCount = type->vector_elements;
    if(vecCount > 1)
        glaType = llvm::VectorType::get(glaType, vecCount);

    return glaType;
}

void GlslToTopVisitor::appendArrayIndexToName(std::string &arrayName, int index)
{
    arrayName.append("[");
    llvm::raw_string_ostream out(arrayName);
    out << index;
    arrayName = out.str();
    arrayName.append("]");
}

llvm::Value* GlslToTopVisitor::createPipelineRead(ir_variable* var, int index)
{
    // For pipeline inputs, and we will generate a fresh pipeline read at each reference,
    // which we will optimize later.
    std::string name(var->name);
    const llvm::Type* readType;

    if (GLSL_TYPE_ARRAY == var->type->base_type) {
        // If we're reading from an array, we just finished traversing the index
        // interpLoc = lastValue;
        // Return the type contained within the array

        readType = convertGlslToGlaType(var->type->fields.array);
        appendArrayIndexToName(name, index);
    } else {
        readType = convertGlslToGlaType(var->type);
    }

    gla::EInterpolationMode mode = gla::EIMNone;
    if (glShader->Type != GL_FRAGMENT_SHADER)
        gla::UnsupportedFunctionality("non-fragment shader pipeline read");

    switch (var->interpolation) {
    case ir_var_smooth:
        mode = gla::EIMSmooth;
        break;
    case ir_var_noperspective:
        mode = gla::EIMNoperspective;
        break;
    case ir_var_flat:
        mode = gla::EIMNone;
        break;
    default:
        gla::UnsupportedFunctionality("interpolation mode");
    }

    // Give each interpolant a temporary unique index
    return glaBuilder->readPipeline(readType, name, getNextInterpIndex(name), mode);
}

// Some interfaces to our LLVM builder require unsigned indices instead of a vector.
// i.e. llvm::IRBuilder::CreateExtractValue()
// This method will do the conversion and inform the caller if not every element was
// a constant integer.
bool GlslToTopVisitor::convertValuesToUnsigned(unsigned* indices, int &count, std::vector<llvm::Value*> chain)
{
    std::vector<llvm::Value*>::iterator start = chain.begin();

    for (count = 0; start != chain.end(); ++start, ++count) {
        assert(count < maxGepIndices);
        if (llvm::Constant* constant = llvm::dyn_cast<llvm::Constant>(*start)) {
            if (llvm::ConstantInt *constantInt = llvm::dyn_cast<llvm::ConstantInt>(constant))
                indices[count] = constantInt->getValue().getSExtValue();
            else
                return false;
        } else {
            return false;
        }
    }

    return true;
}
