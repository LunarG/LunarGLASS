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
// Author: Michael Ilseman, LunarG
//
//===----------------------------------------------------------------------===//

#include "GlslToTopVisitor.h"
#include "LunarGLASSTopIR.h"
#include "LunarGLASSManager.h"
#include "Util.h"
#include "mtypes.h"
#include "Exceptions.h"
#include "Options.h"

#include "llvm/Support/CFG.h"
#include "llvm/Support/raw_ostream.h"


void GlslToTop(struct gl_shader* glShader, gla::Manager* manager)
{
    GlslToTopVisitor v(glShader, manager);

    foreach_iter(exec_list_iterator, iter, *glShader->ir) {
        ir_instruction *ir = (ir_instruction *)iter.get();

        ir->accept(&v);
    }
}

GlslToTopVisitor::GlslToTopVisitor(struct gl_shader* s, gla::Manager* manager)
    : context(llvm::getGlobalContext()), llvmBuilder(context),
      module(manager->getModule()), glShader(s), interpIndex(0), inMain(false), localScope(false), shaderEntry(0)
{
    // Init the first GEP index chain
    std::vector<llvm::Value*> firstChain;
    gepIndexChainStack.push(firstChain);

    // do this after the builder knows the module
    glaBuilder = new gla::Builder(llvmBuilder, manager);

    shaderEntry = glaBuilder->makeMain();
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

    // Type is used for struct and array constants
    const llvm::Type* type = convertGlslToGlaType(constant->type);

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
            for (int i = 0; i < constant->type->array_size(); ++i) {
                vals.push_back(createLLVMConstant(constant->array_elements[i]));
            }
            break;

        case GLSL_TYPE_STRUCT:
            foreach_iter(exec_list_iterator, iter, constant->components) {
                vals.push_back(createLLVMConstant((ir_constant *)iter.get()));
            }
            break;

        case GLSL_TYPE_SAMPLER:
        case GLSL_TYPE_VOID:
        case GLSL_TYPE_ERROR:
            assert(! "Bad vector constant type");
            break;
        }
    }

    return glaBuilder->getConstant(vals, type);
}

ir_visitor_status
    GlslToTopVisitor::visit(ir_loop_jump *ir)
{
    assert(ir->is_break() ^ ir->is_continue());

    if (ir->is_break()) {
        glaBuilder->makeLoopExit();
    } else {
        glaBuilder->makeLoopBackEdge();
    }

    lastValue.clear();

    // Continue on with the parent
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
    } else {
        glsl_base_type baseType = var->type->base_type;

        if (isPipelineInput) {
            lastValue = createPipelineRead(var, 0);
        } else if (GLSL_TYPE_ARRAY == baseType || GLSL_TYPE_STRUCT == baseType || lastValue.isMatrix()) {
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
                lastValue = glaBuilder->createGEP(lastValue, gepIndexChainStack.top());

                lastValue = glaBuilder->createLoad(lastValue);

                break;

            case ir_var_auto:
            case ir_var_temporary:

                std::reverse(gepIndexChainStack.top().begin(), gepIndexChainStack.top().end());

                if (gla::ConvertValuesToUnsigned(indices, indexCount, gepIndexChainStack.top())) {
                    // If we're dereferencing an element in a local array or structure, we must
                    // load the entire aggregate and extract the element, otherwise
                    // promoteMemToReg will not drop the pointer references.
                    lastValue = glaBuilder->createLoad(lastValue);
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
        } else if (var->mode != ir_var_in) {
            // Don't load inputs again... just use them
            lastValue = glaBuilder->createLoad(lastValue);
        }
    }

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_loop *ir)
{
    // If we're inductive, then make a new inductive loop, else just a generic one.
    if (ir->counter) {
        // I've not seen the compare field used, only from+to+increment
        assert(ir->from && ir->to && ir->increment && "partially undefined static inductive loop");
        assert(ir->from->as_constant() && ir->to->as_constant() && ir->increment->as_constant()
               && "non-constant fields for static inductive loop");

        llvm::Constant* finish    = createLLVMConstant(ir->to->as_constant());
        llvm::Constant* increment = createLLVMConstant(ir->increment->as_constant());
        llvm::Value*    counter   = createLLVMVariable(ir->counter);
        llvm::Constant* from      = createLLVMConstant(ir->from->as_constant());

        namedValues[ir->counter] = counter;

        glaBuilder->makeNewLoop(counter, from, finish, increment, haveBuilderIncrementInductiveVariable);
    } else {
        glaBuilder->makeNewLoop();
    }

    // Do the body
    visit_list_elements(this, &ir->body_instructions);

    // Finish the loop
    glaBuilder->closeLoop();

    // lastValue may not be up-to-date, and we shouldn't be referenced anyways
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
        llvm::Function *function = glaBuilder->makeFunctionEntry(convertGlslToGlaType(sig->return_type), sig->function_name(), paramTypes, &functionBlock);
        function->addFnAttr(llvm::Attribute::AlwaysInline);
        llvmBuilder.SetInsertPoint(functionBlock);

        // Visit parameter list again to create mappings to local variables and set attributes.
        iterParam = sig->parameters.iterator();

        llvm::Function::arg_iterator arg = function->arg_begin();
        llvm::Function::arg_iterator endArg = function->arg_end();

        while (iterParam.has_next() && arg != endArg) {
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
    if (! sig->is_builtin) {
        glaBuilder->leaveFunction(inMain);

        if (inMain)
            inMain = false;
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
    bool haveMatrix = false;
    bool isFloat = false;
    bool isSigned = false;

    for (int i = 0; i < numOperands; ++i) {
        expression->operands[i]->accept(this);
        operands[i] = lastValue;
        haveMatrix |= lastValue.isMatrix();
    }

    switch (expression->operands[0]->type->base_type) {
    //case GLSL_TYPE_DOUBLE:
    case GLSL_TYPE_FLOAT:
        isFloat = true;
        break;
    case GLSL_TYPE_INT:
        isSigned = true;
        break;
    }

    gla::Builder::SuperValue result;

    switch (numOperands) {
    case 1:
        if (haveMatrix)
            result = createUnaryMatrixOperation(expression->operation, operands[0]);

        if (result.isClear())
            result = createUnaryOperation(expression->operation, expression->type, operands[0], isFloat, isSigned);

        if (result.isClear())
            result = createUnaryIntrinsic(expression->operation, operands[0], isFloat, isSigned);

        break;
    case 2:
        if (haveMatrix)
            result = createBinaryMatrixOperation(expression->operation, operands[0], operands[1]);

        if (result.isClear())
            result = createBinaryOperation(expression->operation, operands[0], operands[1], isFloat, isSigned);

        if (result.isClear())
            result = createBinaryIntrinsic (expression->operation, operands[0], operands[1], isFloat, isSigned);
    }

    if (result.isClear())
        gla::UnsupportedFunctionality("glslOp ", expression->operation);

    lastValue = result;

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
    gla::Builder::TextureParameters textureParameters = {};

    // Traverse coordinates
    ir->coordinate->accept(this);
    textureParameters.ETPCoords = lastValue;

    // Select texture op and traverse required info, set some flags
    int texFlags = 0;
    switch (ir->op) {
    case ir_tex:
        break;
    case ir_txb:
        texFlags |= gla::ETFBias;
        ir->lod_info.bias->accept(this);
        textureParameters.ETPBiasLod = lastValue;
        texFlags |= gla::ETFBiasLodArg;
        break;
    case ir_txl:
        texFlags |= gla::ETFLod;
        ir->lod_info.lod->accept(this);
        textureParameters.ETPBiasLod = lastValue;
        texFlags |= gla::ETFBiasLodArg;
        break;
    case ir_txf:
        texFlags |= gla::ETFFetch;
        ir->lod_info.lod->accept(this);
        textureParameters.ETPBiasLod = lastValue;
        texFlags |= gla::ETFBiasLodArg;
        break;
    case ir_txd:
        // There's no feature flag for gradient.  Parameter presence alone 
        // will trigger TopBuilder to select the right intrinsic.
        ir->lod_info.grad.dPdx->accept(this);
        textureParameters.ETPGradX = lastValue;
        ir->lod_info.grad.dPdy->accept(this);
        textureParameters.ETPGradY = lastValue;
        break;
    default:
        gla::UnsupportedFunctionality("texture opcode", (int)ir->op, ir->opcode_string());
        break;
    }

    // Texel offsets
    if (ir->offsets[0] || ir->offsets[1] || ir->offsets[2]) {
        int coordWidth = gla::GetComponentCount(textureParameters.ETPCoords);
        assert(coordWidth > 0 && coordWidth < 4);

        llvm::Constant* offset[3];
        offset[0] = gla::MakeIntConstant(context, ir->offsets[0]);
        offset[1] = gla::MakeIntConstant(context, ir->offsets[1]);
        offset[2] = gla::MakeIntConstant(context, ir->offsets[2]);

        texFlags |= gla::ETFOffsetArg;
        textureParameters.ETPOffset = llvm::ConstantVector::get(llvm::ArrayRef<llvm::Constant*>(offset, coordWidth));
    }

    // Detect and traverse projection
    if (ir->projector) {
        texFlags |= gla::ETFProjected;
        ir->projector->accept(this);
        textureParameters.ETPProj = lastValue;
        texFlags |= gla::ETFProjectedArg;
    }

    // Detect and traverse shadow comparison
    if (ir->shadow_comparitor) {
        texFlags |= gla::ETFShadow;
        ir->shadow_comparitor->accept(this);
        textureParameters.ETPShadowRef = lastValue;
        texFlags |= gla::ETFRefZArg;
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
    gla::Builder::SuperValue lValueAggregate;

    unsigned indices[maxGepIndices];
    int indexCount = 0;

    // If we're assigning to an element in a local array or structure, we must
    // load the entire aggregate, insert the new value, and store the whole thing.
    // Otherwise, promoteMemToReg will not drop the pointer references.
    // If we're assigning to an entire struct or array, we'll have no GEP indices
    // and we'll store directly to the l-value.
    if (lValue.indexChain.size()) {
        std::reverse(lValue.indexChain.begin(), lValue.indexChain.end());
        if (gla::ConvertValuesToUnsigned(indices, indexCount, lValue.indexChain)) {
            lValueAggregate = glaBuilder->createLoad(lValue.base);
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
        if (lValueAggregate.isClear())
            targetVector = glaBuilder->createLoad(lValue.base);
        else
            targetVector = llvmBuilder.CreateExtractValue(lValueAggregate, indices, indices + indexCount);

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

    // If we loaded the whole l-value, then insert our r-value
    if (! lValueAggregate.isClear())
        lastValue = glaBuilder->createInsertValue(lValueAggregate, lastValue, indices, indexCount);

    // Store the last value into the l-value, using destination we tracked in base.
    lastValue = glaBuilder->createStore(lastValue, lValue.base);

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_call *call)
{
    exec_list_iterator iter = call->actual_parameters.iterator();

    llvm::SmallVector<llvm::Value*, 4> llvmParams;

    ir_rvalue *param = NULL;
    int paramCount = 0;

    // Build a list of arguments
    while(iter.has_next())
    {
        param = (ir_rvalue *) iter.get();
        param->accept(this);
        llvmParams.push_back(lastValue);

        paramCount++;
        iter.next();
    }

    if(call->get_callee()->function()->has_user_signature()) {
        // Grab the pointer from the previous created function
        llvm::Function* function = functionMap[call->get_callee()];
        assert(function);

        lastValue = llvmBuilder.Insert(llvm::CallInst::Create(function, &llvmParams[0], &llvmParams[paramCount]));
    } else {
        if (! strcmp(call->callee_name(), "mix")) {
            if(gla::GetBasicType(llvmParams[0])->isIntegerTy()) {
                lastValue = llvmBuilder.CreateSelect(llvmParams[2], llvmParams[0], llvmParams[1]);
            } else {
                lastValue = glaBuilder->createIntrinsicCall(llvm::Intrinsic::gla_fMix, llvmParams[0], llvmParams[1], llvmParams[2]);
            }
        } else {
            gla::UnsupportedFunctionality("Built-in function: ", gla::EATContinue);
            gla::UnsupportedFunctionality(call->callee_name());
        }
    }

    return visit_continue_with_parent;
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
    if (inMain)
        glaBuilder->makeMainReturn();
    else if (ir->get_value())
        glaBuilder->makeReturn(false, lastValue);
    else
        glaBuilder->makeReturn();

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
    glaBuilder->makeDiscard(inMain);

    lastValue.clear();

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_if *ifNode)
{
    // emit condition into current block
    ifNode->condition->accept(this);
    gla::Builder::If ifBuilder(lastValue, glaBuilder);

    // emit the then statement
    visit_list_elements(this, &(ifNode->then_instructions));

    // emit the else statement
    if (! ifNode->else_instructions.is_empty()) {
        ifBuilder.makeBeginElse();
        visit_list_elements(this, &(ifNode->else_instructions));
    }

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
        storageQualifier = gla::Builder::ESQResource;
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
    if (var->type->sampler_shadow) {
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

gla::Builder::SuperValue GlslToTopVisitor::createUnaryMatrixOperation(ir_expression_operation op, gla::Builder::SuperValue operand)
{
    gla::Builder::SuperValue result;

    switch(op) {
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
        break;
    }

    return result;
}

gla::Builder::SuperValue GlslToTopVisitor::createBinaryMatrixOperation(ir_expression_operation op, gla::Builder::SuperValue lhs, gla::Builder::SuperValue rhs)
{
    gla::Builder::SuperValue result;

    llvm::Instruction::BinaryOps llvmOp = llvm::Instruction::BinaryOps(0);
    bool componentWise = true;

    switch(op) {
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
        return glaBuilder->createMatrixCompare(lhs, rhs, true);
    case ir_binop_any_nequal:
        return glaBuilder->createMatrixCompare(lhs, rhs, false);
    default:
        gla::UnsupportedFunctionality("Matrix operation");
    }

    if (componentWise)
        return glaBuilder->createMatrixOp(llvmOp, lhs, rhs);
    else
        return glaBuilder->createMatrixMultiply(lhs, rhs);

    return result;
}


gla::Builder::SuperValue GlslToTopVisitor::createUnaryOperation(ir_expression_operation op, const glsl_type* destType, gla::Builder::SuperValue operand, bool isFloat, bool isSigned)
{
    gla::Builder::SuperValue result;

    // Cast ops
    llvm::Instruction::CastOps castOp = llvm::Instruction::CastOps(0);
    switch(op) {
    case ir_unop_f2i:
        castOp = llvm::Instruction::FPToSI;
        break;
    case ir_unop_i2f:
        castOp = llvm::Instruction::SIToFP;
        break;
    case ir_unop_f2b:
        castOp = llvm::Instruction::FPToUI;
        break;
    case ir_unop_b2f:
        castOp = llvm::Instruction::UIToFP;
        break;
    case ir_unop_i2b:
        // any non-zero integer should return true
        return createBinaryOperation(ir_binop_nequal, operand, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0), false, false);
    case ir_unop_b2i:
    //?? case ir_unop_f2u seems missing
        castOp = llvm::Instruction::ZExt;
        break;
    case ir_unop_u2f:
        castOp = llvm::Instruction::UIToFP;
        break;
    }

    if (castOp != 0)
        return llvmBuilder.CreateCast(castOp, operand, convertGlslToGlaType(destType));

    // Unary ops that map to llvm operations
    switch (op) {
    case ir_unop_neg:
        if (isFloat)
            return llvmBuilder.CreateFNeg(operand);
        else
            return llvmBuilder.CreateNeg (operand);
    case ir_unop_logic_not:
    case ir_unop_bit_not:
        return llvmBuilder.CreateNot(operand);
    case ir_unop_rcp:
        return glaBuilder->createRecip(operand);
    }

    return result;
}

gla::Builder::SuperValue GlslToTopVisitor::createUnaryIntrinsic(ir_expression_operation op, gla::Builder::SuperValue operand, bool isFloat, bool isSigned)
{
    // Unary ops that require an intrinsic
    gla::Builder::SuperValue result;
    llvm::Intrinsic::ID intrinsicID = llvm::Intrinsic::ID(0);

    switch(op) {
    case ir_unop_sin:
        intrinsicID = llvm::Intrinsic::gla_fSin;
        break;
    case ir_unop_cos:
        intrinsicID = llvm::Intrinsic::gla_fCos;
        break;
    case ir_unop_acos:
        intrinsicID = llvm::Intrinsic::gla_fAcos;
        break;
    case ir_unop_acosh:
        intrinsicID = llvm::Intrinsic::gla_fAcosh;
        break;
    case ir_unop_asin:
        intrinsicID = llvm::Intrinsic::gla_fAsin;
        break;
    case ir_unop_asinh:
        intrinsicID = llvm::Intrinsic::gla_fAsinh;
        break;
    case ir_unop_atan:
        intrinsicID = llvm::Intrinsic::gla_fAtan;
        break;
    case ir_unop_atanh:
        intrinsicID = llvm::Intrinsic::gla_fAtanh;
        break;
    case ir_unop_tan:
        intrinsicID = llvm::Intrinsic::gla_fTan;
        break;
    case ir_unop_tanh:
        intrinsicID = llvm::Intrinsic::gla_fTanh;
        break;
    case ir_unop_cosh:
        intrinsicID = llvm::Intrinsic::gla_fCosh;
        break;
    case ir_unop_sinh:
        intrinsicID = llvm::Intrinsic::gla_fSinh;
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
    case ir_unop_round_even:
        intrinsicID = llvm::Intrinsic::gla_fRoundEven;
        break;
    case ir_unop_dFdx:
        intrinsicID = llvm::Intrinsic::gla_fDFdx;
        break;
    case ir_unop_dFdy:
        intrinsicID = llvm::Intrinsic::gla_fDFdy;
        break;
    case ir_unop_any:
        intrinsicID = llvm::Intrinsic::gla_any;
        break;
    case ir_unop_all:
        intrinsicID = llvm::Intrinsic::gla_all;
        break;
    case ir_unop_abs:
        if (isFloat)
            intrinsicID = llvm::Intrinsic::gla_fAbs;
        else
            intrinsicID = llvm::Intrinsic::gla_abs;
        break;
    case ir_unop_sign:
        if (isFloat)
            intrinsicID = llvm::Intrinsic::gla_fSign;
        else
            gla::UnsupportedFunctionality("Integer sign()");
        break;
    case ir_unop_trunc:
        intrinsicID = llvm::Intrinsic::gla_fRoundZero;
        break;
    }

    if (intrinsicID != 0)
        return glaBuilder->createIntrinsicCall(intrinsicID, operand);

    return result;
}

gla::Builder::SuperValue GlslToTopVisitor::createBinaryOperation(ir_expression_operation op, gla::Builder::SuperValue lhs, gla::Builder::SuperValue rhs, bool isFloat, bool isSigned)
{
    gla::Builder::SuperValue result;
    llvm::Instruction::BinaryOps binOp = llvm::Instruction::BinaryOps(0);
    bool needsPromotion = true;

    switch(op) {
    case ir_binop_add:
        if (isFloat)
            binOp = llvm::Instruction::FAdd;
        else
            binOp = llvm::Instruction::Add;
        break;
    case ir_binop_sub:
        if (isFloat)
            binOp = llvm::Instruction::FSub;
        else
            binOp = llvm::Instruction::Sub;
        break;
    case ir_binop_mul:
        if (isFloat)
            binOp = llvm::Instruction::FMul;
        else
            binOp = llvm::Instruction::Mul;
        break;
    case ir_binop_div:
        if (isFloat)
            binOp = llvm::Instruction::FDiv;
        else if (isSigned)
            binOp = llvm::Instruction::SDiv;
        else
            binOp = llvm::Instruction::UDiv;
        break;
    case ir_binop_lshift:
        binOp = llvm::Instruction::Shl;
        break;
    case ir_binop_rshift:
        binOp = llvm::Instruction::LShr;
        break;
    case ir_binop_bit_and:
        binOp = llvm::Instruction::And;
        break;
    case ir_binop_bit_or:
        binOp = llvm::Instruction::Or;
        break;
    case ir_binop_logic_xor:
    case ir_binop_bit_xor:
        binOp = llvm::Instruction::Xor;
        break;
    case ir_binop_logic_and:
        assert(gla::IsBoolean(lhs->getType()) && gla::IsScalar(lhs->getType()));
        assert(gla::IsBoolean(rhs->getType()) && gla::IsScalar(rhs->getType()));
        needsPromotion = false;
        binOp = llvm::Instruction::And;
        break;
    case ir_binop_mod:
        if (isFloat)
            binOp = llvm::Instruction::FRem;
        else if (isSigned)
            binOp = llvm::Instruction::SRem;
        else
            binOp = llvm::Instruction::URem;
        break;
    }

    if (binOp != 0) {
        if (needsPromotion)
            glaBuilder->promoteScalar(lhs, rhs);

        return llvmBuilder.CreateBinOp(binOp, lhs, rhs);
    }

    // Comparison instructions
    if (isFloat) {
        llvm::FCmpInst::Predicate pred = llvm::FCmpInst::Predicate(0);
        switch (op) {
        case ir_binop_less:
            pred = llvm::FCmpInst::FCMP_OLT;
            break;
        case ir_binop_greater:
            pred = llvm::FCmpInst::FCMP_OGT;
            break;
        case ir_binop_lequal:
            pred = llvm::FCmpInst::FCMP_OLE;
            break;
        case ir_binop_gequal:
            pred = llvm::FCmpInst::FCMP_OGE;
            break;
        case ir_binop_equal:
            pred = llvm::FCmpInst::FCMP_OEQ;
            break;
        case ir_binop_nequal:
            pred = llvm::FCmpInst::FCMP_ONE;
            break;
        }

        if (pred != 0)
            return llvmBuilder.CreateFCmp(pred, lhs, rhs);
    } else {
        llvm::ICmpInst::Predicate pred = llvm::ICmpInst::Predicate(0);
        if (isSigned) {
            switch (op) {
            case ir_binop_less:
                pred = llvm::ICmpInst::ICMP_SLT;
                break;
            case ir_binop_greater:
                pred = llvm::ICmpInst::ICMP_SGT;
                break;
            case ir_binop_lequal:
                pred = llvm::ICmpInst::ICMP_SLE;
                break;
            case ir_binop_gequal:
                pred = llvm::ICmpInst::ICMP_SGE;
                break;
            case ir_binop_equal:
                pred = llvm::ICmpInst::ICMP_EQ;
                break;
            case ir_binop_nequal:
                pred = llvm::ICmpInst::ICMP_NE;
                break;
            }
        } else {
            switch (op) {
            case ir_binop_less:
                pred = llvm::ICmpInst::ICMP_ULT;
                break;
            case ir_binop_greater:
                pred = llvm::ICmpInst::ICMP_UGT;
                break;
            case ir_binop_lequal:
                pred = llvm::ICmpInst::ICMP_ULE;
                break;
            case ir_binop_gequal:
                pred = llvm::ICmpInst::ICMP_UGE;
                break;
            case ir_binop_equal:
                pred = llvm::ICmpInst::ICMP_EQ;
                break;
            case ir_binop_nequal:
                pred = llvm::ICmpInst::ICMP_NE;
                break;
            }
        }

        if (pred != 0)
            return llvmBuilder.CreateICmp(pred, lhs, rhs);
    }

    return result;
}

gla::Builder::SuperValue GlslToTopVisitor::createBinaryIntrinsic(ir_expression_operation op, gla::Builder::SuperValue lhs, gla::Builder::SuperValue rhs, bool isFloat, bool isSigned)
{
    // Binary ops that require an intrinsic
    gla::Builder::SuperValue result;
    llvm::Intrinsic::ID intrinsicID = llvm::Intrinsic::ID(0);

    switch (op) {
    case ir_binop_min:
        if (isFloat)
            intrinsicID = llvm::Intrinsic::gla_fMin;
        else
            intrinsicID = llvm::Intrinsic::gla_sMin;
        break;
    case ir_binop_max:
        if (isFloat)
            intrinsicID = llvm::Intrinsic::gla_fMax;
        else
            intrinsicID = llvm::Intrinsic::gla_sMax;
        break;
    case ir_binop_pow:
        if (isFloat)
            intrinsicID = llvm::Intrinsic::gla_fPow;
        else
            intrinsicID = llvm::Intrinsic::gla_fPowi;
        break;
    case ir_binop_dot:
        switch (gla::GetComponentCount(lhs)) {
        case 2:
            intrinsicID = llvm::Intrinsic::gla_fDot2;
            break;
        case 3:
            intrinsicID = llvm::Intrinsic::gla_fDot3;
            break;
        case 4:
            intrinsicID = llvm::Intrinsic::gla_fDot4;
            break;
        default:
            assert(! "bad component count for dot");
        }
        break;
    case ir_binop_atan2:
        intrinsicID = llvm::Intrinsic::gla_fAtan2;
        break;
    case ir_binop_ftransform:
        intrinsicID = llvm::Intrinsic::gla_fFixedTransform;
        break;
    case ir_binop_all_equal:
        return glaBuilder->createCompare(lhs, rhs, true, isFloat, isSigned);
    case ir_binop_any_nequal:
        return glaBuilder->createCompare(lhs, rhs, false, isFloat, isSigned);
    case ir_binop_outerProduct:
        return glaBuilder->createMatrixMultiply(lhs, rhs);
    }

    // If intrinsic was assigned, then call the function and return
    if (intrinsicID != 0)
        return glaBuilder->createIntrinsicCall(intrinsicID, lhs, rhs);

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
        return gla::Builder::Matrix::getType(glaType, type->matrix_columns, type->vector_elements);

    // If this variable has a vector element count greater than 1, create an LLVM vector
    unsigned vecCount = type->vector_elements;
    if(vecCount > 1)
        glaType = llvm::VectorType::get(glaType, vecCount);

    return glaType;
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
        gla::AppendArrayIndexToName(name, index);
    } else {
        readType = convertGlslToGlaType(var->type);
    }

    gla::EInterpolationMethod method = gla::EIMNone;
    if (glShader->Type == GL_FRAGMENT_SHADER) {
        switch (var->interpolation) {
        case ir_var_smooth:
            method = gla::EIMSmooth;
            break;
        case ir_var_noperspective:
            method = gla::EIMNoperspective;
            break;
        case ir_var_flat:
            method = gla::EIMNone;
            break;
        default:
            gla::UnsupportedFunctionality("interpolation mode");
        }
    }

    // Give each interpolant a temporary unique index
    return glaBuilder->readPipeline(readType, name, getNextInterpIndex(name), -1 /*mask*/, method);
}
