//===- GlslToTopVisitor.cpp - Help Translate GLSL IR to LunarGLASS Top IR -===//
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
// Author: John Kessenich, LunarG
// Author: Cody Northrop, LunarG
//
//===----------------------------------------------------------------------===//

#include "GlslToTopVisitor.h"
#include "LunarGLASSTopIR.h"
#include "mtypes.h"

void GlslToTop(struct gl_shader* glShader, llvm::Module* module)
{
    GlslToTopVisitor v(glShader, module);

    foreach_iter(exec_list_iterator, iter, *glShader->ir) {
        ir_instruction *ir = (ir_instruction *)iter.get();

        ir->accept(&v);
    }
}

GlslToTopVisitor::GlslToTopVisitor(struct gl_shader* s, llvm::Module* m) 
    : context(llvm::getGlobalContext()), builder(context), module(m), glShader(s)
{
}

GlslToTopVisitor::~GlslToTopVisitor()
{
}

ir_visitor_status
    GlslToTopVisitor::visit(ir_variable *variable)
{
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit(ir_constant *constant)
{
    lastValue = createLLVMConstant(constant);
    
    return visit_continue;
}

llvm::Value* GlslToTopVisitor::createLLVMConstant(ir_constant* constant)
{
    // XXXX Note that we are hard coding 32 bit integer and bool widths.  Query back end for desired width?
    llvm::Constant* llvmConstant;

    unsigned vecCount = constant->type->vector_elements;
    unsigned baseType = constant->type->base_type;

    if(vecCount > 1) {
        //Vectors require 
        std::vector<llvm::Constant*> vals;
        const llvm::VectorType *destVecTy;

        switch(baseType)
        {
        case GLSL_TYPE_UINT:
            destVecTy = llvm::VectorType::get((llvm::Type*)llvm::Type::getInt32Ty(context), vecCount);
            for(int i = 0; i < vecCount; ++i)
                vals.push_back(llvm::ConstantInt::get(context, llvm::APInt(32, (uint64_t)constant->value.u[i], false)));
            break;
        case GLSL_TYPE_INT:
            destVecTy = llvm::VectorType::get((llvm::Type*)llvm::Type::getInt32Ty(context), vecCount);
            for(int i = 0; i < vecCount; ++i)
                vals.push_back(llvm::ConstantInt::get(context, llvm::APInt(32, (uint64_t)constant->value.i[i], true)));
            break;
        case GLSL_TYPE_FLOAT:
            destVecTy = llvm::VectorType::get((llvm::Type*)llvm::Type::getFloatTy(context), vecCount);
            for(int i = 0; i < vecCount; ++i)
                vals.push_back(llvm::ConstantFP::get(context, llvm::APFloat(constant->value.f[i])));
            break;
        case GLSL_TYPE_BOOL:
            //Treat bools as ints for now
            //?? Query the backend for bool width?
            destVecTy = llvm::VectorType::get((llvm::Type*)llvm::Type::getInt32Ty(context), vecCount);
            for(int i = 0; i < vecCount; ++i)
                vals.push_back(llvm::ConstantInt::get(context, llvm::APInt(32, (uint64_t)constant->value.u[i], false)));
            break;
        case GLSL_TYPE_SAMPLER:
        case GLSL_TYPE_ARRAY:
        case GLSL_TYPE_STRUCT:
        case GLSL_TYPE_FUNCTION:
        case GLSL_TYPE_VOID:
        case GLSL_TYPE_ERROR:
        default:
            assert(!"Unknown or unsupported GLSL varType");
            break;
        }

        llvmConstant = llvm::ConstantVector::get(destVecTy, vals);
    }
    else {
        switch(baseType)
        {
        case GLSL_TYPE_UINT:
            llvmConstant = llvm::ConstantInt::get(context, llvm::APInt(32, (uint64_t)constant->value.u[0], false));
            break;
        case GLSL_TYPE_INT:
            llvmConstant = llvm::ConstantInt::get(context, llvm::APInt(32, (uint64_t)constant->value.i[0], true));
            break;
        case GLSL_TYPE_FLOAT:
            llvmConstant = llvm::ConstantFP::get(context, llvm::APFloat(constant->value.f[0]));
            break;
        case GLSL_TYPE_BOOL:
            //Treat bools as ints for now
            //?? Query the backend for bool width?
           llvmConstant = llvm::ConstantInt::get(context, llvm::APInt(32, (uint64_t)constant->value.u[0], false));
            break;
        case GLSL_TYPE_SAMPLER:
        case GLSL_TYPE_ARRAY:
        case GLSL_TYPE_STRUCT:
        case GLSL_TYPE_FUNCTION:
        case GLSL_TYPE_VOID:
        case GLSL_TYPE_ERROR:
        default:
            assert(!"Unknown or unsupported GLSL varType");
            break;
        }
    }
    
    return llvmConstant;
}

ir_visitor_status
    GlslToTopVisitor::visit(ir_loop_jump *ir)
{
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit(ir_dereference_variable *derefVariable)
{
    // Grab the actual variable
    ir_variable *var = derefVariable->variable_referenced();

    // for pipeline reads, we don't create an LLVM variable storage
    if (var->mode != ir_var_in) {

        // Search our value map for existing entry
        std::map<ir_variable*, llvm::Value*>::iterator iter;
        iter = namedValues.find(var);

        if (namedValues.end() == iter) {

            // it was not found, create it
            namedValues[var] = createLLVMVariable(var);

            // For pipeline outputs, we must still maintain a non-pipeline
            // variable for reading/writing that happens before the final 
            // copy out.  Make this current variable be that non-pipeline
            // normal variable, but track it as one that now needs a copy out on
            // shader exit.
            if (var->mode == ir_var_out) {            
                // Track our copy-out for pipe write
                glslOuts.push_back(namedValues[var]);
            }
        }

        // Track the current value
        lastValue = namedValues[var];
    }

    if (in_assignee)
    {
        // Track the dest so we can Store later
        lValue = lastValue;
    }
    else
    {   
        if (var->mode == ir_var_in) {                    
            // For pipeline inputs, and we will generate a fresh pipeline read at each reference, 
            // which we will optimize later.
            llvm::Function *intrinsicName = 0;
            const char *name = NULL;

            // TODO:  We're hard coding our output location to 0 for bringup
            llvm::Constant *llvmConstant = llvm::ConstantInt::get(context, llvm::APInt(32, 0, true));

            // Select intrinsic based on target stage
            if(glShader->Type == GL_FRAGMENT_SHADER) {
                intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::gla_getInterpolant,
                                                        convertGLSLToLLVMType(var->type));
                name = var->name;
            } else {
                assert(!"getAttribute not supported yet for VS");
            }

            // Call the selected intrinsic
            lastValue = builder.CreateCall (intrinsicName, llvmConstant, name);            
        } else {
            lastValue = builder.CreateLoad(lastValue);
        }
    }

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_loop *ir)
{
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_loop *ir)
{
    (void) ir;
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_function_signature *)
{
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_function_signature *)
{
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_function *f)
{
    if (f->has_user_signature()) {
        //?? still need to use correct signature, using only "void fun(void)"
        llvm::FunctionType *functionType = llvm::FunctionType::get(llvm::Type::getVoidTy(context), false);
        llvm::Function *function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, f->name, module);
        llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(context, "entry", function);
        builder.SetInsertPoint(entryBlock);  
    }

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_function *f)
{
    if (f->has_user_signature()) {
        if(!strcmp(f->name, "main")) {
            //Call writeData intrinsic on our outs
            while(!glslOuts.empty()) {
                llvm::Value* loadVal = builder.CreateLoad(glslOuts.front());
                
                llvm::Function *intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::gla_writeData,
                                                         loadVal->getType());

                lastValue = builder.CreateCall2 (intrinsicName,
                                                 llvm::ConstantInt::get(context, llvm::APInt(32, 0, true)), 
                                                 loadVal);
                
                glslOuts.pop_front();
            }
        }

        //?? need to return the return value from the right places with the right type, this 
        // is good just for main
        builder.CreateRet(0);

        module->dump();
    }

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_expression *expression)
{
    lastValue = expandGLSLOp(expression);

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
    assert(!"Unhandled visitor ir_texture");
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_texture *ir)
{
    (void) ir;
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
    assert(!"Unhandled visitor ir_dereference_array");
    return visit_continue;
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
    assert(!"Unhandled visitor ir_dereference_record");
    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_dereference_record *ir)
{
    (void) ir;
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
    //Store the result, using dest we track in base class
    llvm::StoreInst *store = builder.CreateStore(lastValue, lValue);
    lastValue = store;

    return visit_continue;
}

ir_visitor_status
    GlslToTopVisitor::visit_enter(ir_call *call)
{
    if(call->get_callee()->function()->has_user_signature()) {
        assert(!"Can't handle user functions yet");
    } else {
        exec_list_iterator iter = call->actual_parameters.iterator();

        // Stick this somewhere that makes sense, with a real value
        #define GLA_MAX_PARAMETERS 10

        llvm::Value* llvmParams[GLA_MAX_PARAMETERS];
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

        llvm::Value* intrinsicValue = createLLVMIntrinsic(call, llvmParams, paramCount);

        // Track the return value for to be consumed by next instruction
        lastValue = intrinsicValue;
    }
            
    return visit_continue_with_parent;
}

llvm::Value* GlslToTopVisitor::createLLVMIntrinsic(ir_call *call, llvm::Value** llvmParams, int paramCount)
{    
    llvm::Function *intrinsicName = 0;
    char *name = NULL;
    gla::ETextureFlags texFlags = {0};
    llvm::Type* resultType = convertGLSLToLLVMType(call->type);

    #define GLA_MAX_PARAMETER 5
    llvm::Value* outParams[GLA_MAX_PARAMETER];
    for(int i = 0; i < GLA_MAX_PARAMETER; i++)
        outParams[i] = llvmParams[i];

    bool isBiased = false;
    llvm::Intrinsic::ID textureIntrinsicID = llvm::Intrinsic::gla_fTextureSample;
    gla::ESamplerType   samplerType;

    //?? Figure out a cleaner way to select the correct intrinsic
    //switch(call->get_callee()->function()->name)

    // Based on the name of the callee, create the appropriate intrinsicID
    if(!strcmp(call->callee_name(), "any")) {
        intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::gla_any, resultType);
        name = "anyTmp";
    }
    else if(!strcmp(call->callee_name(), "all")) {
        intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::gla_all, resultType);
        name = "allTmp";
    }
    else if(!strcmp(call->callee_name(), "sin")) {
        intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::sin, resultType);
        name = "sinTmp";
    }
    else if(!strcmp(call->callee_name(), "cos")) {
        intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::cos, resultType);
        name = "cosTmp";
    }
    else if(!strcmp(call->callee_name(), "pow")) {
        intrinsicName = getLLVMIntrinsicFunction1(llvm::Intrinsic::pow, resultType);
        name = "powTmp";
    }
    else if(!strcmp(call->callee_name(), "texture1D")) {
        samplerType    = gla::ESampler1D;
        texFlags.EBias = (paramCount > 2) ? true : false;
        name           = "texture1DTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture1DProj")) {
        samplerType         = gla::ESampler1D;
        texFlags.EProjected = true;
        texFlags.EBias      = (paramCount > 2) ? true : false;
        name                = "texture1DProjTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture1DLod")) {
        textureIntrinsicID   = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType   = gla::ESampler1D;
        texFlags.ELod = true;
        name          = "texture1DLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture1DProjLod")) {
        textureIntrinsicID   = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType   = gla::ESampler1D;
        texFlags.ELod = true;
        name          = "texture1DLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture2D")) {
        samplerType      = gla::ESampler2D;
        texFlags.EBias   = (paramCount > 2) ? true : false;
        name             = "texture2DTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    } 
    else if(!strcmp(call->callee_name(), "texture2DProj")) {
        samplerType         = gla::ESampler2D;
        texFlags.EProjected = true;
        texFlags.EBias      = (paramCount > 2) ? true : false;
        name                = "texture2DProjTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture2DLod")) {
        textureIntrinsicID  = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType         = gla::ESampler2D;
        texFlags.ELod       = true;
        name                = "texture2DLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture2DProjLod")) {
        textureIntrinsicID  = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType         = gla::ESampler2D;
        texFlags.EProjected = true;
        texFlags.ELod       = true;
        name                = "texture2DProjLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture3D")) {
        samplerType     = gla::ESampler3D;
        texFlags.EBias  = (paramCount > 2) ? true : false;
        name            = "texture3DTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    } 
    else if(!strcmp(call->callee_name(), "texture3DProj")) {
        samplerType         = gla::ESampler3D;
        texFlags.EProjected = true;
        texFlags.EBias      = (paramCount > 2) ? true : false;
        name                = "texture3DProjTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture3DLod")) {
        samplerType     = gla::ESampler3D;
        texFlags.ELod   = true;
        name            = "texture3DLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "texture3DProjLod")) {
        textureIntrinsicID  = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType         = gla::ESampler3D;
        texFlags.EProjected = true;
        texFlags.ELod       = true;
        name                = "texture3DLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "textureCube")) {
        samplerType     = gla::ESamplerCube;
        texFlags.EBias  = (paramCount > 2) ? true : false;
        name            = "textureCubeTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "textureCubeLod ")) {
        textureIntrinsicID = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType        = gla::ESamplerCube;
        texFlags.ELod      = true;
        name               = "textureCubeLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "shadow1D")) {
        samplerType     = gla::ESampler1DShadow;
        texFlags.EBias  = (paramCount > 2) ? true : false;
        name            = "shadow1DTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "shadow2D")) {
        samplerType     = gla::ESampler2DShadow;
        texFlags.EBias  = (paramCount > 2) ? true : false;
        name            = "shadow2DTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "shadow1DProj")) {
        samplerType         = gla::ESampler1DShadow;
        texFlags.EProjected = true;
        texFlags.EBias      = (paramCount > 2) ? true : false;
        name                = "shadow1DProjTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "shadow2DProj")) {
        samplerType         = gla::ESampler2DShadow;
        texFlags.EProjected = true;
        texFlags.EBias      = (paramCount > 2) ? true : false;
        name                = "shadow2DProjTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "shadow1DLod")) {
        textureIntrinsicID = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType        = gla::ESampler1DShadow;
        texFlags.ELod      = true;
        name               = "shadow1DLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "shadow2DLod")) {
        textureIntrinsicID = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType        = gla::ESampler2DShadow;
        texFlags.ELod      = true;
        name               = "shadow2DLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "shadow1DProjLod")) {
        textureIntrinsicID  = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType         = gla::ESampler1DShadow;
        texFlags.EProjected = true;
        texFlags.ELod       = true;
        name                = "shadow1DProjLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "shadow2DProjLod")) {
        textureIntrinsicID  = llvm::Intrinsic::gla_fTextureSampleLod;
        samplerType         = gla::ESampler2DShadow;
        texFlags.EProjected = true;
        texFlags.ELod       = true;
        name                = "shadow2DProjLodTmp";
        createLLVMTextureIntrinsic(intrinsicName, paramCount, outParams, llvmParams, resultType, textureIntrinsicID, samplerType, texFlags);
    }
    else if(!strcmp(call->callee_name(), "mix")){
        intrinsicName = getLLVMIntrinsicFunction4(llvm::Intrinsic::gla_fMix, resultType, llvmParams[0]->getType(), llvmParams[1]->getType(), llvmParams[2]->getType());
        name = "mixTmp";
    }
    else {
        assert(!"Unsupported built-in");
    }
    
    llvm::CallInst *callInst = 0;

    // Create a call to it
    switch(paramCount)
    {
    case 5:
        callInst = builder.CreateCall5(intrinsicName, outParams[0] , outParams[1], outParams[2], outParams[3], outParams[4], name);
        break;
    case 4:
        callInst = builder.CreateCall4(intrinsicName, outParams[0] , outParams[1], outParams[2], outParams[3], name);
        break;
    case 3:
        callInst = builder.CreateCall3(intrinsicName, outParams[0] , outParams[1], outParams[2], name);
        break;
    case 2:
        callInst = builder.CreateCall2(intrinsicName, outParams[0], outParams[1], name);
        break;
    case 1:
        callInst = builder.CreateCall (intrinsicName, outParams[0], name);
        break;
    default:
        assert(!"Unsupported parameter count");
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
    (void) ir;
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
    lastValue = 0;
    ifNode->condition->accept(this);
    llvm::Value *condValue = lastValue;
    assert (condValue != 0);

    llvm::Function *function = builder.GetInsertBlock()->getParent();

    // make the blocks, but only put the then-block into the function, 
    // the else-block and merge-block will be added later, in order, after
    // earlier code is emitted
    bool haveElse = ! ifNode->else_instructions.is_empty();
    llvm::BasicBlock  *ThenBB = llvm::BasicBlock::Create(context, "then", function);
    llvm::BasicBlock  *ElseBB = haveElse ? llvm::BasicBlock::Create(context, "else") : 0;
    llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(context, "ifmerge");

    // make the flow control split
    if (haveElse)
        builder.CreateCondBr(condValue, ThenBB, ElseBB);
    else
        builder.CreateCondBr(condValue, ThenBB, MergeBB);

    builder.SetInsertPoint(ThenBB);

    // emit the then statement
    visit_list_elements(this, &(ifNode->then_instructions));
    
    // jump to the merge block
    builder.CreateBr(MergeBB);

    // emitting the then-block could change the current block, update
    ThenBB = builder.GetInsertBlock();

    // add else block to the function
    if (haveElse) {
        function->getBasicBlockList().push_back(ElseBB);
        builder.SetInsertPoint(ElseBB);

        // emit the else statement
        visit_list_elements(this, &(ifNode->else_instructions));

        // jump to the merge block
        builder.CreateBr(MergeBB);

        // emitting the else block could change the current block, update
        ElseBB = builder.GetInsertBlock();
    }

    // add the merge block to the function
    function->getBasicBlockList().push_back(MergeBB);
    builder.SetInsertPoint(MergeBB);

    // The glsl "value" of an if-else should never be taken (share code with "?:" though?)
    lastValue = 0;

    return visit_continue_with_parent;
}

ir_visitor_status
    GlslToTopVisitor::visit_leave(ir_if *ir)
{
    (void) ir;
    return visit_continue;
}

llvm::Value* GlslToTopVisitor::createLLVMVariable(ir_variable* var)
{
    unsigned int addressSpace = gla::GlobalAddressSpace;
    bool constant = var->read_only;
    bool local = false;
    llvm::Constant* initializer = 0;
    llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalVariable::InternalLinkage;
    llvm::Type *llvmVarType = convertGLSLToLLVMType(var->type);

    llvm::Value* value = 0;

    switch (var->mode) {
    case ir_var_auto:
        //?? want to distinguish between globals and locals
        //?? this will break for a global accessed by multiple functions
        // a global needs an initializer so it will get eliminated by LLVM
        local = true;
        break;

    case ir_var_uniform:
        // ?? need to generalize to N objects (constant buffers) for higher shader models
        // ?? link:  we need link info to know how large the memory object is
        addressSpace = gla::UniformAddressSpace;
        assert (var->read_only == true);
        linkage = llvm::GlobalVariable::ExternalLinkage;
        break;

    case ir_var_in:
        // inputs should all be pipeline reads
        assert(!"no memory allocations for inputs");
        return 0;

    case ir_var_out:
        // keep internal linkage, because epilogue will to the write out to the pipe
        // internal linkage helps with global optimizations, so does having an initializer
        initializer = llvm::Constant::getNullValue(llvmVarType);
        break;

    case ir_var_inout:
        // can only be for function parameters
        local = true;
        break;

    case ir_var_temporary:
        local = true;
        break;

    default:
        assert(!"Unhandled var->mode");
        break;
    }

    //?? still need to consume the following
    var->max_array_access;
    var->centroid;
    var->invariant;
    var->interpolation;
    var->origin_upper_left;
    var->pixel_center_integer;
    var->location;
    var->constant_value;

    if (local) {
        llvm::BasicBlock* entryBlock = &builder.GetInsertBlock()->getParent()->getEntryBlock();
        llvm::IRBuilder<> entryBuilder(entryBlock, entryBlock->begin());
        value = entryBuilder.CreateAlloca(llvmVarType, 0, var->name);
    } else {
        llvm::GlobalVariable* globalValue = new llvm::GlobalVariable(llvmVarType, constant, linkage,
                                         initializer, var->name, false /* ThreadLocal */, addressSpace);
        module->getGlobalList().push_back(globalValue);
        value = globalValue;
    }

    return value;
}

llvm::Value* GlslToTopVisitor::expandGLSLOp(ir_expression* opExp)
{	
    llvm::Value* operands[2];
    llvm::Value* result;

    int numOperands = opExp->get_num_operands();

    for (int i = 0; i < numOperands; ++i) {
        opExp->operands[i]->accept(this);
        operands[i] = lastValue;
    }

    ir_expression_operation glslOp = opExp->operation;

    switch(glslOp) {
    case ir_binop_add:
        result = builder.CreateFAdd(operands[0], operands[1], "fAddTmp");
        break;
    case ir_binop_sub:
        result = builder.CreateFSub(operands[0], operands[1], "fSubTmp");
        break;
    case ir_binop_mul:
        result = builder.CreateFMul(operands[0], operands[1], "fMulTmp");
        break;
    case ir_binop_div:
        result = builder.CreateFDiv(operands[0], operands[1], "fDivTmp");
        break;
    case ir_binop_less:
        result = builder.CreateFCmpOLT(operands[0], operands[1], "fCmpLTTmp");
        break;
    case ir_binop_greater:
        result = builder.CreateFCmpOGT(operands[0], operands[1], "fCmpGTTmp");
        break;
    case ir_binop_lequal:
        result = builder.CreateFCmpOLE(operands[0], operands[1], "fCmpLETmp");
        break;
    case ir_binop_gequal:
        result = builder.CreateFCmpOGE(operands[0], operands[1], "fCmpGETmp");
        break;
    case ir_binop_equal:
        result = builder.CreateFCmpOEQ(operands[0], operands[1], "fCmpEQTmp");
        break;
    case ir_binop_nequal:
        result = builder.CreateFCmpONE(operands[0], operands[1], "fCmpNETmp");
        break;
    case ir_binop_lshift:
        result = builder.CreateLShr(operands[0], operands[1], "shiftLeftLogical");
        break;
    case ir_binop_rshift:
        result = builder.CreateLShr(operands[0], operands[1], "shiftRightLogical");
        break;
    case ir_binop_logic_and:
        result = builder.CreateAnd(operands[0], operands[1], "logicalAnd");
        break;
    case ir_binop_logic_xor:
        result = builder.CreateXor(operands[0], operands[1], "logicalXor");
        break;
    case ir_binop_logic_or:
        result = builder.CreateOr(operands[0], operands[1], "logicalOr");
        break;
    case ir_binop_min:
    case ir_binop_max:
    case ir_binop_pow:
    case ir_binop_dot:
    case ir_binop_cross:
    case ir_binop_bit_and:
    case ir_binop_bit_xor:
    case ir_binop_bit_or:
    case ir_binop_all_equal:
    case ir_binop_any_nequal:
    case ir_binop_mod:
    default:
        assert(!"Unknown opcode in expandBinaryOp");
    }

    return result;
}

int makeSwizzle(ir_swizzle_mask mask) {
    return (((mask.x)<<0) | ((mask.y)<<2) | ((mask.z)<<4) | ((mask.w)<<6));
}

llvm::Value* GlslToTopVisitor::expandGLSLSwizzle(ir_swizzle* swiz)
{	
    llvm::Value* operand;

    // traverse the tree we're swizzling
    swiz->val->accept(this);
    operand = lastValue;
 
    // convert our GLSL mask to an int
    int swizVal = makeSwizzle(swiz->mask);

    // swizzle needs the source type and dest type
    llvm::Function *intrinsicName = getLLVMIntrinsicFunction2(llvm::Intrinsic::gla_fSwizzle,
                                                             convertGLSLToLLVMType(swiz->type),
                                                             operand->getType());
    
    llvm::CallInst *callInst = builder.CreateCall2 (intrinsicName, 
                                                    operand, 
                                                    llvm::ConstantInt::get(context, llvm::APInt(32, swizVal, true)),
                                                    "swizzleTmp");

    return callInst;
}

llvm::Type* GlslToTopVisitor::convertGLSLToLLVMType(const glsl_type* type)
{
    const unsigned varType = type->base_type;
    //unsigned isMatrix = var->type->is_matrix();

    llvm::Type *llvmVarType;

    switch(varType) {
    case GLSL_TYPE_UINT:
        //?? How to do unsigned var in LLVM?
        llvmVarType = (llvm::Type*)llvm::Type::getInt32Ty(context);
        break;
    case GLSL_TYPE_INT:
        llvmVarType = (llvm::Type*)llvm::Type::getInt32Ty(context);
        break;
    case GLSL_TYPE_FLOAT:
        llvmVarType = (llvm::Type*)llvm::Type::getFloatTy(context);
        break;
    case GLSL_TYPE_BOOL:
        //Treat bools as ints for now
        //?? Query the backend for bool width?
        llvmVarType = (llvm::Type*)llvm::Type::getInt32Ty(context);
        break;
    case GLSL_TYPE_SAMPLER:
        //Treating sampler as an integer for now
        llvmVarType = (llvm::Type*)llvm::Type::getInt32Ty(context);
        break;
    case GLSL_TYPE_ARRAY:
    case GLSL_TYPE_STRUCT:
    case GLSL_TYPE_FUNCTION:
    case GLSL_TYPE_VOID:
    case GLSL_TYPE_ERROR:
    default:
        assert(!"Unknown or unsupported GLSL varType");
        break;
    }

    // If this variable has a vector element count greater than 1, create an LLVM vector
    unsigned vecCount = type->vector_elements;
    if(vecCount > 1)
        llvmVarType = llvm::VectorType::get(llvmVarType, vecCount);

    return llvmVarType;
}

llvm::Function* GlslToTopVisitor::getLLVMIntrinsicFunction1(llvm::Intrinsic::ID ID, const llvm::Type* type1)
{
    int intrinsicTypeCount = 1;
    const llvm::Type* intrinsicTypes[1] = {0};

    intrinsicTypes[0] = type1;

    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID, intrinsicTypes, intrinsicTypeCount);
}

llvm::Function* GlslToTopVisitor::getLLVMIntrinsicFunction2(llvm::Intrinsic::ID ID, const llvm::Type* type1, const llvm::Type* type2)
{
    int intrinsicTypeCount = 2;
    const llvm::Type* intrinsicTypes[2] = {0};

    intrinsicTypes[0] = type1;
    intrinsicTypes[1] = type2;

    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID, intrinsicTypes, intrinsicTypeCount);
}

llvm::Function* GlslToTopVisitor::getLLVMIntrinsicFunction3(llvm::Intrinsic::ID ID, const llvm::Type* type1, const llvm::Type* type2, const llvm::Type* type3)
{
    int intrinsicTypeCount = 3;
    const llvm::Type* intrinsicTypes[3] = {0};

    intrinsicTypes[0] = type1;
    intrinsicTypes[1] = type2;
    intrinsicTypes[2] = type3;

    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID, intrinsicTypes, intrinsicTypeCount);
}

llvm::Function* GlslToTopVisitor::getLLVMIntrinsicFunction4(llvm::Intrinsic::ID ID, const llvm::Type* type1, const llvm::Type* type2, const llvm::Type* type3, const llvm::Type* type4)
{
    int intrinsicTypeCount = 4;
    const llvm::Type* intrinsicTypes[4] = {0};

    intrinsicTypes[0] = type1;
    intrinsicTypes[1] = type2;
    intrinsicTypes[2] = type3;
    intrinsicTypes[3] = type4;

    // Look up the intrinsic
    return llvm::Intrinsic::getDeclaration(module, ID, intrinsicTypes, intrinsicTypeCount);
}

void GlslToTopVisitor::createLLVMTextureIntrinsic(llvm::Function* &intrinsicName, int &paramCount, 
                                                  llvm::Value** outParams, llvm::Value** llvmParams, llvm::Type* resultType, 
                                                  llvm::Intrinsic::ID intrinsicID, gla::ESamplerType samplerType, gla::ETextureFlags texFlags)
{
    bool isBiased = texFlags.EBias;

    switch(intrinsicID) {
    case llvm::Intrinsic::gla_fTextureSample:
            outParams[0] = llvm::ConstantInt::get(context, llvm::APInt(32, samplerType, true));
            outParams[1] = llvmParams[0];
            outParams[2] = llvm::ConstantInt::get(context, llvm::APInt(32, *(int*)&texFlags, true));  //flag enum
            outParams[3] = llvmParams[1];
            
            if (isBiased) {
                //Bias requires SampleLod with EBias flag
                intrinsicID = llvm::Intrinsic::gla_fTextureSampleLod;
                outParams[4] = llvmParams[2];
            }

            paramCount += 2;
            intrinsicName = getLLVMIntrinsicFunction2(intrinsicID, resultType, llvmParams[1]->getType());
            break;
    case llvm::Intrinsic::gla_fTextureSampleLod:
            outParams[0] = llvm::ConstantInt::get(context, llvm::APInt(32, samplerType, true));
            outParams[1] = llvmParams[0];
            outParams[2] = llvm::ConstantInt::get(context, llvm::APInt(32,  *(int*)&texFlags, true));  //flag enum
            outParams[3] = llvmParams[1];
            outParams[4] = llvmParams[2];
            paramCount += 2;
            intrinsicName = getLLVMIntrinsicFunction2(intrinsicID, resultType, llvmParams[1]->getType());
            break;
    default:
        assert(! "Unsupported texture intrinsic");
    }

    return;
}
