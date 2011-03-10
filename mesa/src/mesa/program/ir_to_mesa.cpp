/*
 * Copyright (C) 2005-2007  Brian Paul   All Rights Reserved.
 * Copyright (C) 2008  VMware, Inc.   All Rights Reserved.
 * Copyright © 2010 Intel Corporation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

/**
 * \file ir_to_mesa.cpp
 *
 * Translate GLSL IR to Mesa's gl_program representation.
 */

#include <stdio.h>
#include "main/compiler.h"
#include "ir.h"
#include "ir_visitor.h"
#include "ir_print_visitor.h"
#include "ir_expression_flattening.h"
#include "glsl_types.h"
#include "glsl_parser_extras.h"
#include "../glsl/program.h"
#include "ir_optimization.h"
#include "ast.h"

extern "C" {
#include "main/mtypes.h"
#include "main/shaderapi.h"
#include "main/shaderobj.h"
#include "main/uniforms.h"
#include "program/hash_table.h"
#include "program/prog_instruction.h"
#include "program/prog_optimize.h"
#include "program/prog_print.h"
#include "program/program.h"
#include "program/prog_uniform.h"
#include "program/prog_parameter.h"
#include "program/sampler.h"
}

static int swizzle_for_size(int size);

/**
 * This struct is a corresponding struct to Mesa prog_src_register, with
 * wider fields.
 */
typedef struct ir_to_mesa_src_reg {
   ir_to_mesa_src_reg(int file, int index, const glsl_type *type)
   {
      this->file = (gl_register_file) file;
      this->index = index;
      if (type && (type->is_scalar() || type->is_vector() || type->is_matrix()))
	 this->swizzle = swizzle_for_size(type->vector_elements);
      else
	 this->swizzle = SWIZZLE_XYZW;
      this->negate = 0;
      this->reladdr = NULL;
   }

   ir_to_mesa_src_reg()
   {
      this->file = PROGRAM_UNDEFINED;
      this->index = 0;
      this->swizzle = 0;
      this->negate = 0;
      this->reladdr = NULL;
   }

   gl_register_file file; /**< PROGRAM_* from Mesa */
   int index; /**< temporary index, VERT_ATTRIB_*, FRAG_ATTRIB_*, etc. */
   GLuint swizzle; /**< SWIZZLE_XYZWONEZERO swizzles from Mesa. */
   int negate; /**< NEGATE_XYZW mask from mesa */
   /** Register index should be offset by the integer in this reg. */
   ir_to_mesa_src_reg *reladdr;
} ir_to_mesa_src_reg;

typedef struct ir_to_mesa_dst_reg {
   int file; /**< PROGRAM_* from Mesa */
   int index; /**< temporary index, VERT_ATTRIB_*, FRAG_ATTRIB_*, etc. */
   int writemask; /**< Bitfield of WRITEMASK_[XYZW] */
   GLuint cond_mask:4;
   /** Register index should be offset by the integer in this reg. */
   ir_to_mesa_src_reg *reladdr;
} ir_to_mesa_dst_reg;

extern ir_to_mesa_src_reg ir_to_mesa_undef;

class ir_to_mesa_instruction : public exec_node {
public:
   /* Callers of this ralloc-based new need not call delete. It's
    * easier to just ralloc_free 'ctx' (or any of its ancestors). */
   static void* operator new(size_t size, void *ctx)
   {
      void *node;

      node = rzalloc_size(ctx, size);
      assert(node != NULL);

      return node;
   }

   enum prog_opcode op;
   ir_to_mesa_dst_reg dst_reg;
   ir_to_mesa_src_reg src_reg[3];
   /** Pointer to the ir source this tree came from for debugging */
   ir_instruction *ir;
   GLboolean cond_update;
   bool saturate;
   int sampler; /**< sampler index */
   int tex_target; /**< One of TEXTURE_*_INDEX */
   GLboolean tex_shadow;

   class function_entry *function; /* Set on OPCODE_CAL or OPCODE_BGNSUB */
};

class variable_storage : public exec_node {
public:
   variable_storage(ir_variable *var, gl_register_file file, int index)
      : file(file), index(index), var(var)
   {
      /* empty */
   }

   gl_register_file file;
   int index;
   ir_variable *var; /* variable that maps to this, if any */
};

class function_entry : public exec_node {
public:
   ir_function_signature *sig;

   /**
    * identifier of this function signature used by the program.
    *
    * At the point that Mesa instructions for function calls are
    * generated, we don't know the address of the first instruction of
    * the function body.  So we make the BranchTarget that is called a
    * small integer and rewrite them during set_branchtargets().
    */
   int sig_id;

   /**
    * Pointer to first instruction of the function body.
    *
    * Set during function body emits after main() is processed.
    */
   ir_to_mesa_instruction *bgn_inst;

   /**
    * Index of the first instruction of the function body in actual
    * Mesa IR.
    *
    * Set after convertion from ir_to_mesa_instruction to prog_instruction.
    */
   int inst;

   /** Storage for the return value. */
   ir_to_mesa_src_reg return_reg;
};

class ir_to_mesa_visitor : public ir_visitor {
public:
   ir_to_mesa_visitor();
   ~ir_to_mesa_visitor();

   function_entry *current_function;

   struct gl_context *ctx;
   struct gl_program *prog;
   struct gl_shader_program *shader_program;
   struct gl_shader_compiler_options *options;

   int next_temp;

   variable_storage *find_variable_storage(ir_variable *var);

   function_entry *get_function_signature(ir_function_signature *sig);

   ir_to_mesa_src_reg get_temp(const glsl_type *type);
   void reladdr_to_temp(ir_instruction *ir,
			ir_to_mesa_src_reg *reg, int *num_reladdr);

   struct ir_to_mesa_src_reg src_reg_for_float(float val);

   /**
    * \name Visit methods
    *
    * As typical for the visitor pattern, there must be one \c visit method for
    * each concrete subclass of \c ir_instruction.  Virtual base classes within
    * the hierarchy should not have \c visit methods.
    */
   /*@{*/
   virtual void visit(ir_variable *);
   virtual void visit(ir_loop *);
   virtual void visit(ir_loop_jump *);
   virtual void visit(ir_function_signature *);
   virtual void visit(ir_function *);
   virtual void visit(ir_expression *);
   virtual void visit(ir_swizzle *);
   virtual void visit(ir_dereference_variable  *);
   virtual void visit(ir_dereference_array *);
   virtual void visit(ir_dereference_record *);
   virtual void visit(ir_assignment *);
   virtual void visit(ir_constant *);
   virtual void visit(ir_call *);
   virtual void visit(ir_return *);
   virtual void visit(ir_discard *);
   virtual void visit(ir_texture *);
   virtual void visit(ir_if *);
   /*@}*/

   struct ir_to_mesa_src_reg result;

   /** List of variable_storage */
   exec_list variables;

   /** List of function_entry */
   exec_list function_signatures;
   int next_signature_id;

   /** List of ir_to_mesa_instruction */
   exec_list instructions;

   ir_to_mesa_instruction *ir_to_mesa_emit_op0(ir_instruction *ir,
					       enum prog_opcode op);

   ir_to_mesa_instruction *ir_to_mesa_emit_op1(ir_instruction *ir,
					       enum prog_opcode op,
					       ir_to_mesa_dst_reg dst,
					       ir_to_mesa_src_reg src0);

   ir_to_mesa_instruction *ir_to_mesa_emit_op2(ir_instruction *ir,
					       enum prog_opcode op,
					       ir_to_mesa_dst_reg dst,
					       ir_to_mesa_src_reg src0,
					       ir_to_mesa_src_reg src1);

   ir_to_mesa_instruction *ir_to_mesa_emit_op3(ir_instruction *ir,
					       enum prog_opcode op,
					       ir_to_mesa_dst_reg dst,
					       ir_to_mesa_src_reg src0,
					       ir_to_mesa_src_reg src1,
					       ir_to_mesa_src_reg src2);

   /**
    * Emit the correct dot-product instruction for the type of arguments
    *
    * \sa ir_to_mesa_emit_op2
    */
   void ir_to_mesa_emit_dp(ir_instruction *ir,
			   ir_to_mesa_dst_reg dst,
			   ir_to_mesa_src_reg src0,
			   ir_to_mesa_src_reg src1,
			   unsigned elements);

   void ir_to_mesa_emit_scalar_op1(ir_instruction *ir,
				   enum prog_opcode op,
				   ir_to_mesa_dst_reg dst,
				   ir_to_mesa_src_reg src0);

   void ir_to_mesa_emit_scalar_op2(ir_instruction *ir,
				   enum prog_opcode op,
				   ir_to_mesa_dst_reg dst,
				   ir_to_mesa_src_reg src0,
				   ir_to_mesa_src_reg src1);

   void emit_scs(ir_instruction *ir, enum prog_opcode op,
		 ir_to_mesa_dst_reg dst,
		 const ir_to_mesa_src_reg &src);

   GLboolean try_emit_mad(ir_expression *ir,
			  int mul_operand);
   GLboolean try_emit_sat(ir_expression *ir);

   void emit_swz(ir_expression *ir);

   bool process_move_condition(ir_rvalue *ir);

   void *mem_ctx;
};

ir_to_mesa_src_reg ir_to_mesa_undef = ir_to_mesa_src_reg(PROGRAM_UNDEFINED, 0, NULL);

ir_to_mesa_dst_reg ir_to_mesa_undef_dst = {
   PROGRAM_UNDEFINED, 0, SWIZZLE_NOOP, COND_TR, NULL,
};

ir_to_mesa_dst_reg ir_to_mesa_address_reg = {
   PROGRAM_ADDRESS, 0, WRITEMASK_X, COND_TR, NULL
};

static void
fail_link(struct gl_shader_program *prog, const char *fmt, ...) PRINTFLIKE(2, 3);

static void
fail_link(struct gl_shader_program *prog, const char *fmt, ...)
{
   va_list args;
   va_start(args, fmt);
   ralloc_vasprintf_append(&prog->InfoLog, fmt, args);
   va_end(args);

   prog->LinkStatus = GL_FALSE;
}

static int
swizzle_for_size(int size)
{
   int size_swizzles[4] = {
      MAKE_SWIZZLE4(SWIZZLE_X, SWIZZLE_X, SWIZZLE_X, SWIZZLE_X),
      MAKE_SWIZZLE4(SWIZZLE_X, SWIZZLE_Y, SWIZZLE_Y, SWIZZLE_Y),
      MAKE_SWIZZLE4(SWIZZLE_X, SWIZZLE_Y, SWIZZLE_Z, SWIZZLE_Z),
      MAKE_SWIZZLE4(SWIZZLE_X, SWIZZLE_Y, SWIZZLE_Z, SWIZZLE_W),
   };

   assert((size >= 1) && (size <= 4));
   return size_swizzles[size - 1];
}

ir_to_mesa_instruction *
ir_to_mesa_visitor::ir_to_mesa_emit_op3(ir_instruction *ir,
					enum prog_opcode op,
					ir_to_mesa_dst_reg dst,
					ir_to_mesa_src_reg src0,
					ir_to_mesa_src_reg src1,
					ir_to_mesa_src_reg src2)
{
   ir_to_mesa_instruction *inst = new(mem_ctx) ir_to_mesa_instruction();
   int num_reladdr = 0;

   /* If we have to do relative addressing, we want to load the ARL
    * reg directly for one of the regs, and preload the other reladdr
    * sources into temps.
    */
   num_reladdr += dst.reladdr != NULL;
   num_reladdr += src0.reladdr != NULL;
   num_reladdr += src1.reladdr != NULL;
   num_reladdr += src2.reladdr != NULL;

   reladdr_to_temp(ir, &src2, &num_reladdr);
   reladdr_to_temp(ir, &src1, &num_reladdr);
   reladdr_to_temp(ir, &src0, &num_reladdr);

   if (dst.reladdr) {
      ir_to_mesa_emit_op1(ir, OPCODE_ARL, ir_to_mesa_address_reg,
                          *dst.reladdr);

      num_reladdr--;
   }
   assert(num_reladdr == 0);

   inst->op = op;
   inst->dst_reg = dst;
   inst->src_reg[0] = src0;
   inst->src_reg[1] = src1;
   inst->src_reg[2] = src2;
   inst->ir = ir;

   inst->function = NULL;

   this->instructions.push_tail(inst);

   return inst;
}


ir_to_mesa_instruction *
ir_to_mesa_visitor::ir_to_mesa_emit_op2(ir_instruction *ir,
					enum prog_opcode op,
					ir_to_mesa_dst_reg dst,
					ir_to_mesa_src_reg src0,
					ir_to_mesa_src_reg src1)
{
   return ir_to_mesa_emit_op3(ir, op, dst, src0, src1, ir_to_mesa_undef);
}

ir_to_mesa_instruction *
ir_to_mesa_visitor::ir_to_mesa_emit_op1(ir_instruction *ir,
					enum prog_opcode op,
					ir_to_mesa_dst_reg dst,
					ir_to_mesa_src_reg src0)
{
   assert(dst.writemask != 0);
   return ir_to_mesa_emit_op3(ir, op, dst,
			      src0, ir_to_mesa_undef, ir_to_mesa_undef);
}

ir_to_mesa_instruction *
ir_to_mesa_visitor::ir_to_mesa_emit_op0(ir_instruction *ir,
					enum prog_opcode op)
{
   return ir_to_mesa_emit_op3(ir, op, ir_to_mesa_undef_dst,
			      ir_to_mesa_undef,
			      ir_to_mesa_undef,
			      ir_to_mesa_undef);
}

void
ir_to_mesa_visitor::ir_to_mesa_emit_dp(ir_instruction *ir,
				       ir_to_mesa_dst_reg dst,
				       ir_to_mesa_src_reg src0,
				       ir_to_mesa_src_reg src1,
				       unsigned elements)
{
   static const gl_inst_opcode dot_opcodes[] = {
      OPCODE_DP2, OPCODE_DP3, OPCODE_DP4
   };

   ir_to_mesa_emit_op3(ir, dot_opcodes[elements - 2],
		       dst, src0, src1, ir_to_mesa_undef);
}

inline ir_to_mesa_dst_reg
ir_to_mesa_dst_reg_from_src(ir_to_mesa_src_reg reg)
{
   ir_to_mesa_dst_reg dst_reg;

   dst_reg.file = reg.file;
   dst_reg.index = reg.index;
   dst_reg.writemask = WRITEMASK_XYZW;
   dst_reg.cond_mask = COND_TR;
   dst_reg.reladdr = reg.reladdr;

   return dst_reg;
}

inline ir_to_mesa_src_reg
ir_to_mesa_src_reg_from_dst(ir_to_mesa_dst_reg reg)
{
   return ir_to_mesa_src_reg(reg.file, reg.index, NULL);
}

/**
 * Emits Mesa scalar opcodes to produce unique answers across channels.
 *
 * Some Mesa opcodes are scalar-only, like ARB_fp/vp.  The src X
 * channel determines the result across all channels.  So to do a vec4
 * of this operation, we want to emit a scalar per source channel used
 * to produce dest channels.
 */
void
ir_to_mesa_visitor::ir_to_mesa_emit_scalar_op2(ir_instruction *ir,
					       enum prog_opcode op,
					       ir_to_mesa_dst_reg dst,
					       ir_to_mesa_src_reg orig_src0,
					       ir_to_mesa_src_reg orig_src1)
{
   int i, j;
   int done_mask = ~dst.writemask;

   /* Mesa RCP is a scalar operation splatting results to all channels,
    * like ARB_fp/vp.  So emit as many RCPs as necessary to cover our
    * dst channels.
    */
   for (i = 0; i < 4; i++) {
      GLuint this_mask = (1 << i);
      ir_to_mesa_instruction *inst;
      ir_to_mesa_src_reg src0 = orig_src0;
      ir_to_mesa_src_reg src1 = orig_src1;

      if (done_mask & this_mask)
	 continue;

      GLuint src0_swiz = GET_SWZ(src0.swizzle, i);
      GLuint src1_swiz = GET_SWZ(src1.swizzle, i);
      for (j = i + 1; j < 4; j++) {
	 /* If there is another enabled component in the destination that is
	  * derived from the same inputs, generate its value on this pass as
	  * well.
	  */
	 if (!(done_mask & (1 << j)) &&
	     GET_SWZ(src0.swizzle, j) == src0_swiz &&
	     GET_SWZ(src1.swizzle, j) == src1_swiz) {
	    this_mask |= (1 << j);
	 }
      }
      src0.swizzle = MAKE_SWIZZLE4(src0_swiz, src0_swiz,
				   src0_swiz, src0_swiz);
      src1.swizzle = MAKE_SWIZZLE4(src1_swiz, src1_swiz,
				  src1_swiz, src1_swiz);

      inst = ir_to_mesa_emit_op2(ir, op,
				 dst,
				 src0,
				 src1);
      inst->dst_reg.writemask = this_mask;
      done_mask |= this_mask;
   }
}

void
ir_to_mesa_visitor::ir_to_mesa_emit_scalar_op1(ir_instruction *ir,
					       enum prog_opcode op,
					       ir_to_mesa_dst_reg dst,
					       ir_to_mesa_src_reg src0)
{
   ir_to_mesa_src_reg undef = ir_to_mesa_undef;

   undef.swizzle = SWIZZLE_XXXX;

   ir_to_mesa_emit_scalar_op2(ir, op, dst, src0, undef);
}

/**
 * Emit an OPCODE_SCS instruction
 *
 * The \c SCS opcode functions a bit differently than the other Mesa (or
 * ARB_fragment_program) opcodes.  Instead of splatting its result across all
 * four components of the destination, it writes one value to the \c x
 * component and another value to the \c y component.
 *
 * \param ir        IR instruction being processed
 * \param op        Either \c OPCODE_SIN or \c OPCODE_COS depending on which
 *                  value is desired.
 * \param dst       Destination register
 * \param src       Source register
 */
void
ir_to_mesa_visitor::emit_scs(ir_instruction *ir, enum prog_opcode op,
			     ir_to_mesa_dst_reg dst,
			     const ir_to_mesa_src_reg &src)
{
   /* Vertex programs cannot use the SCS opcode.
    */
   if (this->prog->Target == GL_VERTEX_PROGRAM_ARB) {
      ir_to_mesa_emit_scalar_op1(ir, op, dst, src);
      return;
   }

   const unsigned component = (op == OPCODE_SIN) ? 0 : 1;
   const unsigned scs_mask = (1U << component);
   int done_mask = ~dst.writemask;
   ir_to_mesa_src_reg tmp;

   assert(op == OPCODE_SIN || op == OPCODE_COS);

   /* If there are compnents in the destination that differ from the component
    * that will be written by the SCS instrution, we'll need a temporary.
    */
   if (scs_mask != unsigned(dst.writemask)) {
      tmp = get_temp(glsl_type::vec4_type);
   }

   for (unsigned i = 0; i < 4; i++) {
      unsigned this_mask = (1U << i);
      ir_to_mesa_src_reg src0 = src;

      if ((done_mask & this_mask) != 0)
	 continue;

      /* The source swizzle specified which component of the source generates
       * sine / cosine for the current component in the destination.  The SCS
       * instruction requires that this value be swizzle to the X component.
       * Replace the current swizzle with a swizzle that puts the source in
       * the X component.
       */
      unsigned src0_swiz = GET_SWZ(src.swizzle, i);

      src0.swizzle = MAKE_SWIZZLE4(src0_swiz, src0_swiz,
				   src0_swiz, src0_swiz);
      for (unsigned j = i + 1; j < 4; j++) {
	 /* If there is another enabled component in the destination that is
	  * derived from the same inputs, generate its value on this pass as
	  * well.
	  */
	 if (!(done_mask & (1 << j)) &&
	     GET_SWZ(src0.swizzle, j) == src0_swiz) {
	    this_mask |= (1 << j);
	 }
      }

      if (this_mask != scs_mask) {
	 ir_to_mesa_instruction *inst;
	 ir_to_mesa_dst_reg tmp_dst = ir_to_mesa_dst_reg_from_src(tmp);

	 /* Emit the SCS instruction.
	  */
	 inst = ir_to_mesa_emit_op1(ir, OPCODE_SCS, tmp_dst, src0);
	 inst->dst_reg.writemask = scs_mask;

	 /* Move the result of the SCS instruction to the desired location in
	  * the destination.
	  */
	 tmp.swizzle = MAKE_SWIZZLE4(component, component,
				     component, component);
	 inst = ir_to_mesa_emit_op1(ir, OPCODE_SCS, dst, tmp);
	 inst->dst_reg.writemask = this_mask;
      } else {
	 /* Emit the SCS instruction to write directly to the destination.
	  */
	 ir_to_mesa_instruction *inst =
	    ir_to_mesa_emit_op1(ir, OPCODE_SCS, dst, src0);
	 inst->dst_reg.writemask = scs_mask;
      }

      done_mask |= this_mask;
   }
}

struct ir_to_mesa_src_reg
ir_to_mesa_visitor::src_reg_for_float(float val)
{
   ir_to_mesa_src_reg src_reg(PROGRAM_CONSTANT, -1, NULL);

   src_reg.index = _mesa_add_unnamed_constant(this->prog->Parameters,
					      &val, 1, &src_reg.swizzle);

   return src_reg;
}

static int
type_size(const struct glsl_type *type)
{
   unsigned int i;
   int size;

   switch (type->base_type) {
   case GLSL_TYPE_UINT:
   case GLSL_TYPE_INT:
   case GLSL_TYPE_FLOAT:
   case GLSL_TYPE_BOOL:
      if (type->is_matrix()) {
	 return type->matrix_columns;
      } else {
	 /* Regardless of size of vector, it gets a vec4. This is bad
	  * packing for things like floats, but otherwise arrays become a
	  * mess.  Hopefully a later pass over the code can pack scalars
	  * down if appropriate.
	  */
	 return 1;
      }
   case GLSL_TYPE_ARRAY:
      return type_size(type->fields.array) * type->length;
   case GLSL_TYPE_STRUCT:
      size = 0;
      for (i = 0; i < type->length; i++) {
	 size += type_size(type->fields.structure[i].type);
      }
      return size;
   case GLSL_TYPE_SAMPLER:
      /* Samplers take up one slot in UNIFORMS[], but they're baked in
       * at link time.
       */
      return 1;
   default:
      assert(0);
      return 0;
   }
}

/**
 * In the initial pass of codegen, we assign temporary numbers to
 * intermediate results.  (not SSA -- variable assignments will reuse
 * storage).  Actual register allocation for the Mesa VM occurs in a
 * pass over the Mesa IR later.
 */
ir_to_mesa_src_reg
ir_to_mesa_visitor::get_temp(const glsl_type *type)
{
   ir_to_mesa_src_reg src_reg;
   int swizzle[4];
   int i;

   src_reg.file = PROGRAM_TEMPORARY;
   src_reg.index = next_temp;
   src_reg.reladdr = NULL;
   next_temp += type_size(type);

   if (type->is_array() || type->is_record()) {
      src_reg.swizzle = SWIZZLE_NOOP;
   } else {
      for (i = 0; i < type->vector_elements; i++)
	 swizzle[i] = i;
      for (; i < 4; i++)
	 swizzle[i] = type->vector_elements - 1;
      src_reg.swizzle = MAKE_SWIZZLE4(swizzle[0], swizzle[1],
				      swizzle[2], swizzle[3]);
   }
   src_reg.negate = 0;

   return src_reg;
}

variable_storage *
ir_to_mesa_visitor::find_variable_storage(ir_variable *var)
{
   
   variable_storage *entry;

   foreach_iter(exec_list_iterator, iter, this->variables) {
      entry = (variable_storage *)iter.get();

      if (entry->var == var)
	 return entry;
   }

   return NULL;
}

struct statevar_element {
   const char *field;
   int tokens[STATE_LENGTH];
   int swizzle;
};

static struct statevar_element gl_DepthRange_elements[] = {
   {"near", {STATE_DEPTH_RANGE, 0, 0}, SWIZZLE_XXXX},
   {"far", {STATE_DEPTH_RANGE, 0, 0}, SWIZZLE_YYYY},
   {"diff", {STATE_DEPTH_RANGE, 0, 0}, SWIZZLE_ZZZZ},
};

static struct statevar_element gl_ClipPlane_elements[] = {
   {NULL, {STATE_CLIPPLANE, 0, 0}, SWIZZLE_XYZW}
};

static struct statevar_element gl_Point_elements[] = {
   {"size", {STATE_POINT_SIZE}, SWIZZLE_XXXX},
   {"sizeMin", {STATE_POINT_SIZE}, SWIZZLE_YYYY},
   {"sizeMax", {STATE_POINT_SIZE}, SWIZZLE_ZZZZ},
   {"fadeThresholdSize", {STATE_POINT_SIZE}, SWIZZLE_WWWW},
   {"distanceConstantAttenuation", {STATE_POINT_ATTENUATION}, SWIZZLE_XXXX},
   {"distanceLinearAttenuation", {STATE_POINT_ATTENUATION}, SWIZZLE_YYYY},
   {"distanceQuadraticAttenuation", {STATE_POINT_ATTENUATION}, SWIZZLE_ZZZZ},
};

static struct statevar_element gl_FrontMaterial_elements[] = {
   {"emission", {STATE_MATERIAL, 0, STATE_EMISSION}, SWIZZLE_XYZW},
   {"ambient", {STATE_MATERIAL, 0, STATE_AMBIENT}, SWIZZLE_XYZW},
   {"diffuse", {STATE_MATERIAL, 0, STATE_DIFFUSE}, SWIZZLE_XYZW},
   {"specular", {STATE_MATERIAL, 0, STATE_SPECULAR}, SWIZZLE_XYZW},
   {"shininess", {STATE_MATERIAL, 0, STATE_SHININESS}, SWIZZLE_XXXX},
};

static struct statevar_element gl_BackMaterial_elements[] = {
   {"emission", {STATE_MATERIAL, 1, STATE_EMISSION}, SWIZZLE_XYZW},
   {"ambient", {STATE_MATERIAL, 1, STATE_AMBIENT}, SWIZZLE_XYZW},
   {"diffuse", {STATE_MATERIAL, 1, STATE_DIFFUSE}, SWIZZLE_XYZW},
   {"specular", {STATE_MATERIAL, 1, STATE_SPECULAR}, SWIZZLE_XYZW},
   {"shininess", {STATE_MATERIAL, 1, STATE_SHININESS}, SWIZZLE_XXXX},
};

static struct statevar_element gl_LightSource_elements[] = {
   {"ambient", {STATE_LIGHT, 0, STATE_AMBIENT}, SWIZZLE_XYZW},
   {"diffuse", {STATE_LIGHT, 0, STATE_DIFFUSE}, SWIZZLE_XYZW},
   {"specular", {STATE_LIGHT, 0, STATE_SPECULAR}, SWIZZLE_XYZW},
   {"position", {STATE_LIGHT, 0, STATE_POSITION}, SWIZZLE_XYZW},
   {"halfVector", {STATE_LIGHT, 0, STATE_HALF_VECTOR}, SWIZZLE_XYZW},
   {"spotDirection", {STATE_LIGHT, 0, STATE_SPOT_DIRECTION}, SWIZZLE_XYZW},
   {"spotCosCutoff", {STATE_LIGHT, 0, STATE_SPOT_DIRECTION}, SWIZZLE_WWWW},
   {"spotCutoff", {STATE_LIGHT, 0, STATE_SPOT_CUTOFF}, SWIZZLE_XXXX},
   {"spotExponent", {STATE_LIGHT, 0, STATE_ATTENUATION}, SWIZZLE_WWWW},
   {"constantAttenuation", {STATE_LIGHT, 0, STATE_ATTENUATION}, SWIZZLE_XXXX},
   {"linearAttenuation", {STATE_LIGHT, 0, STATE_ATTENUATION}, SWIZZLE_YYYY},
   {"quadraticAttenuation", {STATE_LIGHT, 0, STATE_ATTENUATION}, SWIZZLE_ZZZZ},
};

static struct statevar_element gl_LightModel_elements[] = {
   {"ambient", {STATE_LIGHTMODEL_AMBIENT, 0}, SWIZZLE_XYZW},
};

static struct statevar_element gl_FrontLightModelProduct_elements[] = {
   {"sceneColor", {STATE_LIGHTMODEL_SCENECOLOR, 0}, SWIZZLE_XYZW},
};

static struct statevar_element gl_BackLightModelProduct_elements[] = {
   {"sceneColor", {STATE_LIGHTMODEL_SCENECOLOR, 1}, SWIZZLE_XYZW},
};

static struct statevar_element gl_FrontLightProduct_elements[] = {
   {"ambient", {STATE_LIGHTPROD, 0, 0, STATE_AMBIENT}, SWIZZLE_XYZW},
   {"diffuse", {STATE_LIGHTPROD, 0, 0, STATE_DIFFUSE}, SWIZZLE_XYZW},
   {"specular", {STATE_LIGHTPROD, 0, 0, STATE_SPECULAR}, SWIZZLE_XYZW},
};

static struct statevar_element gl_BackLightProduct_elements[] = {
   {"ambient", {STATE_LIGHTPROD, 0, 1, STATE_AMBIENT}, SWIZZLE_XYZW},
   {"diffuse", {STATE_LIGHTPROD, 0, 1, STATE_DIFFUSE}, SWIZZLE_XYZW},
   {"specular", {STATE_LIGHTPROD, 0, 1, STATE_SPECULAR}, SWIZZLE_XYZW},
};

static struct statevar_element gl_TextureEnvColor_elements[] = {
   {NULL, {STATE_TEXENV_COLOR, 0}, SWIZZLE_XYZW},
};

static struct statevar_element gl_EyePlaneS_elements[] = {
   {NULL, {STATE_TEXGEN, 0, STATE_TEXGEN_EYE_S}, SWIZZLE_XYZW},
};

static struct statevar_element gl_EyePlaneT_elements[] = {
   {NULL, {STATE_TEXGEN, 0, STATE_TEXGEN_EYE_T}, SWIZZLE_XYZW},
};

static struct statevar_element gl_EyePlaneR_elements[] = {
   {NULL, {STATE_TEXGEN, 0, STATE_TEXGEN_EYE_R}, SWIZZLE_XYZW},
};

static struct statevar_element gl_EyePlaneQ_elements[] = {
   {NULL, {STATE_TEXGEN, 0, STATE_TEXGEN_EYE_Q}, SWIZZLE_XYZW},
};

static struct statevar_element gl_ObjectPlaneS_elements[] = {
   {NULL, {STATE_TEXGEN, 0, STATE_TEXGEN_OBJECT_S}, SWIZZLE_XYZW},
};

static struct statevar_element gl_ObjectPlaneT_elements[] = {
   {NULL, {STATE_TEXGEN, 0, STATE_TEXGEN_OBJECT_T}, SWIZZLE_XYZW},
};

static struct statevar_element gl_ObjectPlaneR_elements[] = {
   {NULL, {STATE_TEXGEN, 0, STATE_TEXGEN_OBJECT_R}, SWIZZLE_XYZW},
};

static struct statevar_element gl_ObjectPlaneQ_elements[] = {
   {NULL, {STATE_TEXGEN, 0, STATE_TEXGEN_OBJECT_Q}, SWIZZLE_XYZW},
};

static struct statevar_element gl_Fog_elements[] = {
   {"color", {STATE_FOG_COLOR}, SWIZZLE_XYZW},
   {"density", {STATE_FOG_PARAMS}, SWIZZLE_XXXX},
   {"start", {STATE_FOG_PARAMS}, SWIZZLE_YYYY},
   {"end", {STATE_FOG_PARAMS}, SWIZZLE_ZZZZ},
   {"scale", {STATE_FOG_PARAMS}, SWIZZLE_WWWW},
};

static struct statevar_element gl_NormalScale_elements[] = {
   {NULL, {STATE_NORMAL_SCALE}, SWIZZLE_XXXX},
};

#define MATRIX(name, statevar, modifier)			\
   static struct statevar_element name ## _elements[] = {		\
      { NULL, { statevar, 0, 0, 0, modifier}, SWIZZLE_XYZW },		\
      { NULL, { statevar, 0, 1, 1, modifier}, SWIZZLE_XYZW },		\
      { NULL, { statevar, 0, 2, 2, modifier}, SWIZZLE_XYZW },		\
      { NULL, { statevar, 0, 3, 3, modifier}, SWIZZLE_XYZW },		\
   }

MATRIX(gl_ModelViewMatrix,
       STATE_MODELVIEW_MATRIX, STATE_MATRIX_TRANSPOSE);
MATRIX(gl_ModelViewMatrixInverse,
       STATE_MODELVIEW_MATRIX, STATE_MATRIX_INVTRANS);
MATRIX(gl_ModelViewMatrixTranspose,
       STATE_MODELVIEW_MATRIX, 0);
MATRIX(gl_ModelViewMatrixInverseTranspose,
       STATE_MODELVIEW_MATRIX, STATE_MATRIX_INVERSE);

MATRIX(gl_ProjectionMatrix,
       STATE_PROJECTION_MATRIX, STATE_MATRIX_TRANSPOSE);
MATRIX(gl_ProjectionMatrixInverse,
       STATE_PROJECTION_MATRIX, STATE_MATRIX_INVTRANS);
MATRIX(gl_ProjectionMatrixTranspose,
       STATE_PROJECTION_MATRIX, 0);
MATRIX(gl_ProjectionMatrixInverseTranspose,
       STATE_PROJECTION_MATRIX, STATE_MATRIX_INVERSE);

MATRIX(gl_ModelViewProjectionMatrix,
       STATE_MVP_MATRIX, STATE_MATRIX_TRANSPOSE);
MATRIX(gl_ModelViewProjectionMatrixInverse,
       STATE_MVP_MATRIX, STATE_MATRIX_INVTRANS);
MATRIX(gl_ModelViewProjectionMatrixTranspose,
       STATE_MVP_MATRIX, 0);
MATRIX(gl_ModelViewProjectionMatrixInverseTranspose,
       STATE_MVP_MATRIX, STATE_MATRIX_INVERSE);

MATRIX(gl_TextureMatrix,
       STATE_TEXTURE_MATRIX, STATE_MATRIX_TRANSPOSE);
MATRIX(gl_TextureMatrixInverse,
       STATE_TEXTURE_MATRIX, STATE_MATRIX_INVTRANS);
MATRIX(gl_TextureMatrixTranspose,
       STATE_TEXTURE_MATRIX, 0);
MATRIX(gl_TextureMatrixInverseTranspose,
       STATE_TEXTURE_MATRIX, STATE_MATRIX_INVERSE);

static struct statevar_element gl_NormalMatrix_elements[] = {
   { NULL, { STATE_MODELVIEW_MATRIX, 0, 0, 0, STATE_MATRIX_INVERSE},
     SWIZZLE_XYZW },
   { NULL, { STATE_MODELVIEW_MATRIX, 0, 1, 1, STATE_MATRIX_INVERSE},
     SWIZZLE_XYZW },
   { NULL, { STATE_MODELVIEW_MATRIX, 0, 2, 2, STATE_MATRIX_INVERSE},
     SWIZZLE_XYZW },
};

#undef MATRIX

#define STATEVAR(name) {#name, name ## _elements, Elements(name ## _elements)}

static const struct statevar {
   const char *name;
   struct statevar_element *elements;
   unsigned int num_elements;
} statevars[] = {
   STATEVAR(gl_DepthRange),
   STATEVAR(gl_ClipPlane),
   STATEVAR(gl_Point),
   STATEVAR(gl_FrontMaterial),
   STATEVAR(gl_BackMaterial),
   STATEVAR(gl_LightSource),
   STATEVAR(gl_LightModel),
   STATEVAR(gl_FrontLightModelProduct),
   STATEVAR(gl_BackLightModelProduct),
   STATEVAR(gl_FrontLightProduct),
   STATEVAR(gl_BackLightProduct),
   STATEVAR(gl_TextureEnvColor),
   STATEVAR(gl_EyePlaneS),
   STATEVAR(gl_EyePlaneT),
   STATEVAR(gl_EyePlaneR),
   STATEVAR(gl_EyePlaneQ),
   STATEVAR(gl_ObjectPlaneS),
   STATEVAR(gl_ObjectPlaneT),
   STATEVAR(gl_ObjectPlaneR),
   STATEVAR(gl_ObjectPlaneQ),
   STATEVAR(gl_Fog),

   STATEVAR(gl_ModelViewMatrix),
   STATEVAR(gl_ModelViewMatrixInverse),
   STATEVAR(gl_ModelViewMatrixTranspose),
   STATEVAR(gl_ModelViewMatrixInverseTranspose),

   STATEVAR(gl_ProjectionMatrix),
   STATEVAR(gl_ProjectionMatrixInverse),
   STATEVAR(gl_ProjectionMatrixTranspose),
   STATEVAR(gl_ProjectionMatrixInverseTranspose),

   STATEVAR(gl_ModelViewProjectionMatrix),
   STATEVAR(gl_ModelViewProjectionMatrixInverse),
   STATEVAR(gl_ModelViewProjectionMatrixTranspose),
   STATEVAR(gl_ModelViewProjectionMatrixInverseTranspose),

   STATEVAR(gl_TextureMatrix),
   STATEVAR(gl_TextureMatrixInverse),
   STATEVAR(gl_TextureMatrixTranspose),
   STATEVAR(gl_TextureMatrixInverseTranspose),

   STATEVAR(gl_NormalMatrix),
   STATEVAR(gl_NormalScale),
};


void
ir_to_mesa_visitor::visit(ir_variable *ir)
{
   if (strcmp(ir->name, "gl_FragCoord") == 0) {
      struct gl_fragment_program *fp = (struct gl_fragment_program *)this->prog;

      fp->OriginUpperLeft = ir->origin_upper_left;
      fp->PixelCenterInteger = ir->pixel_center_integer;
   }

   if (ir->mode == ir_var_uniform && strncmp(ir->name, "gl_", 3) == 0) {
      unsigned int i;

      for (i = 0; i < Elements(statevars); i++) {
	 if (strcmp(ir->name, statevars[i].name) == 0)
	    break;
      }

      if (i == Elements(statevars)) {
	 fail_link(this->shader_program,
		   "Failed to find builtin uniform `%s'\n", ir->name);
	 return;
      }

      const struct statevar *statevar = &statevars[i];

      int array_count;
      if (ir->type->is_array()) {
	 array_count = ir->type->length;
      } else {
	 array_count = 1;
      }

      /* Check if this statevar's setup in the STATE file exactly
       * matches how we'll want to reference it as a
       * struct/array/whatever.  If not, then we need to move it into
       * temporary storage and hope that it'll get copy-propagated
       * out.
       */
      for (i = 0; i < statevar->num_elements; i++) {
	 if (statevar->elements[i].swizzle != SWIZZLE_XYZW) {
	    break;
	 }
      }

      struct variable_storage *storage;
      ir_to_mesa_dst_reg dst;
      if (i == statevar->num_elements) {
	 /* We'll set the index later. */
	 storage = new(mem_ctx) variable_storage(ir, PROGRAM_STATE_VAR, -1);
	 this->variables.push_tail(storage);

	 dst = ir_to_mesa_undef_dst;
      } else {
	 storage = new(mem_ctx) variable_storage(ir, PROGRAM_TEMPORARY,
						 this->next_temp);
	 this->variables.push_tail(storage);
	 this->next_temp += type_size(ir->type);

	 dst = ir_to_mesa_dst_reg_from_src(ir_to_mesa_src_reg(PROGRAM_TEMPORARY,
							      storage->index,
							      NULL));
      }


      for (int a = 0; a < array_count; a++) {
	 for (unsigned int i = 0; i < statevar->num_elements; i++) {
	    struct statevar_element *element = &statevar->elements[i];
	    int tokens[STATE_LENGTH];

	    memcpy(tokens, element->tokens, sizeof(element->tokens));
	    if (ir->type->is_array()) {
	       tokens[1] = a;
	    }

	    int index = _mesa_add_state_reference(this->prog->Parameters,
						  (gl_state_index *)tokens);

	    if (storage->file == PROGRAM_STATE_VAR) {
	       if (storage->index == -1) {
		  storage->index = index;
	       } else {
		  assert(index ==
                         (int)(storage->index + a * statevar->num_elements + i));
	       }
	    } else {
	       ir_to_mesa_src_reg src(PROGRAM_STATE_VAR, index, NULL);
	       src.swizzle = element->swizzle;
	       ir_to_mesa_emit_op1(ir, OPCODE_MOV, dst, src);
	       /* even a float takes up a whole vec4 reg in a struct/array. */
	       dst.index++;
	    }
	 }
      }
      if (storage->file == PROGRAM_TEMPORARY &&
	  dst.index != storage->index + type_size(ir->type)) {
	 fail_link(this->shader_program,
		   "failed to load builtin uniform `%s'  (%d/%d regs loaded)\n",
		   ir->name, dst.index - storage->index,
		   type_size(ir->type));
      }
   }
}

void
ir_to_mesa_visitor::visit(ir_loop *ir)
{
   ir_dereference_variable *counter = NULL;

   if (ir->counter != NULL)
      counter = new(ir) ir_dereference_variable(ir->counter);

   if (ir->from != NULL) {
      assert(ir->counter != NULL);

      ir_assignment *a = new(ir) ir_assignment(counter, ir->from, NULL);

      a->accept(this);
      delete a;
   }

   ir_to_mesa_emit_op0(NULL, OPCODE_BGNLOOP);

   if (ir->to) {
      ir_expression *e =
	 new(ir) ir_expression(ir->cmp, glsl_type::bool_type,
			       counter, ir->to);
      ir_if *if_stmt =  new(ir) ir_if(e);

      ir_loop_jump *brk = new(ir) ir_loop_jump(ir_loop_jump::jump_break);

      if_stmt->then_instructions.push_tail(brk);

      if_stmt->accept(this);

      delete if_stmt;
      delete e;
      delete brk;
   }

   visit_exec_list(&ir->body_instructions, this);

   if (ir->increment) {
      ir_expression *e =
	 new(ir) ir_expression(ir_binop_add, counter->type,
			       counter, ir->increment);

      ir_assignment *a = new(ir) ir_assignment(counter, e, NULL);

      a->accept(this);
      delete a;
      delete e;
   }

   ir_to_mesa_emit_op0(NULL, OPCODE_ENDLOOP);
}

void
ir_to_mesa_visitor::visit(ir_loop_jump *ir)
{
   switch (ir->mode) {
   case ir_loop_jump::jump_break:
      ir_to_mesa_emit_op0(NULL, OPCODE_BRK);
      break;
   case ir_loop_jump::jump_continue:
      ir_to_mesa_emit_op0(NULL, OPCODE_CONT);
      break;
   }
}


void
ir_to_mesa_visitor::visit(ir_function_signature *ir)
{
   assert(0);
   (void)ir;
}

void
ir_to_mesa_visitor::visit(ir_function *ir)
{
   /* Ignore function bodies other than main() -- we shouldn't see calls to
    * them since they should all be inlined before we get to ir_to_mesa.
    */
   if (strcmp(ir->name, "main") == 0) {
      const ir_function_signature *sig;
      exec_list empty;

      sig = ir->matching_signature(&empty);

      assert(sig);

      foreach_iter(exec_list_iterator, iter, sig->body) {
	 ir_instruction *ir = (ir_instruction *)iter.get();

	 ir->accept(this);
      }
   }
}

GLboolean
ir_to_mesa_visitor::try_emit_mad(ir_expression *ir, int mul_operand)
{
   int nonmul_operand = 1 - mul_operand;
   ir_to_mesa_src_reg a, b, c;

   ir_expression *expr = ir->operands[mul_operand]->as_expression();
   if (!expr || expr->operation != ir_binop_mul)
      return false;

   expr->operands[0]->accept(this);
   a = this->result;
   expr->operands[1]->accept(this);
   b = this->result;
   ir->operands[nonmul_operand]->accept(this);
   c = this->result;

   this->result = get_temp(ir->type);
   ir_to_mesa_emit_op3(ir, OPCODE_MAD,
		       ir_to_mesa_dst_reg_from_src(this->result), a, b, c);

   return true;
}

GLboolean
ir_to_mesa_visitor::try_emit_sat(ir_expression *ir)
{
   /* Saturates were only introduced to vertex programs in
    * NV_vertex_program3, so don't give them to drivers in the VP.
    */
   if (this->prog->Target == GL_VERTEX_PROGRAM_ARB)
      return false;

   ir_rvalue *sat_src = ir->as_rvalue_to_saturate();
   if (!sat_src)
      return false;

   sat_src->accept(this);
   ir_to_mesa_src_reg src = this->result;

   this->result = get_temp(ir->type);
   ir_to_mesa_instruction *inst;
   inst = ir_to_mesa_emit_op1(ir, OPCODE_MOV,
			      ir_to_mesa_dst_reg_from_src(this->result),
			      src);
   inst->saturate = true;

   return true;
}

void
ir_to_mesa_visitor::reladdr_to_temp(ir_instruction *ir,
				    ir_to_mesa_src_reg *reg, int *num_reladdr)
{
   if (!reg->reladdr)
      return;

   ir_to_mesa_emit_op1(ir, OPCODE_ARL, ir_to_mesa_address_reg, *reg->reladdr);

   if (*num_reladdr != 1) {
      ir_to_mesa_src_reg temp = get_temp(glsl_type::vec4_type);

      ir_to_mesa_emit_op1(ir, OPCODE_MOV,
			  ir_to_mesa_dst_reg_from_src(temp), *reg);
      *reg = temp;
   }

   (*num_reladdr)--;
}

void
ir_to_mesa_visitor::emit_swz(ir_expression *ir)
{
   /* Assume that the vector operator is in a form compatible with OPCODE_SWZ.
    * This means that each of the operands is either an immediate value of -1,
    * 0, or 1, or is a component from one source register (possibly with
    * negation).
    */
   uint8_t components[4] = { 0 };
   bool negate[4] = { false };
   ir_variable *var = NULL;

   for (unsigned i = 0; i < ir->type->vector_elements; i++) {
      ir_rvalue *op = ir->operands[i];

      assert(op->type->is_scalar());

      while (op != NULL) {
	 switch (op->ir_type) {
	 case ir_type_constant: {

	    assert(op->type->is_scalar());

	    const ir_constant *const c = op->as_constant();
	    if (c->is_one()) {
	       components[i] = SWIZZLE_ONE;
	    } else if (c->is_zero()) {
	       components[i] = SWIZZLE_ZERO;
	    } else if (c->is_negative_one()) {
	       components[i] = SWIZZLE_ONE;
	       negate[i] = true;
	    } else {
	       assert(!"SWZ constant must be 0.0 or 1.0.");
	    }

	    op = NULL;
	    break;
	 }

	 case ir_type_dereference_variable: {
	    ir_dereference_variable *const deref =
	       (ir_dereference_variable *) op;

	    assert((var == NULL) || (deref->var == var));
	    components[i] = SWIZZLE_X;
	    var = deref->var;
	    op = NULL;
	    break;
	 }

	 case ir_type_expression: {
	    ir_expression *const expr = (ir_expression *) op;

	    assert(expr->operation == ir_unop_neg);
	    negate[i] = true;

	    op = expr->operands[0];
	    break;
	 }

	 case ir_type_swizzle: {
	    ir_swizzle *const swiz = (ir_swizzle *) op;

	    components[i] = swiz->mask.x;
	    op = swiz->val;
	    break;
	 }

	 default:
	    assert(!"Should not get here.");
	    return;
	 }
      }
   }

   assert(var != NULL);

   ir_dereference_variable *const deref =
      new(mem_ctx) ir_dereference_variable(var);

   this->result.file = PROGRAM_UNDEFINED;
   deref->accept(this);
   if (this->result.file == PROGRAM_UNDEFINED) {
      ir_print_visitor v;
      printf("Failed to get tree for expression operand:\n");
      deref->accept(&v);
      exit(1);
   }

   ir_to_mesa_src_reg src;

   src = this->result;
   src.swizzle = MAKE_SWIZZLE4(components[0],
			       components[1],
			       components[2],
			       components[3]);
   src.negate = ((unsigned(negate[0]) << 0)
		 | (unsigned(negate[1]) << 1)
		 | (unsigned(negate[2]) << 2)
		 | (unsigned(negate[3]) << 3));

   /* Storage for our result.  Ideally for an assignment we'd be using the
    * actual storage for the result here, instead.
    */
   const ir_to_mesa_src_reg result_src = get_temp(ir->type);
   ir_to_mesa_dst_reg result_dst = ir_to_mesa_dst_reg_from_src(result_src);

   /* Limit writes to the channels that will be used by result_src later.
    * This does limit this temp's use as a temporary for multi-instruction
    * sequences.
    */
   result_dst.writemask = (1 << ir->type->vector_elements) - 1;

   ir_to_mesa_emit_op1(ir, OPCODE_SWZ, result_dst, src);
   this->result = result_src;
}

void
ir_to_mesa_visitor::visit(ir_expression *ir)
{
   unsigned int operand;
   struct ir_to_mesa_src_reg op[Elements(ir->operands)];
   struct ir_to_mesa_src_reg result_src;
   struct ir_to_mesa_dst_reg result_dst;

   /* Quick peephole: Emit OPCODE_MAD(a, b, c) instead of ADD(MUL(a, b), c)
    */
   if (ir->operation == ir_binop_add) {
      if (try_emit_mad(ir, 1))
	 return;
      if (try_emit_mad(ir, 0))
	 return;
   }
   if (try_emit_sat(ir))
      return;

   if (ir->operation == ir_quadop_vector) {
      this->emit_swz(ir);
      return;
   }

   for (operand = 0; operand < ir->get_num_operands(); operand++) {
      this->result.file = PROGRAM_UNDEFINED;
      ir->operands[operand]->accept(this);
      if (this->result.file == PROGRAM_UNDEFINED) {
	 ir_print_visitor v;
	 printf("Failed to get tree for expression operand:\n");
	 ir->operands[operand]->accept(&v);
	 exit(1);
      }
      op[operand] = this->result;

      /* Matrix expression operands should have been broken down to vector
       * operations already.
       */
      assert(!ir->operands[operand]->type->is_matrix());
   }

   int vector_elements = ir->operands[0]->type->vector_elements;
   if (ir->operands[1]) {
      vector_elements = MAX2(vector_elements,
			     ir->operands[1]->type->vector_elements);
   }

   this->result.file = PROGRAM_UNDEFINED;

   /* Storage for our result.  Ideally for an assignment we'd be using
    * the actual storage for the result here, instead.
    */
   result_src = get_temp(ir->type);
   /* convenience for the emit functions below. */
   result_dst = ir_to_mesa_dst_reg_from_src(result_src);
   /* Limit writes to the channels that will be used by result_src later.
    * This does limit this temp's use as a temporary for multi-instruction
    * sequences.
    */
   result_dst.writemask = (1 << ir->type->vector_elements) - 1;

   switch (ir->operation) {
   case ir_unop_logic_not:
      ir_to_mesa_emit_op2(ir, OPCODE_SEQ, result_dst,
			  op[0], src_reg_for_float(0.0));
      break;
   case ir_unop_neg:
      op[0].negate = ~op[0].negate;
      result_src = op[0];
      break;
   case ir_unop_abs:
      ir_to_mesa_emit_op1(ir, OPCODE_ABS, result_dst, op[0]);
      break;
   case ir_unop_sign:
      ir_to_mesa_emit_op1(ir, OPCODE_SSG, result_dst, op[0]);
      break;
   case ir_unop_rcp:
      ir_to_mesa_emit_scalar_op1(ir, OPCODE_RCP, result_dst, op[0]);
      break;

   case ir_unop_exp2:
      ir_to_mesa_emit_scalar_op1(ir, OPCODE_EX2, result_dst, op[0]);
      break;
   case ir_unop_exp:
   case ir_unop_log:
      assert(!"not reached: should be handled by ir_explog_to_explog2");
      break;
   case ir_unop_log2:
      ir_to_mesa_emit_scalar_op1(ir, OPCODE_LG2, result_dst, op[0]);
      break;
   case ir_unop_sin:
      ir_to_mesa_emit_scalar_op1(ir, OPCODE_SIN, result_dst, op[0]);
      break;
   case ir_unop_cos:
      ir_to_mesa_emit_scalar_op1(ir, OPCODE_COS, result_dst, op[0]);
      break;
   case ir_unop_sin_reduced:
      emit_scs(ir, OPCODE_SIN, result_dst, op[0]);
      break;
   case ir_unop_cos_reduced:
      emit_scs(ir, OPCODE_COS, result_dst, op[0]);
      break;

   case ir_unop_dFdx:
      ir_to_mesa_emit_op1(ir, OPCODE_DDX, result_dst, op[0]);
      break;
   case ir_unop_dFdy:
      ir_to_mesa_emit_op1(ir, OPCODE_DDY, result_dst, op[0]);
      break;

   case ir_unop_noise: {
      const enum prog_opcode opcode =
	 prog_opcode(OPCODE_NOISE1
		     + (ir->operands[0]->type->vector_elements) - 1);
      assert((opcode >= OPCODE_NOISE1) && (opcode <= OPCODE_NOISE4));

      ir_to_mesa_emit_op1(ir, opcode, result_dst, op[0]);
      break;
   }

   case ir_binop_add:
      ir_to_mesa_emit_op2(ir, OPCODE_ADD, result_dst, op[0], op[1]);
      break;
   case ir_binop_sub:
      ir_to_mesa_emit_op2(ir, OPCODE_SUB, result_dst, op[0], op[1]);
      break;

   case ir_binop_mul:
      ir_to_mesa_emit_op2(ir, OPCODE_MUL, result_dst, op[0], op[1]);
      break;
   case ir_binop_div:
      assert(!"not reached: should be handled by ir_div_to_mul_rcp");
   case ir_binop_mod:
      assert(!"ir_binop_mod should have been converted to b * fract(a/b)");
      break;

   case ir_binop_less:
      ir_to_mesa_emit_op2(ir, OPCODE_SLT, result_dst, op[0], op[1]);
      break;
   case ir_binop_greater:
      ir_to_mesa_emit_op2(ir, OPCODE_SGT, result_dst, op[0], op[1]);
      break;
   case ir_binop_lequal:
      ir_to_mesa_emit_op2(ir, OPCODE_SLE, result_dst, op[0], op[1]);
      break;
   case ir_binop_gequal:
      ir_to_mesa_emit_op2(ir, OPCODE_SGE, result_dst, op[0], op[1]);
      break;
   case ir_binop_equal:
      ir_to_mesa_emit_op2(ir, OPCODE_SEQ, result_dst, op[0], op[1]);
      break;
   case ir_binop_nequal:
      ir_to_mesa_emit_op2(ir, OPCODE_SNE, result_dst, op[0], op[1]);
      break;
   case ir_binop_all_equal:
      /* "==" operator producing a scalar boolean. */
      if (ir->operands[0]->type->is_vector() ||
	  ir->operands[1]->type->is_vector()) {
	 ir_to_mesa_src_reg temp = get_temp(glsl_type::vec4_type);
	 ir_to_mesa_emit_op2(ir, OPCODE_SNE,
			     ir_to_mesa_dst_reg_from_src(temp), op[0], op[1]);
	 ir_to_mesa_emit_dp(ir, result_dst, temp, temp, vector_elements);
	 ir_to_mesa_emit_op2(ir, OPCODE_SEQ,
			     result_dst, result_src, src_reg_for_float(0.0));
      } else {
	 ir_to_mesa_emit_op2(ir, OPCODE_SEQ, result_dst, op[0], op[1]);
      }
      break;
   case ir_binop_any_nequal:
      /* "!=" operator producing a scalar boolean. */
      if (ir->operands[0]->type->is_vector() ||
	  ir->operands[1]->type->is_vector()) {
	 ir_to_mesa_src_reg temp = get_temp(glsl_type::vec4_type);
	 ir_to_mesa_emit_op2(ir, OPCODE_SNE,
			     ir_to_mesa_dst_reg_from_src(temp), op[0], op[1]);
	 ir_to_mesa_emit_dp(ir, result_dst, temp, temp, vector_elements);
	 ir_to_mesa_emit_op2(ir, OPCODE_SNE,
			     result_dst, result_src, src_reg_for_float(0.0));
      } else {
	 ir_to_mesa_emit_op2(ir, OPCODE_SNE, result_dst, op[0], op[1]);
      }
      break;

   case ir_unop_any:
      assert(ir->operands[0]->type->is_vector());
      ir_to_mesa_emit_dp(ir, result_dst, op[0], op[0],
			 ir->operands[0]->type->vector_elements);
      ir_to_mesa_emit_op2(ir, OPCODE_SNE,
			  result_dst, result_src, src_reg_for_float(0.0));
      break;

   case ir_binop_logic_xor:
      ir_to_mesa_emit_op2(ir, OPCODE_SNE, result_dst, op[0], op[1]);
      break;

   case ir_binop_logic_or:
      /* This could be a saturated add and skip the SNE. */
      ir_to_mesa_emit_op2(ir, OPCODE_ADD,
			  result_dst,
			  op[0], op[1]);

      ir_to_mesa_emit_op2(ir, OPCODE_SNE,
			  result_dst,
			  result_src, src_reg_for_float(0.0));
      break;

   case ir_binop_logic_and:
      /* the bool args are stored as float 0.0 or 1.0, so "mul" gives us "and". */
      ir_to_mesa_emit_op2(ir, OPCODE_MUL,
			  result_dst,
			  op[0], op[1]);
      break;

   case ir_binop_dot:
      assert(ir->operands[0]->type->is_vector());
      assert(ir->operands[0]->type == ir->operands[1]->type);
      ir_to_mesa_emit_dp(ir, result_dst, op[0], op[1],
			 ir->operands[0]->type->vector_elements);
      break;

   case ir_unop_sqrt:
      /* sqrt(x) = x * rsq(x). */
      ir_to_mesa_emit_scalar_op1(ir, OPCODE_RSQ, result_dst, op[0]);
      ir_to_mesa_emit_op2(ir, OPCODE_MUL, result_dst, result_src, op[0]);
      /* For incoming channels <= 0, set the result to 0. */
      op[0].negate = ~op[0].negate;
      ir_to_mesa_emit_op3(ir, OPCODE_CMP, result_dst,
			  op[0], result_src, src_reg_for_float(0.0));
      break;
   case ir_unop_rsq:
      ir_to_mesa_emit_scalar_op1(ir, OPCODE_RSQ, result_dst, op[0]);
      break;
   case ir_unop_i2f:
   case ir_unop_b2f:
   case ir_unop_b2i:
      /* Mesa IR lacks types, ints are stored as truncated floats. */
      result_src = op[0];
      break;
   case ir_unop_f2i:
      ir_to_mesa_emit_op1(ir, OPCODE_TRUNC, result_dst, op[0]);
      break;
   case ir_unop_f2b:
   case ir_unop_i2b:
      ir_to_mesa_emit_op2(ir, OPCODE_SNE, result_dst,
			  op[0], src_reg_for_float(0.0));
      break;
   case ir_unop_trunc:
      ir_to_mesa_emit_op1(ir, OPCODE_TRUNC, result_dst, op[0]);
      break;
   case ir_unop_ceil:
      op[0].negate = ~op[0].negate;
      ir_to_mesa_emit_op1(ir, OPCODE_FLR, result_dst, op[0]);
      result_src.negate = ~result_src.negate;
      break;
   case ir_unop_floor:
      ir_to_mesa_emit_op1(ir, OPCODE_FLR, result_dst, op[0]);
      break;
   case ir_unop_fract:
      ir_to_mesa_emit_op1(ir, OPCODE_FRC, result_dst, op[0]);
      break;

   case ir_binop_min:
      ir_to_mesa_emit_op2(ir, OPCODE_MIN, result_dst, op[0], op[1]);
      break;
   case ir_binop_max:
      ir_to_mesa_emit_op2(ir, OPCODE_MAX, result_dst, op[0], op[1]);
      break;
   case ir_binop_pow:
      ir_to_mesa_emit_scalar_op2(ir, OPCODE_POW, result_dst, op[0], op[1]);
      break;

   case ir_unop_bit_not:
   case ir_unop_u2f:
   case ir_binop_lshift:
   case ir_binop_rshift:
   case ir_binop_bit_and:
   case ir_binop_bit_xor:
   case ir_binop_bit_or:
   case ir_unop_round_even:
      assert(!"GLSL 1.30 features unsupported");
      break;

   case ir_quadop_vector:
      /* This operation should have already been handled.
       */
      assert(!"Should not get here.");
      break;
   }

   this->result = result_src;
}


void
ir_to_mesa_visitor::visit(ir_swizzle *ir)
{
   ir_to_mesa_src_reg src_reg;
   int i;
   int swizzle[4];

   /* Note that this is only swizzles in expressions, not those on the left
    * hand side of an assignment, which do write masking.  See ir_assignment
    * for that.
    */

   ir->val->accept(this);
   src_reg = this->result;
   assert(src_reg.file != PROGRAM_UNDEFINED);

   for (i = 0; i < 4; i++) {
      if (i < ir->type->vector_elements) {
	 switch (i) {
	 case 0:
	    swizzle[i] = GET_SWZ(src_reg.swizzle, ir->mask.x);
	    break;
	 case 1:
	    swizzle[i] = GET_SWZ(src_reg.swizzle, ir->mask.y);
	    break;
	 case 2:
	    swizzle[i] = GET_SWZ(src_reg.swizzle, ir->mask.z);
	    break;
	 case 3:
	    swizzle[i] = GET_SWZ(src_reg.swizzle, ir->mask.w);
	    break;
	 }
      } else {
	 /* If the type is smaller than a vec4, replicate the last
	  * channel out.
	  */
	 swizzle[i] = swizzle[ir->type->vector_elements - 1];
      }
   }

   src_reg.swizzle = MAKE_SWIZZLE4(swizzle[0],
				   swizzle[1],
				   swizzle[2],
				   swizzle[3]);

   this->result = src_reg;
}

void
ir_to_mesa_visitor::visit(ir_dereference_variable *ir)
{
   variable_storage *entry = find_variable_storage(ir->var);

   if (!entry) {
      switch (ir->var->mode) {
      case ir_var_uniform:
	 entry = new(mem_ctx) variable_storage(ir->var, PROGRAM_UNIFORM,
					       ir->var->location);
	 this->variables.push_tail(entry);
	 break;
      case ir_var_in:
      case ir_var_out:
      case ir_var_inout:
	 /* The linker assigns locations for varyings and attributes,
	  * including deprecated builtins (like gl_Color), user-assign
	  * generic attributes (glBindVertexLocation), and
	  * user-defined varyings.
	  *
	  * FINISHME: We would hit this path for function arguments.  Fix!
	  */

     //LunarG commented out
	 //assert(ir->var->location != -1);
	 if (ir->var->mode == ir_var_in ||
	     ir->var->mode == ir_var_inout) {
	    entry = new(mem_ctx) variable_storage(ir->var,
						  PROGRAM_INPUT,
						  ir->var->location);

	    if (this->prog->Target == GL_VERTEX_PROGRAM_ARB &&
		ir->var->location >= VERT_ATTRIB_GENERIC0) {
	       _mesa_add_attribute(prog->Attributes,
				   ir->var->name,
				   _mesa_sizeof_glsl_type(ir->var->type->gl_type),
				   ir->var->type->gl_type,
				   ir->var->location - VERT_ATTRIB_GENERIC0);
	    }
	 } else {
	    entry = new(mem_ctx) variable_storage(ir->var,
						  PROGRAM_OUTPUT,
						  ir->var->location);
	 }

	 break;
      case ir_var_auto:
      case ir_var_temporary:
	 entry = new(mem_ctx) variable_storage(ir->var, PROGRAM_TEMPORARY,
					       this->next_temp);
	 this->variables.push_tail(entry);

	 next_temp += type_size(ir->var->type);
	 break;
      }

      if (!entry) {
	 printf("Failed to make storage for %s\n", ir->var->name);
	 exit(1);
      }
   }

   this->result = ir_to_mesa_src_reg(entry->file, entry->index, ir->var->type);
}

void
ir_to_mesa_visitor::visit(ir_dereference_array *ir)
{
   ir_constant *index;
   ir_to_mesa_src_reg src_reg;
   int element_size = type_size(ir->type);

   index = ir->array_index->constant_expression_value();

   ir->array->accept(this);
   src_reg = this->result;

   if (index) {
      src_reg.index += index->value.i[0] * element_size;
   } else {
      ir_to_mesa_src_reg array_base = this->result;
      /* Variable index array dereference.  It eats the "vec4" of the
       * base of the array and an index that offsets the Mesa register
       * index.
       */
      ir->array_index->accept(this);

      ir_to_mesa_src_reg index_reg;

      if (element_size == 1) {
	 index_reg = this->result;
      } else {
	 index_reg = get_temp(glsl_type::float_type);

	 ir_to_mesa_emit_op2(ir, OPCODE_MUL,
			     ir_to_mesa_dst_reg_from_src(index_reg),
			     this->result, src_reg_for_float(element_size));
      }

      src_reg.reladdr = ralloc(mem_ctx, ir_to_mesa_src_reg);
      memcpy(src_reg.reladdr, &index_reg, sizeof(index_reg));
   }

   /* If the type is smaller than a vec4, replicate the last channel out. */
   if (ir->type->is_scalar() || ir->type->is_vector())
      src_reg.swizzle = swizzle_for_size(ir->type->vector_elements);
   else
      src_reg.swizzle = SWIZZLE_NOOP;

   this->result = src_reg;
}

void
ir_to_mesa_visitor::visit(ir_dereference_record *ir)
{
   unsigned int i;
   const glsl_type *struct_type = ir->record->type;
   int offset = 0;

   ir->record->accept(this);

   for (i = 0; i < struct_type->length; i++) {
      if (strcmp(struct_type->fields.structure[i].name, ir->field) == 0)
	 break;
      offset += type_size(struct_type->fields.structure[i].type);
   }

   /* If the type is smaller than a vec4, replicate the last channel out. */
   if (ir->type->is_scalar() || ir->type->is_vector())
      this->result.swizzle = swizzle_for_size(ir->type->vector_elements);
   else
      this->result.swizzle = SWIZZLE_NOOP;

   this->result.index += offset;
}

/**
 * We want to be careful in assignment setup to hit the actual storage
 * instead of potentially using a temporary like we might with the
 * ir_dereference handler.
 */
static struct ir_to_mesa_dst_reg
get_assignment_lhs(ir_dereference *ir, ir_to_mesa_visitor *v)
{
   /* The LHS must be a dereference.  If the LHS is a variable indexed array
    * access of a vector, it must be separated into a series conditional moves
    * before reaching this point (see ir_vec_index_to_cond_assign).
    */
   assert(ir->as_dereference());
   ir_dereference_array *deref_array = ir->as_dereference_array();
   if (deref_array) {
      assert(!deref_array->array->type->is_vector());
   }

   /* Use the rvalue deref handler for the most part.  We'll ignore
    * swizzles in it and write swizzles using writemask, though.
    */
   ir->accept(v);
   return ir_to_mesa_dst_reg_from_src(v->result);
}

/**
 * Process the condition of a conditional assignment
 *
 * Examines the condition of a conditional assignment to generate the optimal
 * first operand of a \c CMP instruction.  If the condition is a relational
 * operator with 0 (e.g., \c ir_binop_less), the value being compared will be
 * used as the source for the \c CMP instruction.  Otherwise the comparison
 * is processed to a boolean result, and the boolean result is used as the
 * operand to the CMP instruction.
 */
bool
ir_to_mesa_visitor::process_move_condition(ir_rvalue *ir)
{
   ir_rvalue *src_ir = ir;
   bool negate = true;
   bool switch_order = false;

   ir_expression *const expr = ir->as_expression();
   if ((expr != NULL) && (expr->get_num_operands() == 2)) {
      bool zero_on_left = false;

      if (expr->operands[0]->is_zero()) {
	 src_ir = expr->operands[1];
	 zero_on_left = true;
      } else if (expr->operands[1]->is_zero()) {
	 src_ir = expr->operands[0];
	 zero_on_left = false;
      }

      /*      a is -  0  +            -  0  +
       * (a <  0)  T  F  F  ( a < 0)  T  F  F
       * (0 <  a)  F  F  T  (-a < 0)  F  F  T
       * (a <= 0)  T  T  F  (-a < 0)  F  F  T  (swap order of other operands)
       * (0 <= a)  F  T  T  ( a < 0)  T  F  F  (swap order of other operands)
       * (a >  0)  F  F  T  (-a < 0)  F  F  T
       * (0 >  a)  T  F  F  ( a < 0)  T  F  F
       * (a >= 0)  F  T  T  ( a < 0)  T  F  F  (swap order of other operands)
       * (0 >= a)  T  T  F  (-a < 0)  F  F  T  (swap order of other operands)
       *
       * Note that exchanging the order of 0 and 'a' in the comparison simply
       * means that the value of 'a' should be negated.
       */
      if (src_ir != ir) {
	 switch (expr->operation) {
	 case ir_binop_less:
	    switch_order = false;
	    negate = zero_on_left;
	    break;

	 case ir_binop_greater:
	    switch_order = false;
	    negate = !zero_on_left;
	    break;

	 case ir_binop_lequal:
	    switch_order = true;
	    negate = !zero_on_left;
	    break;

	 case ir_binop_gequal:
	    switch_order = true;
	    negate = zero_on_left;
	    break;

	 default:
	    /* This isn't the right kind of comparison afterall, so make sure
	     * the whole condition is visited.
	     */
	    src_ir = ir;
	    break;
	 }
      }
   }

   src_ir->accept(this);

   /* We use the OPCODE_CMP (a < 0 ? b : c) for conditional moves, and the
    * condition we produced is 0.0 or 1.0.  By flipping the sign, we can
    * choose which value OPCODE_CMP produces without an extra instruction
    * computing the condition.
    */
   if (negate)
      this->result.negate = ~this->result.negate;

   return switch_order;
}

void
ir_to_mesa_visitor::visit(ir_assignment *ir)
{
   struct ir_to_mesa_dst_reg l;
   struct ir_to_mesa_src_reg r;
   int i;

   ir->rhs->accept(this);
   r = this->result;

   l = get_assignment_lhs(ir->lhs, this);

   /* FINISHME: This should really set to the correct maximal writemask for each
    * FINISHME: component written (in the loops below).  This case can only
    * FINISHME: occur for matrices, arrays, and structures.
    */
   if (ir->write_mask == 0) {
      assert(!ir->lhs->type->is_scalar() && !ir->lhs->type->is_vector());
      l.writemask = WRITEMASK_XYZW;
   } else if (ir->lhs->type->is_scalar()) {
      /* FINISHME: This hack makes writing to gl_FragDepth, which lives in the
       * FINISHME: W component of fragment shader output zero, work correctly.
       */
      l.writemask = WRITEMASK_XYZW;
   } else {
      int swizzles[4];
      int first_enabled_chan = 0;
      int rhs_chan = 0;

      assert(ir->lhs->type->is_vector());
      l.writemask = ir->write_mask;

      for (int i = 0; i < 4; i++) {
	 if (l.writemask & (1 << i)) {
	    first_enabled_chan = GET_SWZ(r.swizzle, i);
	    break;
	 }
      }

      /* Swizzle a small RHS vector into the channels being written.
       *
       * glsl ir treats write_mask as dictating how many channels are
       * present on the RHS while Mesa IR treats write_mask as just
       * showing which channels of the vec4 RHS get written.
       */
      for (int i = 0; i < 4; i++) {
	 if (l.writemask & (1 << i))
	    swizzles[i] = GET_SWZ(r.swizzle, rhs_chan++);
	 else
	    swizzles[i] = first_enabled_chan;
      }
      r.swizzle = MAKE_SWIZZLE4(swizzles[0], swizzles[1],
				swizzles[2], swizzles[3]);
   }

   assert(l.file != PROGRAM_UNDEFINED);
   assert(r.file != PROGRAM_UNDEFINED);

   if (ir->condition) {
      const bool switch_order = this->process_move_condition(ir->condition);
      ir_to_mesa_src_reg condition = this->result;

      for (i = 0; i < type_size(ir->lhs->type); i++) {
	 if (switch_order) {
	    ir_to_mesa_emit_op3(ir, OPCODE_CMP, l,
				condition, ir_to_mesa_src_reg_from_dst(l), r);
	 } else {
	    ir_to_mesa_emit_op3(ir, OPCODE_CMP, l,
				condition, r, ir_to_mesa_src_reg_from_dst(l));
	 }

	 l.index++;
	 r.index++;
      }
   } else {
      for (i = 0; i < type_size(ir->lhs->type); i++) {
	 ir_to_mesa_emit_op1(ir, OPCODE_MOV, l, r);
	 l.index++;
	 r.index++;
      }
   }
}


void
ir_to_mesa_visitor::visit(ir_constant *ir)
{
   ir_to_mesa_src_reg src_reg;
   GLfloat stack_vals[4] = { 0 };
   GLfloat *values = stack_vals;
   unsigned int i;

   /* Unfortunately, 4 floats is all we can get into
    * _mesa_add_unnamed_constant.  So, make a temp to store an
    * aggregate constant and move each constant value into it.  If we
    * get lucky, copy propagation will eliminate the extra moves.
    */

   if (ir->type->base_type == GLSL_TYPE_STRUCT) {
      ir_to_mesa_src_reg temp_base = get_temp(ir->type);
      ir_to_mesa_dst_reg temp = ir_to_mesa_dst_reg_from_src(temp_base);

      foreach_iter(exec_list_iterator, iter, ir->components) {
	 ir_constant *field_value = (ir_constant *)iter.get();
	 int size = type_size(field_value->type);

	 assert(size > 0);

	 field_value->accept(this);
	 src_reg = this->result;

	 for (i = 0; i < (unsigned int)size; i++) {
	    ir_to_mesa_emit_op1(ir, OPCODE_MOV, temp, src_reg);

	    src_reg.index++;
	    temp.index++;
	 }
      }
      this->result = temp_base;
      return;
   }

   if (ir->type->is_array()) {
      ir_to_mesa_src_reg temp_base = get_temp(ir->type);
      ir_to_mesa_dst_reg temp = ir_to_mesa_dst_reg_from_src(temp_base);
      int size = type_size(ir->type->fields.array);

      assert(size > 0);

      for (i = 0; i < ir->type->length; i++) {
	 ir->array_elements[i]->accept(this);
	 src_reg = this->result;
	 for (int j = 0; j < size; j++) {
	    ir_to_mesa_emit_op1(ir, OPCODE_MOV, temp, src_reg);

	    src_reg.index++;
	    temp.index++;
	 }
      }
      this->result = temp_base;
      return;
   }

   if (ir->type->is_matrix()) {
      ir_to_mesa_src_reg mat = get_temp(ir->type);
      ir_to_mesa_dst_reg mat_column = ir_to_mesa_dst_reg_from_src(mat);

      for (i = 0; i < ir->type->matrix_columns; i++) {
	 assert(ir->type->base_type == GLSL_TYPE_FLOAT);
	 values = &ir->value.f[i * ir->type->vector_elements];

	 src_reg = ir_to_mesa_src_reg(PROGRAM_CONSTANT, -1, NULL);
	 src_reg.index = _mesa_add_unnamed_constant(this->prog->Parameters,
						values,
						ir->type->vector_elements,
						&src_reg.swizzle);
	 ir_to_mesa_emit_op1(ir, OPCODE_MOV, mat_column, src_reg);

	 mat_column.index++;
      }

      this->result = mat;
      return;
   }

   src_reg.file = PROGRAM_CONSTANT;
   switch (ir->type->base_type) {
   case GLSL_TYPE_FLOAT:
      values = &ir->value.f[0];
      break;
   case GLSL_TYPE_UINT:
      for (i = 0; i < ir->type->vector_elements; i++) {
	 values[i] = ir->value.u[i];
      }
      break;
   case GLSL_TYPE_INT:
      for (i = 0; i < ir->type->vector_elements; i++) {
	 values[i] = ir->value.i[i];
      }
      break;
   case GLSL_TYPE_BOOL:
      for (i = 0; i < ir->type->vector_elements; i++) {
	 values[i] = ir->value.b[i];
      }
      break;
   default:
      assert(!"Non-float/uint/int/bool constant");
   }

   this->result = ir_to_mesa_src_reg(PROGRAM_CONSTANT, -1, ir->type);
   this->result.index = _mesa_add_unnamed_constant(this->prog->Parameters,
						   values,
						   ir->type->vector_elements,
						   &this->result.swizzle);
}

function_entry *
ir_to_mesa_visitor::get_function_signature(ir_function_signature *sig)
{
   function_entry *entry;

   foreach_iter(exec_list_iterator, iter, this->function_signatures) {
      entry = (function_entry *)iter.get();

      if (entry->sig == sig)
	 return entry;
   }

   entry = ralloc(mem_ctx, function_entry);
   entry->sig = sig;
   entry->sig_id = this->next_signature_id++;
   entry->bgn_inst = NULL;

   /* Allocate storage for all the parameters. */
   foreach_iter(exec_list_iterator, iter, sig->parameters) {
      ir_variable *param = (ir_variable *)iter.get();
      variable_storage *storage;

      storage = find_variable_storage(param);
      assert(!storage);

      storage = new(mem_ctx) variable_storage(param, PROGRAM_TEMPORARY,
					      this->next_temp);
      this->variables.push_tail(storage);

      this->next_temp += type_size(param->type);
   }

   if (!sig->return_type->is_void()) {
      entry->return_reg = get_temp(sig->return_type);
   } else {
      entry->return_reg = ir_to_mesa_undef;
   }

   this->function_signatures.push_tail(entry);
   return entry;
}

void
ir_to_mesa_visitor::visit(ir_call *ir)
{
   ir_to_mesa_instruction *call_inst;
   ir_function_signature *sig = ir->get_callee();
   function_entry *entry = get_function_signature(sig);
   int i;

   /* Process in parameters. */
   exec_list_iterator sig_iter = sig->parameters.iterator();
   foreach_iter(exec_list_iterator, iter, *ir) {
      ir_rvalue *param_rval = (ir_rvalue *)iter.get();
      ir_variable *param = (ir_variable *)sig_iter.get();

      if (param->mode == ir_var_in ||
	  param->mode == ir_var_inout) {
	 variable_storage *storage = find_variable_storage(param);
	 assert(storage);

	 param_rval->accept(this);
	 ir_to_mesa_src_reg r = this->result;

	 ir_to_mesa_dst_reg l;
	 l.file = storage->file;
	 l.index = storage->index;
	 l.reladdr = NULL;
	 l.writemask = WRITEMASK_XYZW;
	 l.cond_mask = COND_TR;

	 for (i = 0; i < type_size(param->type); i++) {
	    ir_to_mesa_emit_op1(ir, OPCODE_MOV, l, r);
	    l.index++;
	    r.index++;
	 }
      }

      sig_iter.next();
   }
   assert(!sig_iter.has_next());

   /* Emit call instruction */
   call_inst = ir_to_mesa_emit_op1(ir, OPCODE_CAL,
				   ir_to_mesa_undef_dst, ir_to_mesa_undef);
   call_inst->function = entry;

   /* Process out parameters. */
   sig_iter = sig->parameters.iterator();
   foreach_iter(exec_list_iterator, iter, *ir) {
      ir_rvalue *param_rval = (ir_rvalue *)iter.get();
      ir_variable *param = (ir_variable *)sig_iter.get();

      if (param->mode == ir_var_out ||
	  param->mode == ir_var_inout) {
	 variable_storage *storage = find_variable_storage(param);
	 assert(storage);

	 ir_to_mesa_src_reg r;
	 r.file = storage->file;
	 r.index = storage->index;
	 r.reladdr = NULL;
	 r.swizzle = SWIZZLE_NOOP;
	 r.negate = 0;

	 param_rval->accept(this);
	 ir_to_mesa_dst_reg l = ir_to_mesa_dst_reg_from_src(this->result);

	 for (i = 0; i < type_size(param->type); i++) {
	    ir_to_mesa_emit_op1(ir, OPCODE_MOV, l, r);
	    l.index++;
	    r.index++;
	 }
      }

      sig_iter.next();
   }
   assert(!sig_iter.has_next());

   /* Process return value. */
   this->result = entry->return_reg;
}

void
ir_to_mesa_visitor::visit(ir_texture *ir)
{
   ir_to_mesa_src_reg result_src, coord, lod_info, projector;
   ir_to_mesa_dst_reg result_dst, coord_dst;
   ir_to_mesa_instruction *inst = NULL;
   prog_opcode opcode = OPCODE_NOP;

   ir->coordinate->accept(this);

   /* Put our coords in a temp.  We'll need to modify them for shadow,
    * projection, or LOD, so the only case we'd use it as is is if
    * we're doing plain old texturing.  Mesa IR optimization should
    * handle cleaning up our mess in that case.
    */
   coord = get_temp(glsl_type::vec4_type);
   coord_dst = ir_to_mesa_dst_reg_from_src(coord);
   ir_to_mesa_emit_op1(ir, OPCODE_MOV, coord_dst,
		       this->result);

   if (ir->projector) {
      ir->projector->accept(this);
      projector = this->result;
   }

   /* Storage for our result.  Ideally for an assignment we'd be using
    * the actual storage for the result here, instead.
    */
   result_src = get_temp(glsl_type::vec4_type);
   result_dst = ir_to_mesa_dst_reg_from_src(result_src);

   switch (ir->op) {
   case ir_tex:
      opcode = OPCODE_TEX;
      break;
   case ir_txb:
      opcode = OPCODE_TXB;
      ir->lod_info.bias->accept(this);
      lod_info = this->result;
      break;
   case ir_txl:
      opcode = OPCODE_TXL;
      ir->lod_info.lod->accept(this);
      lod_info = this->result;
      break;
   case ir_txd:
   case ir_txf:
      assert(!"GLSL 1.30 features unsupported");
      break;
   }

   if (ir->projector) {
      if (opcode == OPCODE_TEX) {
	 /* Slot the projector in as the last component of the coord. */
	 coord_dst.writemask = WRITEMASK_W;
	 ir_to_mesa_emit_op1(ir, OPCODE_MOV, coord_dst, projector);
	 coord_dst.writemask = WRITEMASK_XYZW;
	 opcode = OPCODE_TXP;
      } else {
	 ir_to_mesa_src_reg coord_w = coord;
	 coord_w.swizzle = SWIZZLE_WWWW;

	 /* For the other TEX opcodes there's no projective version
	  * since the last slot is taken up by lod info.  Do the
	  * projective divide now.
	  */
	 coord_dst.writemask = WRITEMASK_W;
	 ir_to_mesa_emit_op1(ir, OPCODE_RCP, coord_dst, projector);

	 coord_dst.writemask = WRITEMASK_XYZ;
	 ir_to_mesa_emit_op2(ir, OPCODE_MUL, coord_dst, coord, coord_w);

	 coord_dst.writemask = WRITEMASK_XYZW;
	 coord.swizzle = SWIZZLE_XYZW;
      }
   }

   if (ir->shadow_comparitor) {
      /* Slot the shadow value in as the second to last component of the
       * coord.
       */
      ir->shadow_comparitor->accept(this);
      coord_dst.writemask = WRITEMASK_Z;
      ir_to_mesa_emit_op1(ir, OPCODE_MOV, coord_dst, this->result);
      coord_dst.writemask = WRITEMASK_XYZW;
   }

   if (opcode == OPCODE_TXL || opcode == OPCODE_TXB) {
      /* Mesa IR stores lod or lod bias in the last channel of the coords. */
      coord_dst.writemask = WRITEMASK_W;
      ir_to_mesa_emit_op1(ir, OPCODE_MOV, coord_dst, lod_info);
      coord_dst.writemask = WRITEMASK_XYZW;
   }

   inst = ir_to_mesa_emit_op1(ir, opcode, result_dst, coord);

   if (ir->shadow_comparitor)
      inst->tex_shadow = GL_TRUE;

   inst->sampler = _mesa_get_sampler_uniform_value(ir->sampler,
						   this->shader_program,
						   this->prog);

   const glsl_type *sampler_type = ir->sampler->type;

   switch (sampler_type->sampler_dimensionality) {
   case GLSL_SAMPLER_DIM_1D:
      inst->tex_target = (sampler_type->sampler_array)
	 ? TEXTURE_1D_ARRAY_INDEX : TEXTURE_1D_INDEX;
      break;
   case GLSL_SAMPLER_DIM_2D:
      inst->tex_target = (sampler_type->sampler_array)
	 ? TEXTURE_2D_ARRAY_INDEX : TEXTURE_2D_INDEX;
      break;
   case GLSL_SAMPLER_DIM_3D:
      inst->tex_target = TEXTURE_3D_INDEX;
      break;
   case GLSL_SAMPLER_DIM_CUBE:
      inst->tex_target = TEXTURE_CUBE_INDEX;
      break;
   case GLSL_SAMPLER_DIM_RECT:
      inst->tex_target = TEXTURE_RECT_INDEX;
      break;
   case GLSL_SAMPLER_DIM_BUF:
      assert(!"FINISHME: Implement ARB_texture_buffer_object");
      break;
   default:
      assert(!"Should not get here.");
   }

   this->result = result_src;
}

void
ir_to_mesa_visitor::visit(ir_return *ir)
{
   if (ir->get_value()) {
      ir_to_mesa_dst_reg l;
      int i;

      assert(current_function);

      ir->get_value()->accept(this);
      ir_to_mesa_src_reg r = this->result;

      l = ir_to_mesa_dst_reg_from_src(current_function->return_reg);

      for (i = 0; i < type_size(current_function->sig->return_type); i++) {
	 ir_to_mesa_emit_op1(ir, OPCODE_MOV, l, r);
	 l.index++;
	 r.index++;
      }
   }

   ir_to_mesa_emit_op0(ir, OPCODE_RET);
}

void
ir_to_mesa_visitor::visit(ir_discard *ir)
{
   struct gl_fragment_program *fp = (struct gl_fragment_program *)this->prog;

   if (ir->condition) {
      ir->condition->accept(this);
      this->result.negate = ~this->result.negate;
      ir_to_mesa_emit_op1(ir, OPCODE_KIL, ir_to_mesa_undef_dst, this->result);
   } else {
      ir_to_mesa_emit_op0(ir, OPCODE_KIL_NV);
   }

   fp->UsesKill = GL_TRUE;
}

void
ir_to_mesa_visitor::visit(ir_if *ir)
{
   ir_to_mesa_instruction *cond_inst, *if_inst, *else_inst = NULL;
   ir_to_mesa_instruction *prev_inst;

   prev_inst = (ir_to_mesa_instruction *)this->instructions.get_tail();

   ir->condition->accept(this);
   assert(this->result.file != PROGRAM_UNDEFINED);

   if (this->options->EmitCondCodes) {
      cond_inst = (ir_to_mesa_instruction *)this->instructions.get_tail();

      /* See if we actually generated any instruction for generating
       * the condition.  If not, then cook up a move to a temp so we
       * have something to set cond_update on.
       */
      if (cond_inst == prev_inst) {
	 ir_to_mesa_src_reg temp = get_temp(glsl_type::bool_type);
	 cond_inst = ir_to_mesa_emit_op1(ir->condition, OPCODE_MOV,
					 ir_to_mesa_dst_reg_from_src(temp),
					 result);
      }
      cond_inst->cond_update = GL_TRUE;

      if_inst = ir_to_mesa_emit_op0(ir->condition, OPCODE_IF);
      if_inst->dst_reg.cond_mask = COND_NE;
   } else {
      if_inst = ir_to_mesa_emit_op1(ir->condition,
				    OPCODE_IF, ir_to_mesa_undef_dst,
				    this->result);
   }

   this->instructions.push_tail(if_inst);

   visit_exec_list(&ir->then_instructions, this);

   if (!ir->else_instructions.is_empty()) {
      else_inst = ir_to_mesa_emit_op0(ir->condition, OPCODE_ELSE);
      visit_exec_list(&ir->else_instructions, this);
   }

   if_inst = ir_to_mesa_emit_op1(ir->condition, OPCODE_ENDIF,
				 ir_to_mesa_undef_dst, ir_to_mesa_undef);
}

ir_to_mesa_visitor::ir_to_mesa_visitor()
{
   result.file = PROGRAM_UNDEFINED;
   next_temp = 1;
   next_signature_id = 1;
   current_function = NULL;
   mem_ctx = ralloc_context(NULL);
}

ir_to_mesa_visitor::~ir_to_mesa_visitor()
{
   ralloc_free(mem_ctx);
}

static struct prog_src_register
mesa_src_reg_from_ir_src_reg(ir_to_mesa_src_reg reg)
{
   struct prog_src_register mesa_reg;

   mesa_reg.File = reg.file;
   assert(reg.index < (1 << INST_INDEX_BITS));
   mesa_reg.Index = reg.index;
   mesa_reg.Swizzle = reg.swizzle;
   mesa_reg.RelAddr = reg.reladdr != NULL;
   mesa_reg.Negate = reg.negate;
   mesa_reg.Abs = 0;
   mesa_reg.HasIndex2 = GL_FALSE;
   mesa_reg.RelAddr2 = 0;
   mesa_reg.Index2 = 0;

   return mesa_reg;
}

static void
set_branchtargets(ir_to_mesa_visitor *v,
		  struct prog_instruction *mesa_instructions,
		  int num_instructions)
{
   int if_count = 0, loop_count = 0;
   int *if_stack, *loop_stack;
   int if_stack_pos = 0, loop_stack_pos = 0;
   int i, j;

   for (i = 0; i < num_instructions; i++) {
      switch (mesa_instructions[i].Opcode) {
      case OPCODE_IF:
	 if_count++;
	 break;
      case OPCODE_BGNLOOP:
	 loop_count++;
	 break;
      case OPCODE_BRK:
      case OPCODE_CONT:
	 mesa_instructions[i].BranchTarget = -1;
	 break;
      default:
	 break;
      }
   }

   if_stack = rzalloc_array(v->mem_ctx, int, if_count);
   loop_stack = rzalloc_array(v->mem_ctx, int, loop_count);

   for (i = 0; i < num_instructions; i++) {
      switch (mesa_instructions[i].Opcode) {
      case OPCODE_IF:
	 if_stack[if_stack_pos] = i;
	 if_stack_pos++;
	 break;
      case OPCODE_ELSE:
	 mesa_instructions[if_stack[if_stack_pos - 1]].BranchTarget = i;
	 if_stack[if_stack_pos - 1] = i;
	 break;
      case OPCODE_ENDIF:
	 mesa_instructions[if_stack[if_stack_pos - 1]].BranchTarget = i;
	 if_stack_pos--;
	 break;
      case OPCODE_BGNLOOP:
	 loop_stack[loop_stack_pos] = i;
	 loop_stack_pos++;
	 break;
      case OPCODE_ENDLOOP:
	 loop_stack_pos--;
	 /* Rewrite any breaks/conts at this nesting level (haven't
	  * already had a BranchTarget assigned) to point to the end
	  * of the loop.
	  */
	 for (j = loop_stack[loop_stack_pos]; j < i; j++) {
	    if (mesa_instructions[j].Opcode == OPCODE_BRK ||
		mesa_instructions[j].Opcode == OPCODE_CONT) {
	       if (mesa_instructions[j].BranchTarget == -1) {
		  mesa_instructions[j].BranchTarget = i;
	       }
	    }
	 }
	 /* The loop ends point at each other. */
	 mesa_instructions[i].BranchTarget = loop_stack[loop_stack_pos];
	 mesa_instructions[loop_stack[loop_stack_pos]].BranchTarget = i;
	 break;
      case OPCODE_CAL:
	 foreach_iter(exec_list_iterator, iter, v->function_signatures) {
	    function_entry *entry = (function_entry *)iter.get();

	    if (entry->sig_id == mesa_instructions[i].BranchTarget) {
	       mesa_instructions[i].BranchTarget = entry->inst;
	       break;
	    }
	 }
	 break;
      default:
	 break;
      }
   }
}

static void
              print_program(struct prog_instruction *mesa_instructions,
              ir_instruction **mesa_instruction_annotation,
              int num_instructions)
          {
              ir_instruction *last_ir = NULL;
              int i;
              int indent = 0;

              for (i = 0; i < num_instructions; i++) {
                  struct prog_instruction *mesa_inst = mesa_instructions + i;
                  ir_instruction *ir = mesa_instruction_annotation[i];

                  fprintf(stdout, "%3d: ", i);

                  //if (last_ir != ir && ir) {
                  //    int j;

                  //    for (j = 0; j < indent; j++) {
                  //        fprintf(stdout, " ");
                  //    }
                  //    ir->print();
                  //    printf("\n");
                  //    last_ir = ir;

                  //    fprintf(stdout, "     "); /* line number spacing. */
                  //}

                  indent = _mesa_fprint_instruction_opt(stdout, mesa_inst, indent,
                      PROG_PRINT_DEBUG, NULL);
              }
          }

static void
count_resources(struct gl_program *prog)
{
   unsigned int i;

   prog->SamplersUsed = 0;

   for (i = 0; i < prog->NumInstructions; i++) {
      struct prog_instruction *inst = &prog->Instructions[i];

      if (_mesa_is_tex_instruction(inst->Opcode)) {
	 prog->SamplerTargets[inst->TexSrcUnit] =
	    (gl_texture_index)inst->TexSrcTarget;
	 prog->SamplersUsed |= 1 << inst->TexSrcUnit;
	 if (inst->TexShadow) {
	    prog->ShadowSamplers |= 1 << inst->TexSrcUnit;
	 }
      }
   }

   _mesa_update_shader_textures_used(prog);
}

struct uniform_sort {
   struct gl_uniform *u;
   int pos;
};

/* The shader_program->Uniforms list is almost sorted in increasing
 * uniform->{Frag,Vert}Pos locations, but not quite when there are
 * uniforms shared between targets.  We need to add parameters in
 * increasing order for the targets.
 */
static int
sort_uniforms(const void *a, const void *b)
{
   struct uniform_sort *u1 = (struct uniform_sort *)a;
   struct uniform_sort *u2 = (struct uniform_sort *)b;

   return u1->pos - u2->pos;
}

/* Add the uniforms to the parameters.  The linker chose locations
 * in our parameters lists (which weren't created yet), which the
 * uniforms code will use to poke values into our parameters list
 * when uniforms are updated.
 */
static void
add_uniforms_to_parameters_list(struct gl_shader_program *shader_program,
				struct gl_shader *shader,
				struct gl_program *prog)
{
   unsigned int i;
   unsigned int next_sampler = 0, num_uniforms = 0;
   struct uniform_sort *sorted_uniforms;

   sorted_uniforms = ralloc_array(NULL, struct uniform_sort,
				  shader_program->Uniforms->NumUniforms);

   for (i = 0; i < shader_program->Uniforms->NumUniforms; i++) {
      struct gl_uniform *uniform = shader_program->Uniforms->Uniforms + i;
      int parameter_index = -1;

      switch (shader->Type) {
      case GL_VERTEX_SHADER:
	 parameter_index = uniform->VertPos;
	 break;
      case GL_FRAGMENT_SHADER:
	 parameter_index = uniform->FragPos;
	 break;
      case GL_GEOMETRY_SHADER:
	 parameter_index = uniform->GeomPos;
	 break;
      }

      /* Only add uniforms used in our target. */
      if (parameter_index != -1) {
	 sorted_uniforms[num_uniforms].pos = parameter_index;
	 sorted_uniforms[num_uniforms].u = uniform;
	 num_uniforms++;
      }
   }

   qsort(sorted_uniforms, num_uniforms, sizeof(struct uniform_sort),
	 sort_uniforms);

   for (i = 0; i < num_uniforms; i++) {
      struct gl_uniform *uniform = sorted_uniforms[i].u;
      int parameter_index = sorted_uniforms[i].pos;
      const glsl_type *type = uniform->Type;
      unsigned int size;

      if (type->is_vector() ||
	  type->is_scalar()) {
	 size = type->vector_elements;
      } else {
	 size = type_size(type) * 4;
      }

      gl_register_file file;
      if (type->is_sampler() ||
	  (type->is_array() && type->fields.array->is_sampler())) {
	 file = PROGRAM_SAMPLER;
      } else {
	 file = PROGRAM_UNIFORM;
      }

      GLint index = _mesa_lookup_parameter_index(prog->Parameters, -1,
						 uniform->Name);

      if (index < 0) {
	 index = _mesa_add_parameter(prog->Parameters, file,
				     uniform->Name, size, type->gl_type,
				     NULL, NULL, 0x0);

	 /* Sampler uniform values are stored in prog->SamplerUnits,
	  * and the entry in that array is selected by this index we
	  * store in ParameterValues[].
	  */
	 if (file == PROGRAM_SAMPLER) {
	    for (unsigned int j = 0; j < size / 4; j++)
	       prog->Parameters->ParameterValues[index + j][0] = next_sampler++;
	 }

	 /* The location chosen in the Parameters list here (returned
	  * from _mesa_add_uniform) has to match what the linker chose.
	  */
	 if (index != parameter_index) {
	    fail_link(shader_program, "Allocation of uniform `%s' to target "
		      "failed (%d vs %d)\n",
		      uniform->Name, index, parameter_index);
	 }
      }
   }

   ralloc_free(sorted_uniforms);
}

static void
set_uniform_initializer(struct gl_context *ctx, void *mem_ctx,
			struct gl_shader_program *shader_program,
			const char *name, const glsl_type *type,
			ir_constant *val)
{
   if (type->is_record()) {
      ir_constant *field_constant;

      field_constant = (ir_constant *)val->components.get_head();

      for (unsigned int i = 0; i < type->length; i++) {
	 const glsl_type *field_type = type->fields.structure[i].type;
	 const char *field_name = ralloc_asprintf(mem_ctx, "%s.%s", name,
					    type->fields.structure[i].name);
	 set_uniform_initializer(ctx, mem_ctx, shader_program, field_name,
				 field_type, field_constant);
	 field_constant = (ir_constant *)field_constant->next;
      }
      return;
   }

   int loc = _mesa_get_uniform_location(ctx, shader_program, name);

   if (loc == -1) {
      fail_link(shader_program,
		"Couldn't find uniform for initializer %s\n", name);
      return;
   }

   for (unsigned int i = 0; i < (type->is_array() ? type->length : 1); i++) {
      ir_constant *element;
      const glsl_type *element_type;
      if (type->is_array()) {
	 element = val->array_elements[i];
	 element_type = type->fields.array;
      } else {
	 element = val;
	 element_type = type;
      }

      void *values;

      if (element_type->base_type == GLSL_TYPE_BOOL) {
	 int *conv = ralloc_array(mem_ctx, int, element_type->components());
	 for (unsigned int j = 0; j < element_type->components(); j++) {
	    conv[j] = element->value.b[j];
	 }
	 values = (void *)conv;
	 element_type = glsl_type::get_instance(GLSL_TYPE_INT,
						element_type->vector_elements,
						1);
      } else {
	 values = &element->value;
      }

      if (element_type->is_matrix()) {
	 _mesa_uniform_matrix(ctx, shader_program,
			      element_type->matrix_columns,
			      element_type->vector_elements,
			      loc, 1, GL_FALSE, (GLfloat *)values);
	 loc += element_type->matrix_columns;
      } else {
	 _mesa_uniform(ctx, shader_program, loc, element_type->matrix_columns,
		       values, element_type->gl_type);
	 loc += type_size(element_type);
      }
   }
}

static void
set_uniform_initializers(struct gl_context *ctx,
			 struct gl_shader_program *shader_program)
{
   void *mem_ctx = NULL;

   for (unsigned int i = 0; i < MESA_SHADER_TYPES; i++) {
      struct gl_shader *shader = shader_program->_LinkedShaders[i];

      if (shader == NULL)
	 continue;

      foreach_iter(exec_list_iterator, iter, *shader->ir) {
	 ir_instruction *ir = (ir_instruction *)iter.get();
	 ir_variable *var = ir->as_variable();

	 if (!var || var->mode != ir_var_uniform || !var->constant_value)
	    continue;

	 if (!mem_ctx)
	    mem_ctx = ralloc_context(NULL);

	 set_uniform_initializer(ctx, mem_ctx, shader_program, var->name,
				 var->type, var->constant_value);
      }
   }

   ralloc_free(mem_ctx);
}


/**
 * Convert a shader's GLSL IR into a Mesa gl_program.
 */
static struct gl_program *
get_mesa_program(struct gl_context *ctx,
                 struct gl_shader_program *shader_program,
		 struct gl_shader *shader)
{
   ir_to_mesa_visitor v;
   struct prog_instruction *mesa_instructions, *mesa_inst;
   ir_instruction **mesa_instruction_annotation;
   int i;
   struct gl_program *prog;
   GLenum target;
   const char *target_string;
   GLboolean progress;
   struct gl_shader_compiler_options *options =
         &ctx->ShaderCompilerOptions[_mesa_shader_type_to_index(shader->Type)];

   switch (shader->Type) {
   case GL_VERTEX_SHADER:
      target = GL_VERTEX_PROGRAM_ARB;
      target_string = "vertex";
      break;
   case GL_FRAGMENT_SHADER:
      target = GL_FRAGMENT_PROGRAM_ARB;
      target_string = "fragment";
      break;
   case GL_GEOMETRY_SHADER:
      target = GL_GEOMETRY_PROGRAM_NV;
      target_string = "geometry";
      break;
   default:
      assert(!"should not be reached");
      return NULL;
   }

   validate_ir_tree(shader->ir);

   //LunarG commented out
   //prog = ctx->Driver.NewProgram(ctx, target, shader_program->Name);
   prog = _mesa_new_program(ctx, target, shader_program->Name);
   if (!prog)
      return NULL;
   prog->Parameters = _mesa_new_parameter_list();
   prog->Varying = _mesa_new_parameter_list();
   prog->Attributes = _mesa_new_parameter_list();
   v.ctx = ctx;
   v.prog = prog;
   v.shader_program = shader_program;
   v.options = options;

   add_uniforms_to_parameters_list(shader_program, shader, prog);

   /* Emit Mesa IR for main(). */
   visit_exec_list(shader->ir, &v);
   v.ir_to_mesa_emit_op0(NULL, OPCODE_END);

   /* Now emit bodies for any functions that were used. */
   do {
      progress = GL_FALSE;

      foreach_iter(exec_list_iterator, iter, v.function_signatures) {
	 function_entry *entry = (function_entry *)iter.get();

	 if (!entry->bgn_inst) {
	    v.current_function = entry;

	    entry->bgn_inst = v.ir_to_mesa_emit_op0(NULL, OPCODE_BGNSUB);
	    entry->bgn_inst->function = entry;

	    visit_exec_list(&entry->sig->body, &v);

	    ir_to_mesa_instruction *last;
	    last = (ir_to_mesa_instruction *)v.instructions.get_tail();
	    if (last->op != OPCODE_RET)
	       v.ir_to_mesa_emit_op0(NULL, OPCODE_RET);

	    ir_to_mesa_instruction *end;
	    end = v.ir_to_mesa_emit_op0(NULL, OPCODE_ENDSUB);
	    end->function = entry;

	    progress = GL_TRUE;
	 }
      }
   } while (progress);

   prog->NumTemporaries = v.next_temp;

   int num_instructions = 0;
   foreach_iter(exec_list_iterator, iter, v.instructions) {
      num_instructions++;
   }

   mesa_instructions =
      (struct prog_instruction *)calloc(num_instructions,
					sizeof(*mesa_instructions));
   mesa_instruction_annotation = ralloc_array(v.mem_ctx, ir_instruction *,
					      num_instructions);

   /* Convert ir_mesa_instructions into prog_instructions.
    */
   mesa_inst = mesa_instructions;
   i = 0;
   foreach_iter(exec_list_iterator, iter, v.instructions) {
      const ir_to_mesa_instruction *inst = (ir_to_mesa_instruction *)iter.get();

      mesa_inst->Opcode = inst->op;
      mesa_inst->CondUpdate = inst->cond_update;
      if (inst->saturate)
	 mesa_inst->SaturateMode = SATURATE_ZERO_ONE;
      mesa_inst->DstReg.File = inst->dst_reg.file;
      mesa_inst->DstReg.Index = inst->dst_reg.index;
      mesa_inst->DstReg.CondMask = inst->dst_reg.cond_mask;
      mesa_inst->DstReg.WriteMask = inst->dst_reg.writemask;
      mesa_inst->DstReg.RelAddr = inst->dst_reg.reladdr != NULL;
      mesa_inst->SrcReg[0] = mesa_src_reg_from_ir_src_reg(inst->src_reg[0]);
      mesa_inst->SrcReg[1] = mesa_src_reg_from_ir_src_reg(inst->src_reg[1]);
      mesa_inst->SrcReg[2] = mesa_src_reg_from_ir_src_reg(inst->src_reg[2]);
      mesa_inst->TexSrcUnit = inst->sampler;
      mesa_inst->TexSrcTarget = inst->tex_target;
      mesa_inst->TexShadow = inst->tex_shadow;
      mesa_instruction_annotation[i] = inst->ir;

      /* Set IndirectRegisterFiles. */
      if (mesa_inst->DstReg.RelAddr)
         prog->IndirectRegisterFiles |= 1 << mesa_inst->DstReg.File;

      /* Update program's bitmask of indirectly accessed register files */
      for (unsigned src = 0; src < 3; src++)
         if (mesa_inst->SrcReg[src].RelAddr)
            prog->IndirectRegisterFiles |= 1 << mesa_inst->SrcReg[src].File;

      if (options->EmitNoIfs && mesa_inst->Opcode == OPCODE_IF) {
	 fail_link(shader_program, "Couldn't flatten if statement\n");
      }

      switch (mesa_inst->Opcode) {
      case OPCODE_BGNSUB:
	 inst->function->inst = i;
	 mesa_inst->Comment = strdup(inst->function->sig->function_name());
	 break;
      case OPCODE_ENDSUB:
	 mesa_inst->Comment = strdup(inst->function->sig->function_name());
	 break;
      case OPCODE_CAL:
	 mesa_inst->BranchTarget = inst->function->sig_id; /* rewritten later */
	 break;
      case OPCODE_ARL:
	 prog->NumAddressRegs = 1;
	 break;
      default:
	 break;
      }

      mesa_inst++;
      i++;

      if (!shader_program->LinkStatus)
         break;
   }

   if (!shader_program->LinkStatus) {
      free(mesa_instructions);
      _mesa_reference_program(ctx, &shader->Program, NULL);
      return NULL;
   }

   set_branchtargets(&v, mesa_instructions, num_instructions);

   if (ctx->Shader.Flags & GLSL_DUMP &&
       (ctx->Shader.Flags & GLSL_NO_OPT) != 0) {
       //printf("\n");
       //printf("GLSL IR for linked %s program %d:\n", target_string,
       //shader_program->Name);
       //_mesa_print_ir(shader->ir, NULL);
       //printf("\n");
       printf("\n");
       printf("===========================================\n");
       printf("\n");
       printf("Unoptimzied Mesa IR without LunarGLASS for linked %s program %d:\n", target_string,
           shader_program->Name);
       print_program(mesa_instructions, mesa_instruction_annotation,
           num_instructions);
   }

   prog->Instructions = mesa_instructions;
   prog->NumInstructions = num_instructions;

   do_set_program_inouts(shader->ir, prog);
   count_resources(prog);

   _mesa_reference_program(ctx, &shader->Program, prog);

   if ((ctx->Shader.Flags & GLSL_NO_OPT) == 0) {
       _mesa_optimize_program(ctx, prog);
       printf("\n");
       printf("===========================================\n");
       printf("\n");
       printf("Optimized Mesa IR without GLA for linked %s program %d:\n", target_string,
           shader_program->Name);
       _mesa_print_program(prog);
   }

   return prog;
}

extern "C" {

/**
 * Called via ctx->Driver.CompilerShader().
 * This is a no-op.
 * XXX can we remove the ctx->Driver.CompileShader() hook?
 */
GLboolean
_mesa_ir_compile_shader(struct gl_context *ctx, struct gl_shader *shader)
{
   assert(shader->CompileStatus);
   (void) ctx;

   return GL_TRUE;
}


/**
 * Link a shader.
 * Called via ctx->Driver.LinkShader()
 * This actually involves converting GLSL IR into Mesa gl_programs with
 * code lowering and other optimizations.
 */
GLboolean
_mesa_ir_link_shader(struct gl_context *ctx, struct gl_shader_program *prog)
{
   assert(prog->LinkStatus);

   for (unsigned i = 0; i < MESA_SHADER_TYPES; i++) {
      if (prog->_LinkedShaders[i] == NULL)
	 continue;

      bool progress;
      exec_list *ir = prog->_LinkedShaders[i]->ir;
      const struct gl_shader_compiler_options *options =
            &ctx->ShaderCompilerOptions[_mesa_shader_type_to_index(prog->_LinkedShaders[i]->Type)];

      do {
	 progress = false;

	 /* Lowering */
	 do_mat_op_to_vec(ir);
	 lower_instructions(ir, (MOD_TO_FRACT | DIV_TO_MUL_RCP | EXP_TO_EXP2
				 | LOG_TO_LOG2
				 | ((options->EmitNoPow) ? POW_TO_EXP2 : 0)));

	 progress = do_lower_jumps(ir, true, true, options->EmitNoMainReturn, options->EmitNoCont, options->EmitNoLoops) || progress;

	 progress = do_common_optimization(ir, true, options->MaxUnrollIterations) || progress;

	 progress = lower_quadop_vector(ir, true) || progress;

	 if (options->EmitNoIfs) {
	    progress = lower_discard(ir) || progress;
	    progress = lower_if_to_cond_assign(ir) || progress;
	 }

	 if (options->EmitNoNoise)
	    progress = lower_noise(ir) || progress;

	 /* If there are forms of indirect addressing that the driver
	  * cannot handle, perform the lowering pass.
	  */
	 if (options->EmitNoIndirectInput || options->EmitNoIndirectOutput
	     || options->EmitNoIndirectTemp || options->EmitNoIndirectUniform)
	   progress =
	     lower_variable_index_to_cond_assign(ir,
						 options->EmitNoIndirectInput,
						 options->EmitNoIndirectOutput,
						 options->EmitNoIndirectTemp,
						 options->EmitNoIndirectUniform)
	     || progress;

	 progress = do_vec_index_to_cond_assign(ir) || progress;
      } while (progress);

      validate_ir_tree(ir);
   }

   for (unsigned i = 0; i < MESA_SHADER_TYPES; i++) {
      struct gl_program *linked_prog;

      if (prog->_LinkedShaders[i] == NULL)
	 continue;

      linked_prog = get_mesa_program(ctx, prog, prog->_LinkedShaders[i]);

      if (linked_prog) {
         bool ok = true;

         switch (prog->_LinkedShaders[i]->Type) {
         case GL_VERTEX_SHADER:
            _mesa_reference_vertprog(ctx, &prog->VertexProgram,
                                     (struct gl_vertex_program *)linked_prog);
            ok = ctx->Driver.ProgramStringNotify(ctx, GL_VERTEX_PROGRAM_ARB,
                                                 linked_prog);
            break;
         case GL_FRAGMENT_SHADER:
            _mesa_reference_fragprog(ctx, &prog->FragmentProgram,
                                     (struct gl_fragment_program *)linked_prog);
            //LunarG commented out
            //ok = ctx->Driver.ProgramStringNotify(ctx, GL_FRAGMENT_PROGRAM_ARB,
            //                                     linked_prog);
            break;
         case GL_GEOMETRY_SHADER:
            _mesa_reference_geomprog(ctx, &prog->GeometryProgram,
                                     (struct gl_geometry_program *)linked_prog);
            ok = ctx->Driver.ProgramStringNotify(ctx, GL_GEOMETRY_PROGRAM_NV,
                                                 linked_prog);
            break;
         }
         if (!ok) {
            return GL_FALSE;
         }
      }

      _mesa_reference_program(ctx, &linked_prog, NULL);
   }

   return GL_TRUE;
}


/**
 * Compile a GLSL shader.  Called via glCompileShader().
 */
void
_mesa_glsl_compile_shader(struct gl_context *ctx, struct gl_shader *shader)
{
   struct _mesa_glsl_parse_state *state =
      new(shader) _mesa_glsl_parse_state(ctx, shader->Type, shader);

   const char *source = shader->Source;
   /* Check if the user called glCompileShader without first calling
    * glShaderSource.  This should fail to compile, but not raise a GL_ERROR.
    */
   if (source == NULL) {
      shader->CompileStatus = GL_FALSE;
      return;
   }

   state->error = preprocess(state, &source, &state->info_log,
			     &ctx->Extensions, ctx->API);

   if (ctx->Shader.Flags & GLSL_DUMP) {
      printf("GLSL source for shader %d:\n", shader->Name);
      printf("%s\n", shader->Source);
   }

   if (!state->error) {
     _mesa_glsl_lexer_ctor(state, source);
     _mesa_glsl_parse(state);
     _mesa_glsl_lexer_dtor(state);
   }

   ralloc_free(shader->ir);
   shader->ir = new(shader) exec_list;
   if (!state->error && !state->translation_unit.is_empty())
      _mesa_ast_to_hir(shader->ir, state);

   if (!state->error && !shader->ir->is_empty()) {
      validate_ir_tree(shader->ir);

      /* Do some optimization at compile time to reduce shader IR size
       * and reduce later work if the same shader is linked multiple times
       */
      while (do_common_optimization(shader->ir, false, 32))
	 ;

      validate_ir_tree(shader->ir);
   }

   shader->symbols = state->symbols;

   shader->CompileStatus = !state->error;
   shader->InfoLog = state->info_log;
   shader->Version = state->language_version;
   memcpy(shader->builtins_to_link, state->builtins_to_link,
	  sizeof(shader->builtins_to_link[0]) * state->num_builtins_to_link);
   shader->num_builtins_to_link = state->num_builtins_to_link;

   if (ctx->Shader.Flags & GLSL_LOG) {
      _mesa_write_shader_to_file(shader);
   }

   if (ctx->Shader.Flags & GLSL_DUMP) {
      if (shader->CompileStatus) {
	 printf("GLSL IR for shader %d:\n", shader->Name);
	 _mesa_print_ir(shader->ir, NULL);
	 printf("\n\n");
      } else {
	 printf("GLSL shader %d failed to compile.\n", shader->Name);
      }
      if (shader->InfoLog && shader->InfoLog[0] != 0) {
	 printf("GLSL shader %d info log:\n", shader->Name);
	 printf("%s\n", shader->InfoLog);
      }
   }

   /* Retain any live IR, but trash the rest. */
   reparent_ir(shader->ir, shader->ir);

   ralloc_free(state);

   if (shader->CompileStatus) {
      if (!ctx->Driver.CompileShader(ctx, shader))
	 shader->CompileStatus = GL_FALSE;
   }
}


/**
 * Link a GLSL shader program.  Called via glLinkProgram().
 */
void
_mesa_glsl_link_shader(struct gl_context *ctx, struct gl_shader_program *prog)
{
   unsigned int i;

   _mesa_clear_shader_program_data(ctx, prog);

   prog->LinkStatus = GL_TRUE;

   for (i = 0; i < prog->NumShaders; i++) {
      if (!prog->Shaders[i]->CompileStatus) {
	 fail_link(prog, "linking with uncompiled shader");
	 prog->LinkStatus = GL_FALSE;
      }
   }

   prog->Varying = _mesa_new_parameter_list();
   _mesa_reference_vertprog(ctx, &prog->VertexProgram, NULL);
   _mesa_reference_fragprog(ctx, &prog->FragmentProgram, NULL);
   _mesa_reference_geomprog(ctx, &prog->GeometryProgram, NULL);

   if (prog->LinkStatus) {
      link_shaders(ctx, prog);
   }

   if (prog->LinkStatus) {
      if (!ctx->Driver.LinkShader(ctx, prog)) {
	 prog->LinkStatus = GL_FALSE;
      }
   }

   set_uniform_initializers(ctx, prog);

   if (ctx->Shader.Flags & GLSL_DUMP) {
      if (!prog->LinkStatus) {
	 printf("GLSL shader program %d failed to link\n", prog->Name);
      }

      if (prog->InfoLog && prog->InfoLog[0] != 0) {
	 printf("GLSL shader program %d info log:\n", prog->Name);
	 printf("%s\n", prog->InfoLog);
      }
   }
}

// LunarG commented
// This is where we've pulled in a *bunch* of mesa to code support
// shader conversion
#include "context.h"
/**
 * Add a new unnamed constant to the parameter list.  This will be used
 * when a fragment/vertex program contains something like this:
 *    MOV r, { 0, 1, 2, 3 };
 * If swizzleOut is non-null we'll search the parameter list for an
 * existing instance of the constant which matches with a swizzle.
 *
 * \param paramList  the parameter list
 * \param values  four float values
 * \param swizzleOut  returns swizzle mask for accessing the constant
 * \return index/position of the new parameter in the parameter list.
 */
GLint
_mesa_add_unnamed_constant(struct gl_program_parameter_list *paramList,
                           const GLfloat values[4], GLuint size,
                           GLuint *swizzleOut)
{
   GLint pos;
   ASSERT(size >= 1);
   ASSERT(size <= 4);

   if (swizzleOut &&
       _mesa_lookup_parameter_constant(paramList, values,
                                       size, &pos, swizzleOut)) {
      return pos;
   }

   /* Look for empty space in an already unnamed constant parameter
    * to add this constant.  This will only work for single-element
    * constants because we rely on smearing (i.e. .yyyy or .zzzz).
    */
   if (size == 1 && swizzleOut) {
      for (pos = 0; pos < (GLint) paramList->NumParameters; pos++) {
         struct gl_program_parameter *p = paramList->Parameters + pos;
         if (p->Type == PROGRAM_CONSTANT && p->Size + size <= 4) {
            /* ok, found room */
            GLfloat *pVal = paramList->ParameterValues[pos];
            GLuint swz = p->Size; /* 1, 2 or 3 for Y, Z, W */
            pVal[p->Size] = values[0];
            p->Size++;
            *swizzleOut = MAKE_SWIZZLE4(swz, swz, swz, swz);
            return pos;
         }
      }
   }

   /* add a new parameter to store this constant */
   pos = _mesa_add_parameter(paramList, PROGRAM_CONSTANT, NULL,
                             size, GL_NONE, values, NULL, 0x0);
   if (pos >= 0 && swizzleOut) {
      if (size == 1)
         *swizzleOut = SWIZZLE_XXXX;
      else
         *swizzleOut = SWIZZLE_NOOP;
   }
   return pos;
}

/**
 * Add a new state reference to the parameter list.
 * This will be used when the program contains something like this:
 *    PARAM ambient = state.material.front.ambient;
 *
 * \param paramList  the parameter list
 * \param stateTokens  an array of 5 (STATE_LENGTH) state tokens
 * \return index of the new parameter.
 */
GLint
_mesa_add_state_reference(struct gl_program_parameter_list *paramList,
                          const gl_state_index stateTokens[STATE_LENGTH])
{
   const GLuint size = 4; /* XXX fix */
   char *name;
   GLint index;

   /* Check if the state reference is already in the list */
   for (index = 0; index < (GLint) paramList->NumParameters; index++) {
      GLuint i, match = 0;
      for (i = 0; i < STATE_LENGTH; i++) {
         if (paramList->Parameters[index].StateIndexes[i] == stateTokens[i]) {
            match++;
         }
         else {
            break;
         }
      }
      if (match == STATE_LENGTH) {
         /* this state reference is already in the parameter list */
         return index;
      }
   }

   name = _mesa_program_state_string(stateTokens);
   index = _mesa_add_parameter(paramList, PROGRAM_STATE_VAR, name,
                               size, GL_NONE,
                               NULL, (gl_state_index *) stateTokens, 0x0);
   paramList->StateFlags |= _mesa_program_state_flags(stateTokens);

   /* free name string here since we duplicated it in add_parameter() */
   free(name);

   return index;
}

/**
 * Add parameter representing a vertex program attribute.
 * \param size  size of attribute (in floats), may be -1 if unknown
 * \param attrib  the attribute index, or -1 if unknown
 */
GLint
_mesa_add_attribute(struct gl_program_parameter_list *paramList,
                    const char *name, GLint size, GLenum datatype, GLint attrib)
{
   GLint i = _mesa_lookup_parameter_index(paramList, -1, name);
   if (i >= 0) {
      /* replace */
      if (attrib < 0)
         attrib = i;
      paramList->Parameters[i].StateIndexes[0] = (gl_state_index)attrib;
   }
   else {
      /* add */
      gl_state_index state[STATE_LENGTH];
      state[0] = (gl_state_index) attrib;
      if (size < 0)
         size = 4;
      i = _mesa_add_parameter(paramList, PROGRAM_INPUT, name,
                              size, datatype, NULL, state, 0x0);
   }
   return i;
}

/**
 * Return the size of the given GLSL datatype, in floats (components).
 */
GLint
_mesa_sizeof_glsl_type(GLenum type)
{
   switch (type) {
   case GL_FLOAT:
   case GL_INT:
   case GL_BOOL:
   case GL_SAMPLER_1D:
   case GL_SAMPLER_2D:
   case GL_SAMPLER_3D:
   case GL_SAMPLER_CUBE:
   case GL_SAMPLER_1D_SHADOW:
   case GL_SAMPLER_2D_SHADOW:
   case GL_SAMPLER_2D_RECT_ARB:
   case GL_SAMPLER_2D_RECT_SHADOW_ARB:
   case GL_SAMPLER_1D_ARRAY_EXT:
   case GL_SAMPLER_2D_ARRAY_EXT:
   case GL_SAMPLER_1D_ARRAY_SHADOW_EXT:
   case GL_SAMPLER_2D_ARRAY_SHADOW_EXT:
   case GL_SAMPLER_CUBE_SHADOW_EXT:
      return 1;
   case GL_FLOAT_VEC2:
   case GL_INT_VEC2:
   case GL_UNSIGNED_INT_VEC2:
   case GL_BOOL_VEC2:
      return 2;
   case GL_FLOAT_VEC3:
   case GL_INT_VEC3:
   case GL_UNSIGNED_INT_VEC3:
   case GL_BOOL_VEC3:
      return 3;
   case GL_FLOAT_VEC4:
   case GL_INT_VEC4:
   case GL_UNSIGNED_INT_VEC4:
   case GL_BOOL_VEC4:
      return 4;
   case GL_FLOAT_MAT2:
   case GL_FLOAT_MAT2x3:
   case GL_FLOAT_MAT2x4:
      return 8; /* two float[4] vectors */
   case GL_FLOAT_MAT3:
   case GL_FLOAT_MAT3x2:
   case GL_FLOAT_MAT3x4:
      return 12; /* three float[4] vectors */
   case GL_FLOAT_MAT4:
   case GL_FLOAT_MAT4x2:
   case GL_FLOAT_MAT4x3:
      return 16;  /* four float[4] vectors */
   default:
      _mesa_problem(NULL, "Invalid type in _mesa_sizeof_glsl_type()");
      return 1;
   }
}

/**
 * Given a program parameter name, find its position in the list of parameters.
 * \param paramList  the parameter list to search
 * \param nameLen  length of name (in chars).
 *                 If length is negative, assume that name is null-terminated.
 * \param name  the name to search for
 * \return index of parameter in the list.
 */
GLint
_mesa_lookup_parameter_index(const struct gl_program_parameter_list *paramList,
                             GLsizei nameLen, const char *name)
{
   GLint i;

   if (!paramList)
      return -1;

   if (nameLen == -1) {
      /* name is null-terminated */
      for (i = 0; i < (GLint) paramList->NumParameters; i++) {
         if (paramList->Parameters[i].Name &&
	     strcmp(paramList->Parameters[i].Name, name) == 0)
            return i;
      }
   }
   else {
      /* name is not null-terminated, use nameLen */
      for (i = 0; i < (GLint) paramList->NumParameters; i++) {
         if (paramList->Parameters[i].Name &&
	     strncmp(paramList->Parameters[i].Name, name, nameLen) == 0
             && strlen(paramList->Parameters[i].Name) == (size_t)nameLen)
            return i;
      }
   }
   return -1;
}

struct gl_program_parameter_list *
_mesa_new_parameter_list(void)
{
   return CALLOC_STRUCT(gl_program_parameter_list);
}


struct gl_program_parameter_list *
_mesa_new_parameter_list_sized(unsigned size)
{
   struct gl_program_parameter_list *p = _mesa_new_parameter_list();

   if ((p != NULL) && (size != 0)) {
      p->Size = size;

      /* alloc arrays */
      p->Parameters = (struct gl_program_parameter *)
	 calloc(1, size * sizeof(struct gl_program_parameter));

      p->ParameterValues = (GLfloat (*)[4])
         _mesa_align_malloc(size * 4 *sizeof(GLfloat), 16);


      if ((p->Parameters == NULL) || (p->ParameterValues == NULL)) {
	 free(p->Parameters);
	 _mesa_align_free(p->ParameterValues);
	 free(p);
	 p = NULL;
      }
   }

   return p;
}

/**
 * Update the vertex/fragment program's TexturesUsed array.
 *
 * This needs to be called after glUniform(set sampler var) is called.
 * A call to glUniform(samplerVar, value) causes a sampler to point to a
 * particular texture unit.  We know the sampler's texture target
 * (1D/2D/3D/etc) from compile time but the sampler's texture unit is
 * set by glUniform() calls.
 *
 * So, scan the program->SamplerUnits[] and program->SamplerTargets[]
 * information to update the prog->TexturesUsed[] values.
 * Each value of TexturesUsed[unit] is one of zero, TEXTURE_1D_INDEX,
 * TEXTURE_2D_INDEX, TEXTURE_3D_INDEX, etc.
 * We'll use that info for state validation before rendering.
 */
void
_mesa_update_shader_textures_used(struct gl_program *prog)
{
   GLuint s;

   memset(prog->TexturesUsed, 0, sizeof(prog->TexturesUsed));

   for (s = 0; s < MAX_SAMPLERS; s++) {
      if (prog->SamplersUsed & (1 << s)) {
         GLuint unit = prog->SamplerUnits[s];
         GLuint tgt = prog->SamplerTargets[s];
         assert(unit < MAX_TEXTURE_IMAGE_UNITS);
         assert(tgt < NUM_TEXTURE_TARGETS);
         prog->TexturesUsed[unit] |= (1 << tgt);
      }
   }
}


GLboolean
_mesa_is_tex_instruction(gl_inst_opcode opcode)
{
   return (opcode == OPCODE_TEX ||
           opcode == OPCODE_TXB ||
           opcode == OPCODE_TXD ||
           opcode == OPCODE_TXL ||
           opcode == OPCODE_TXP);
}


/**
 * Add a new parameter to a parameter list.
 * Note that parameter values are usually 4-element GLfloat vectors.
 * When size > 4 we'll allocate a sequential block of parameters to
 * store all the values (in blocks of 4).
 *
 * \param paramList  the list to add the parameter to
 * \param type  type of parameter, such as 
 * \param name  the parameter name, will be duplicated/copied!
 * \param size  number of elements in 'values' vector (1..4, or more)
 * \param datatype  GL_FLOAT, GL_FLOAT_VECx, GL_INT, GL_INT_VECx or GL_NONE.
 * \param values  initial parameter value, up to 4 GLfloats, or NULL
 * \param state  state indexes, or NULL
 * \return  index of new parameter in the list, or -1 if error (out of mem)
 */
GLint
_mesa_add_parameter(struct gl_program_parameter_list *paramList,
                    gl_register_file type, const char *name,
                    GLuint size, GLenum datatype, const GLfloat *values,
                    const gl_state_index state[STATE_LENGTH],
                    GLbitfield flags)
{
   const GLuint oldNum = paramList->NumParameters;
   const GLuint sz4 = (size + 3) / 4; /* no. of new param slots needed */

   assert(size > 0);

   if (oldNum + sz4 > paramList->Size) {
      /* Need to grow the parameter list array (alloc some extra) */
      paramList->Size = paramList->Size + 4 * sz4;

      /* realloc arrays */
      paramList->Parameters = (struct gl_program_parameter *)
	 _mesa_realloc(paramList->Parameters,
		       oldNum * sizeof(struct gl_program_parameter),
		       paramList->Size * sizeof(struct gl_program_parameter));

      paramList->ParameterValues = (GLfloat (*)[4])
         _mesa_align_realloc(paramList->ParameterValues,         /* old buf */
                             oldNum * 4 * sizeof(GLfloat),      /* old size */
                             paramList->Size * 4 *sizeof(GLfloat), /* new sz */
                             16);
   }

   if (!paramList->Parameters ||
       !paramList->ParameterValues) {
      /* out of memory */
      paramList->NumParameters = 0;
      paramList->Size = 0;
      return -1;
   }
   else {
      GLuint i;

      paramList->NumParameters = oldNum + sz4;

      memset(&paramList->Parameters[oldNum], 0,
             sz4 * sizeof(struct gl_program_parameter));

      for (i = 0; i < sz4; i++) {
         struct gl_program_parameter *p = paramList->Parameters + oldNum + i;
         p->Name = name ? _mesa_strdup(name) : NULL;
         p->Type = type;
         p->Size = size;
         p->DataType = datatype;
         p->Flags = flags;
         if (values) {
            COPY_4V(paramList->ParameterValues[oldNum + i], values);
            values += 4;
            p->Initialized = GL_TRUE;
         }
         else {
            /* silence valgrind */
            ASSIGN_4V(paramList->ParameterValues[oldNum + i], 0, 0, 0, 0);
         }
         size -= 4;
      }

      if (state) {
         for (i = 0; i < STATE_LENGTH; i++)
            paramList->Parameters[oldNum].StateIndexes[i] = state[i];
      }

      return (GLint) oldNum;
   }
}

/**
 * Reference counting for vertex/fragment programs
 */
void
_mesa_reference_program(struct gl_context *ctx,
                        struct gl_program **ptr,
                        struct gl_program *prog)
{
   assert(ptr);
   if (*ptr && prog) {
      /* sanity check */
      if ((*ptr)->Target == GL_VERTEX_PROGRAM_ARB)
         ASSERT(prog->Target == GL_VERTEX_PROGRAM_ARB);
      else if ((*ptr)->Target == GL_FRAGMENT_PROGRAM_ARB)
         ASSERT(prog->Target == GL_FRAGMENT_PROGRAM_ARB ||
                prog->Target == GL_FRAGMENT_PROGRAM_NV);
      else if ((*ptr)->Target == MESA_GEOMETRY_PROGRAM)
         ASSERT(prog->Target == MESA_GEOMETRY_PROGRAM);
   }
   if (*ptr == prog) {
      return;  /* no change */
   }
   if (*ptr) {
      GLboolean deleteFlag;

      /*_glthread_LOCK_MUTEX((*ptr)->Mutex);*/
#if 0
      printf("Program %p ID=%u Target=%s  Refcount-- to %d\n",
             *ptr, (*ptr)->Id,
             ((*ptr)->Target == GL_VERTEX_PROGRAM_ARB ? "VP" :
              ((*ptr)->Target == MESA_GEOMETRY_PROGRAM ? "GP" : "FP")),
             (*ptr)->RefCount - 1);
#endif
      ASSERT((*ptr)->RefCount > 0);
      (*ptr)->RefCount--;

      deleteFlag = ((*ptr)->RefCount == 0);
      /*_glthread_UNLOCK_MUTEX((*ptr)->Mutex);*/

      if (deleteFlag) {
         ASSERT(ctx);
         ctx->Driver.DeleteProgram(ctx, *ptr);
      }

      *ptr = NULL;
   }

   assert(!*ptr);
   if (prog) {
      /*_glthread_LOCK_MUTEX(prog->Mutex);*/
      prog->RefCount++;
#if 0
      printf("Program %p ID=%u Target=%s  Refcount++ to %d\n",
             prog, prog->Id,
             (prog->Target == GL_VERTEX_PROGRAM_ARB ? "VP" :
              (prog->Target == MESA_GEOMETRY_PROGRAM ? "GP" : "FP")),
             prog->RefCount);
#endif
      /*_glthread_UNLOCK_MUTEX(prog->Mutex);*/
   }

   *ptr = prog;
}

/**
 * Clear (free) the shader program state that gets produced by linking.
 */
void
_mesa_clear_shader_program_data(struct gl_context *ctx,
                                struct gl_shader_program *shProg)
{
   _mesa_reference_vertprog(ctx, &shProg->VertexProgram, NULL);
   _mesa_reference_fragprog(ctx, &shProg->FragmentProgram, NULL);
   _mesa_reference_geomprog(ctx, &shProg->GeometryProgram, NULL);

   if (shProg->Uniforms) {
      _mesa_free_uniform_list(shProg->Uniforms);
      shProg->Uniforms = NULL;
   }

   if (shProg->Varying) {
      _mesa_free_parameter_list(shProg->Varying);
      shProg->Varying = NULL;
   }
}

                                /**
 * Separate the uniform location and parameter offset.  See above.
 */
static void
split_location_offset(GLint *location, GLint *offset)
{
   *offset = *location & 0xffff;
   *location = *location >> 16;
}

static GLboolean
is_sampler_type(GLenum type)
{
   switch (type) {
   case GL_SAMPLER_1D:
   case GL_SAMPLER_2D:
   case GL_SAMPLER_3D:
   case GL_SAMPLER_CUBE:
   case GL_SAMPLER_1D_SHADOW:
   case GL_SAMPLER_2D_SHADOW:
   case GL_SAMPLER_2D_RECT_ARB:
   case GL_SAMPLER_2D_RECT_SHADOW_ARB:
   case GL_SAMPLER_1D_ARRAY_EXT:
   case GL_SAMPLER_2D_ARRAY_EXT:
   case GL_SAMPLER_1D_ARRAY_SHADOW_EXT:
   case GL_SAMPLER_2D_ARRAY_SHADOW_EXT:
      return GL_TRUE;
   default:
      return GL_FALSE;
   }
}

/**
 * Check if the type given by userType is allowed to set a uniform of the
 * target type.  Generally, equivalence is required, but setting Boolean
 * uniforms can be done with glUniformiv or glUniformfv.
 */
static GLboolean
compatible_types(GLenum userType, GLenum targetType)
{
   if (userType == targetType)
      return GL_TRUE;

   if (targetType == GL_BOOL && (userType == GL_FLOAT ||
                                 userType == GL_UNSIGNED_INT ||
                                 userType == GL_INT))
      return GL_TRUE;

   if (targetType == GL_BOOL_VEC2 && (userType == GL_FLOAT_VEC2 ||
                                      userType == GL_UNSIGNED_INT_VEC2 ||
                                      userType == GL_INT_VEC2))
      return GL_TRUE;

   if (targetType == GL_BOOL_VEC3 && (userType == GL_FLOAT_VEC3 ||
                                      userType == GL_UNSIGNED_INT_VEC3 ||
                                      userType == GL_INT_VEC3))
      return GL_TRUE;

   if (targetType == GL_BOOL_VEC4 && (userType == GL_FLOAT_VEC4 ||
                                      userType == GL_UNSIGNED_INT_VEC4 ||
                                      userType == GL_INT_VEC4))
      return GL_TRUE;

   if (is_sampler_type(targetType) && userType == GL_INT)
      return GL_TRUE;

   return GL_FALSE;
}

static GLboolean
is_boolean_type(GLenum type)
{
   switch (type) {
   case GL_BOOL:
   case GL_BOOL_VEC2:
   case GL_BOOL_VEC3:
   case GL_BOOL_VEC4:
      return GL_TRUE;
   default:
      return GL_FALSE;
   }
}

static GLenum
base_uniform_type(GLenum type)
{
   switch (type) {
#if 0 /* not needed, for now */
   case GL_BOOL:
   case GL_BOOL_VEC2:
   case GL_BOOL_VEC3:
   case GL_BOOL_VEC4:
      return GL_BOOL;
#endif
   case GL_FLOAT:
   case GL_FLOAT_VEC2:
   case GL_FLOAT_VEC3:
   case GL_FLOAT_VEC4:
      return GL_FLOAT;
   case GL_UNSIGNED_INT:
   case GL_UNSIGNED_INT_VEC2:
   case GL_UNSIGNED_INT_VEC3:
   case GL_UNSIGNED_INT_VEC4:
      return GL_UNSIGNED_INT;
   case GL_INT:
   case GL_INT_VEC2:
   case GL_INT_VEC3:
   case GL_INT_VEC4:
      return GL_INT;
   default:
      _mesa_problem(NULL, "Invalid type in base_uniform_type()");
      return GL_FLOAT;
   }
}
/**
 * Set the value of a program's uniform variable.
 * \param program  the program whose uniform to update
 * \param index  the index of the program parameter for the uniform
 * \param offset  additional parameter slot offset (for arrays)
 * \param type  the incoming datatype of 'values'
 * \param count  the number of uniforms to set
 * \param elems  number of elements per uniform (1, 2, 3 or 4)
 * \param values  the new values, of datatype 'type'
 */
static void
set_program_uniform(struct gl_context *ctx, struct gl_program *program,
                    GLint index, GLint offset,
                    GLenum type, GLsizei count, GLint elems,
                    const void *values)
{
   const struct gl_program_parameter *param =
      &program->Parameters->Parameters[index];

   assert(offset >= 0);
   assert(elems >= 1);
   assert(elems <= 4);

   if (!compatible_types(type, param->DataType)) {
      _mesa_error(ctx, GL_INVALID_OPERATION, "glUniform(type mismatch)");
      return;
   }

   if (index + offset > (GLint) program->Parameters->Size) {
      /* out of bounds! */
      return;
   }

   if (param->Type == PROGRAM_SAMPLER) {
      /* This controls which texture unit which is used by a sampler */
      GLboolean changed = GL_FALSE;
      GLint i;

      /* this should have been caught by the compatible_types() check */
      ASSERT(type == GL_INT);

      /* loop over number of samplers to change */
      for (i = 0; i < count; i++) {
         GLuint sampler =
            (GLuint) program->Parameters->ParameterValues[index + offset + i][0];
         GLuint texUnit = ((GLuint *) values)[i];

         /* check that the sampler (tex unit index) is legal */
         if (texUnit >= ctx->Const.MaxTextureImageUnits) {
            _mesa_error(ctx, GL_INVALID_VALUE,
                        "glUniform1(invalid sampler/tex unit index for '%s')",
                        param->Name);
            return;
         }

         /* This maps a sampler to a texture unit: */
         if (sampler < MAX_SAMPLERS) {
#if 0
            printf("Set program %p sampler %d '%s' to unit %u\n",
		   program, sampler, param->Name, texUnit);
#endif
            if (program->SamplerUnits[sampler] != texUnit) {
               program->SamplerUnits[sampler] = texUnit;
               changed = GL_TRUE;
            }
         }
      }

      if (changed) {
         /* When a sampler's value changes it usually requires rewriting
          * a GPU program's TEX instructions since there may not be a
          * sampler->texture lookup table.  We signal this with the
          * ProgramStringNotify() callback.
          */
         FLUSH_VERTICES(ctx, _NEW_TEXTURE | _NEW_PROGRAM);
         _mesa_update_shader_textures_used(program);
         /* Do we need to care about the return value here?
          * This should not be the first time the driver was notified of
          * this program.
          */
         (void) ctx->Driver.ProgramStringNotify(ctx, program->Target, program);
      }
   }
   else {
      /* ordinary uniform variable */
      const GLboolean isUniformBool = is_boolean_type(param->DataType);
      const GLenum basicType = base_uniform_type(type);
      const GLint slots = (param->Size + 3) / 4;
      const GLint typeSize = _mesa_sizeof_glsl_type(param->DataType);
      GLsizei k, i;

      if ((GLint) param->Size > typeSize) {
         /* an array */
         /* we'll ignore extra data below */
      }
      else {
         /* non-array: count must be at most one; count == 0 is handled by the loop below */
         if (count > 1) {
            _mesa_error(ctx, GL_INVALID_OPERATION,
                        "glUniform(uniform '%s' is not an array)",
                        param->Name);
            return;
         }
      }

      /* loop over number of array elements */
      for (k = 0; k < count; k++) {
         GLfloat *uniformVal;

         if (offset + k >= slots) {
            /* Extra array data is ignored */
            break;
         }

         /* uniformVal (the destination) is always float[4] */
         uniformVal = program->Parameters->ParameterValues[index + offset + k];

         if (basicType == GL_INT) {
            /* convert user's ints to floats */
            const GLint *iValues = ((const GLint *) values) + k * elems;
            for (i = 0; i < elems; i++) {
               uniformVal[i] = (GLfloat) iValues[i];
            }
         }
         else if (basicType == GL_UNSIGNED_INT) {
            /* convert user's uints to floats */
            const GLuint *iValues = ((const GLuint *) values) + k * elems;
            for (i = 0; i < elems; i++) {
               uniformVal[i] = (GLfloat) iValues[i];
            }
         }
         else {
            const GLfloat *fValues = ((const GLfloat *) values) + k * elems;
            assert(basicType == GL_FLOAT);
            for (i = 0; i < elems; i++) {
               uniformVal[i] = fValues[i];
            }
         }

         /* if the uniform is bool-valued, convert to 1.0 or 0.0 */
         if (isUniformBool) {
            for (i = 0; i < elems; i++) {
               uniformVal[i] = uniformVal[i] ? 1.0f : 0.0f;
            }
         }
      }
   }
}

                                /**
 * Called via glUniform*() functions.
 */
void
_mesa_uniform(struct gl_context *ctx, struct gl_shader_program *shProg,
	      GLint location, GLsizei count,
              const GLvoid *values, GLenum type)
{
   struct gl_uniform *uniform;
   GLint elems, offset;

   if (!shProg || !shProg->LinkStatus) {
      _mesa_error(ctx, GL_INVALID_OPERATION, "glUniform(program not linked)");
      return;
   }

   if (location == -1)
      return;   /* The standard specifies this as a no-op */

   if (location < -1) {
      _mesa_error(ctx, GL_INVALID_OPERATION, "glUniform(location=%d)",
                  location);
      return;
   }

   split_location_offset(&location, &offset);

   if (location < 0 || location >= (GLint) shProg->Uniforms->NumUniforms) {
      _mesa_error(ctx, GL_INVALID_VALUE, "glUniform(location=%d)", location);
      return;
   }

   if (count < 0) {
      _mesa_error(ctx, GL_INVALID_VALUE, "glUniform(count < 0)");
      return;
   }

   elems = _mesa_sizeof_glsl_type(type);

   FLUSH_VERTICES(ctx, _NEW_PROGRAM_CONSTANTS);

   uniform = &shProg->Uniforms->Uniforms[location];

   if (ctx->Shader.Flags & GLSL_UNIFORMS) {
      const GLenum basicType = base_uniform_type(type);
      GLint i;
      printf("Mesa: set program %u uniform %s (loc %d) to: ",
	     shProg->Name, uniform->Name, location);
      if (basicType == GL_INT) {
         const GLint *v = (const GLint *) values;
         for (i = 0; i < count * elems; i++) {
            printf("%d ", v[i]);
         }
      }
      else if (basicType == GL_UNSIGNED_INT) {
         const GLuint *v = (const GLuint *) values;
         for (i = 0; i < count * elems; i++) {
            printf("%u ", v[i]);
         }
      }
      else {
         const GLfloat *v = (const GLfloat *) values;
         assert(basicType == GL_FLOAT);
         for (i = 0; i < count * elems; i++) {
            printf("%g ", v[i]);
         }
      }
      printf("\n");
   }

   /* A uniform var may be used by both a vertex shader and a fragment
    * shader.  We may need to update one or both shader's uniform here:
    */
   if (shProg->VertexProgram) {
      /* convert uniform location to program parameter index */
      GLint index = uniform->VertPos;
      if (index >= 0) {
         set_program_uniform(ctx, &shProg->VertexProgram->Base,
                             index, offset, type, count, elems, values);
      }
   }

   if (shProg->FragmentProgram) {
      /* convert uniform location to program parameter index */
      GLint index = uniform->FragPos;
      if (index >= 0) {
         set_program_uniform(ctx, &shProg->FragmentProgram->Base,
                             index, offset, type, count, elems, values);
      }
   }

   if (shProg->GeometryProgram) {
      /* convert uniform location to program parameter index */
      GLint index = uniform->GeomPos;
      if (index >= 0) {
         set_program_uniform(ctx, &shProg->GeometryProgram->Base,
                             index, offset, type, count, elems, values);
      }
   }

   uniform->Initialized = GL_TRUE;
}

static void
get_matrix_dims(GLenum type, GLint *rows, GLint *cols)
{
   switch (type) {
   case GL_FLOAT_MAT2:
      *rows = *cols = 2;
      break;
   case GL_FLOAT_MAT2x3:
      *rows = 3;
      *cols = 2;
      break;
   case GL_FLOAT_MAT2x4:
      *rows = 4;
      *cols = 2;
      break;
   case GL_FLOAT_MAT3:
      *rows = 3;
      *cols = 3;
      break;
   case GL_FLOAT_MAT3x2:
      *rows = 2;
      *cols = 3;
      break;
   case GL_FLOAT_MAT3x4:
      *rows = 4;
      *cols = 3;
      break;
   case GL_FLOAT_MAT4:
      *rows = 4;
      *cols = 4;
      break;
   case GL_FLOAT_MAT4x2:
      *rows = 2;
      *cols = 4;
      break;
   case GL_FLOAT_MAT4x3:
      *rows = 3;
      *cols = 4;
      break;
   default:
      *rows = *cols = 0;
   }
}

/**
 * Set a matrix-valued program parameter.
 */
static void
set_program_uniform_matrix(struct gl_context *ctx, struct gl_program *program,
                           GLuint index, GLuint offset,
                           GLuint count, GLuint rows, GLuint cols,
                           GLboolean transpose, const GLfloat *values)
{
   GLuint mat, row, col;
   GLuint src = 0;
   const struct gl_program_parameter * param = &program->Parameters->Parameters[index];
   const GLuint slots = (param->Size + 3) / 4;
   const GLint typeSize = _mesa_sizeof_glsl_type(param->DataType);
   GLint nr, nc;

   /* check that the number of rows, columns is correct */
   get_matrix_dims(param->DataType, &nr, &nc);
   if (rows != nr || cols != nc) {
      _mesa_error(ctx, GL_INVALID_OPERATION,
                  "glUniformMatrix(matrix size mismatch)");
      return;
   }

   if ((GLint) param->Size <= typeSize) {
      /* non-array: count must be at most one; count == 0 is handled by the loop below */
      if (count > 1) {
         _mesa_error(ctx, GL_INVALID_OPERATION,
                     "glUniformMatrix(uniform is not an array)");
         return;
      }
   }

   /*
    * Note: the _columns_ of a matrix are stored in program registers, not
    * the rows.  So, the loops below look a little funny.
    * XXX could optimize this a bit...
    */

   /* loop over matrices */
   for (mat = 0; mat < count; mat++) {

      /* each matrix: */
      for (col = 0; col < cols; col++) {
         GLfloat *v;
         if (offset >= slots) {
            /* Ignore writes beyond the end of (the used part of) an array */
            return;
         }
         v = program->Parameters->ParameterValues[index + offset];
         for (row = 0; row < rows; row++) {
            if (transpose) {
               v[row] = values[src + row * cols + col];
            }
            else {
               v[row] = values[src + col * rows + row];
            }
         }

         offset++;
      }

      src += rows * cols;  /* next matrix */
   }
}

/**
 * Called by glUniformMatrix*() functions.
 * Note: cols=2, rows=4  ==>  array[2] of vec4
 */
void
_mesa_uniform_matrix(struct gl_context *ctx, struct gl_shader_program *shProg,
		     GLint cols, GLint rows,
                     GLint location, GLsizei count,
                     GLboolean transpose, const GLfloat *values)
{
   struct gl_uniform *uniform;
   GLint offset;

   if (!shProg || !shProg->LinkStatus) {
      _mesa_error(ctx, GL_INVALID_OPERATION,
         "glUniformMatrix(program not linked)");
      return;
   }

   if (location == -1)
      return;   /* The standard specifies this as a no-op */

   if (location < -1) {
      _mesa_error(ctx, GL_INVALID_OPERATION, "glUniformMatrix(location)");
      return;
   }

   split_location_offset(&location, &offset);

   if (location < 0 || location >= (GLint) shProg->Uniforms->NumUniforms) {
      _mesa_error(ctx, GL_INVALID_VALUE, "glUniformMatrix(location)");
      return;
   }
   if (values == NULL) {
      _mesa_error(ctx, GL_INVALID_VALUE, "glUniformMatrix");
      return;
   }

   FLUSH_VERTICES(ctx, _NEW_PROGRAM_CONSTANTS);

   uniform = &shProg->Uniforms->Uniforms[location];

   if (shProg->VertexProgram) {
      /* convert uniform location to program parameter index */
      GLint index = uniform->VertPos;
      if (index >= 0) {
         set_program_uniform_matrix(ctx, &shProg->VertexProgram->Base,
                                    index, offset,
                                    count, rows, cols, transpose, values);
      }
   }

   if (shProg->FragmentProgram) {
      /* convert uniform location to program parameter index */
      GLint index = uniform->FragPos;
      if (index >= 0) {
         set_program_uniform_matrix(ctx, &shProg->FragmentProgram->Base,
                                    index, offset,
                                    count, rows, cols, transpose, values);
      }
   }

   if (shProg->GeometryProgram) {
      /* convert uniform location to program parameter index */
      GLint index = uniform->GeomPos;
      if (index >= 0) {
         set_program_uniform_matrix(ctx, &shProg->GeometryProgram->Base,
                                    index, offset,
                                    count, rows, cols, transpose, values);
      }
   }

   uniform->Initialized = GL_TRUE;
}

static struct gl_program_parameter *
get_uniform_parameter(const struct gl_shader_program *shProg, GLuint index)
{
   const struct gl_program *prog = NULL;
   GLint progPos;

   progPos = shProg->Uniforms->Uniforms[index].VertPos;
   if (progPos >= 0) {
      prog = &shProg->VertexProgram->Base;
   }
   else {
      progPos = shProg->Uniforms->Uniforms[index].FragPos;
      if (progPos >= 0) {
         prog = &shProg->FragmentProgram->Base;
      } else {
         progPos = shProg->Uniforms->Uniforms[index].GeomPos;
         if (progPos >= 0) {
            prog = &shProg->GeometryProgram->Base;
         }
      }
   }

   if (!prog || progPos < 0)
      return NULL; /* should never happen */

   return &prog->Parameters->Parameters[progPos];
}

/**
 * GLGL uniform arrays and structs require special handling.
 *
 * The GL_ARB_shader_objects spec says that if you use
 * glGetUniformLocation to get the location of an array, you CANNOT
 * access other elements of the array by adding an offset to the
 * returned location.  For example, you must call
 * glGetUniformLocation("foo[16]") if you want to set the 16th element
 * of the array with glUniform().
 *
 * HOWEVER, some other OpenGL drivers allow accessing array elements
 * by adding an offset to the returned array location.  And some apps
 * seem to depend on that behaviour.
 *
 * Mesa's gl_uniform_list doesn't directly support this since each
 * entry in the list describes one uniform variable, not one uniform
 * element.  We could insert dummy entries in the list for each array
 * element after [0] but that causes complications elsewhere.
 *
 * We solve this problem by encoding two values in the location that's
 * returned by glGetUniformLocation():
 *  a) index into gl_uniform_list::Uniforms[] for the uniform
 *  b) an array/field offset (0 for simple types)
 *
 * These two values are encoded in the high and low halves of a GLint.
 * By putting the uniform number in the high part and the offset in the
 * low part, we can support the unofficial ability to index into arrays
 * by adding offsets to the location value.
 */
static void
merge_location_offset(GLint *location, GLint offset)
{
   *location = (*location << 16) | offset;
}

/**
 * Called via glGetUniformLocation().
 *
 * The return value will encode two values, the uniform location and an
 * offset (used for arrays, structs).
 */
GLint
_mesa_get_uniform_location(struct gl_context *ctx, struct gl_shader_program *shProg,
			   const GLchar *name)
{
   GLint offset = 0, location = -1;

   if (shProg->LinkStatus == GL_FALSE) {
      _mesa_error(ctx, GL_INVALID_OPERATION, "glGetUniformfv(program)");
      return -1;
   }

   /* XXX we should return -1 if the uniform was declared, but not
    * actually used.
    */

   /* XXX we need to be able to parse uniform names for structs and arrays
    * such as:
    *   mymatrix[1]
    *   mystruct.field1
    */

   {
      /* handle 1-dimension arrays here... */
      const char *c = strchr(name, '[');
      if (c) {
         /* truncate name at [ */
         const GLint len = c - name;
         GLchar *newName = (GLchar *)malloc(len + 1);
         if (!newName)
            return -1; /* out of mem */
         memcpy(newName, name, len);
         newName[len] = 0;

         location = _mesa_lookup_uniform(shProg->Uniforms, newName);
         if (location >= 0) {
            const GLint element = atoi(c + 1);
            if (element > 0) {
               /* get type of the uniform array element */
               struct gl_program_parameter *p;
               p = get_uniform_parameter(shProg, location);
               if (p) {
                  GLint rows, cols;
                  get_matrix_dims(p->DataType, &rows, &cols);
                  if (rows < 1)
                     rows = 1;
                  offset = element * rows;
               }
            }
         }

         free(newName);
      }
   }

   if (location < 0) {
      location = _mesa_lookup_uniform(shProg->Uniforms, name);
   }

   if (location >= 0) {
      merge_location_offset(&location, offset);
   }

   return location;
}

/**
 * Look for a float vector in the given parameter list.  The float vector
 * may be of length 1, 2, 3 or 4.  If swizzleOut is non-null, we'll try
 * swizzling to find a match.
 * \param list  the parameter list to search
 * \param v  the float vector to search for
 * \param vSize  number of element in v
 * \param posOut  returns the position of the constant, if found
 * \param swizzleOut  returns a swizzle mask describing location of the
 *                    vector elements if found.
 * \return GL_TRUE if found, GL_FALSE if not found
 */
GLboolean
_mesa_lookup_parameter_constant(const struct gl_program_parameter_list *list,
                                const GLfloat v[], GLuint vSize,
                                GLint *posOut, GLuint *swizzleOut)
{
   GLuint i;

   assert(vSize >= 1);
   assert(vSize <= 4);

   if (!list) {
      *posOut = -1;
      return GL_FALSE;
   }

   for (i = 0; i < list->NumParameters; i++) {
      if (list->Parameters[i].Type == PROGRAM_CONSTANT) {
         if (!swizzleOut) {
            /* swizzle not allowed */
            GLuint j, match = 0;
            for (j = 0; j < vSize; j++) {
               if (v[j] == list->ParameterValues[i][j])
                  match++;
            }
            if (match == vSize) {
               *posOut = i;
               return GL_TRUE;
            }
         }
         else {
            /* try matching w/ swizzle */
             if (vSize == 1) {
                /* look for v[0] anywhere within float[4] value */
                GLuint j;
                for (j = 0; j < list->Parameters[i].Size; j++) {
                   if (list->ParameterValues[i][j] == v[0]) {
                      /* found it */
                      *posOut = i;
                      *swizzleOut = MAKE_SWIZZLE4(j, j, j, j);
                      return GL_TRUE;
                   }
                }
             }
             else if (vSize <= list->Parameters[i].Size) {
                /* see if we can match this constant (with a swizzle) */
                GLuint swz[4];
                GLuint match = 0, j, k;
                for (j = 0; j < vSize; j++) {
                   if (v[j] == list->ParameterValues[i][j]) {
                      swz[j] = j;
                      match++;
                   }
                   else {
                      for (k = 0; k < list->Parameters[i].Size; k++) {
                         if (v[j] == list->ParameterValues[i][k]) {
                            swz[j] = k;
                            match++;
                            break;
                         }
                      }
                   }
                }
                /* smear last value to remaining positions */
                for (; j < 4; j++)
                   swz[j] = swz[j-1];

                if (match == vSize) {
                   *posOut = i;
                   *swizzleOut = MAKE_SWIZZLE4(swz[0], swz[1], swz[2], swz[3]);
                   return GL_TRUE;
                }
             }
         }
      }
   }

   *posOut = -1;
   return GL_FALSE;
}

/**
 * Return a bitmask of the Mesa state flags (_NEW_* values) which would
 * indicate that the given context state may have changed.
 * The bitmask is used during validation to determine if we need to update
 * vertex/fragment program parameters (like "state.material.color") when
 * some GL state has changed.
 */
GLbitfield
_mesa_program_state_flags(const gl_state_index state[STATE_LENGTH])
{
   switch (state[0]) {
   case STATE_MATERIAL:
   case STATE_LIGHT:
   case STATE_LIGHTMODEL_AMBIENT:
   case STATE_LIGHTMODEL_SCENECOLOR:
   case STATE_LIGHTPROD:
      return _NEW_LIGHT;

   case STATE_TEXGEN:
   case STATE_TEXENV_COLOR:
      return _NEW_TEXTURE;

   case STATE_FOG_COLOR:
   case STATE_FOG_PARAMS:
      return _NEW_FOG;

   case STATE_CLIPPLANE:
      return _NEW_TRANSFORM;

   case STATE_POINT_SIZE:
   case STATE_POINT_ATTENUATION:
      return _NEW_POINT;

   case STATE_MODELVIEW_MATRIX:
      return _NEW_MODELVIEW;
   case STATE_PROJECTION_MATRIX:
      return _NEW_PROJECTION;
   case STATE_MVP_MATRIX:
      return _NEW_MODELVIEW | _NEW_PROJECTION;
   case STATE_TEXTURE_MATRIX:
      return _NEW_TEXTURE_MATRIX;
   case STATE_PROGRAM_MATRIX:
      return _NEW_TRACK_MATRIX;

   case STATE_DEPTH_RANGE:
      return _NEW_VIEWPORT;

   case STATE_FRAGMENT_PROGRAM:
   case STATE_VERTEX_PROGRAM:
      return _NEW_PROGRAM;

   case STATE_NORMAL_SCALE:
      return _NEW_MODELVIEW;

   case STATE_INTERNAL:
      switch (state[1]) {
      case STATE_CURRENT_ATTRIB:
         return _NEW_CURRENT_ATTRIB;

      case STATE_NORMAL_SCALE:
         return _NEW_MODELVIEW;

      case STATE_TEXRECT_SCALE:
      case STATE_SHADOW_AMBIENT:
      case STATE_ROT_MATRIX_0:
      case STATE_ROT_MATRIX_1:
	 return _NEW_TEXTURE;
      case STATE_FOG_PARAMS_OPTIMIZED:
	 return _NEW_FOG;
      case STATE_POINT_SIZE_CLAMPED:
      case STATE_POINT_SIZE_IMPL_CLAMP:
         return _NEW_POINT | _NEW_MULTISAMPLE;
      case STATE_LIGHT_SPOT_DIR_NORMALIZED:
      case STATE_LIGHT_POSITION:
      case STATE_LIGHT_POSITION_NORMALIZED:
      case STATE_LIGHT_HALF_VECTOR:
         return _NEW_LIGHT;

      case STATE_PT_SCALE:
      case STATE_PT_BIAS:
         return _NEW_PIXEL;

      case STATE_FB_SIZE:
      case STATE_FB_WPOS_Y_TRANSFORM:
         return _NEW_BUFFERS;

      default:
         /* unknown state indexes are silently ignored and
         *  no flag set, since it is handled by the driver.
         */
	 return 0;
      }

   default:
      _mesa_problem(NULL, "unexpected state[0] in make_state_flags()");
      return 0;
   }
}

static void
append(char *dst, const char *src)
{
   while (*dst)
      dst++;
   while (*src)
     *dst++ = *src++;
   *dst = 0;
}


/**
 * Convert token 'k' to a string, append it onto 'dst' string.
 */
static void
append_token(char *dst, gl_state_index k)
{
   switch (k) {
   case STATE_MATERIAL:
      append(dst, "material");
      break;
   case STATE_LIGHT:
      append(dst, "light");
      break;
   case STATE_LIGHTMODEL_AMBIENT:
      append(dst, "lightmodel.ambient");
      break;
   case STATE_LIGHTMODEL_SCENECOLOR:
      break;
   case STATE_LIGHTPROD:
      append(dst, "lightprod");
      break;
   case STATE_TEXGEN:
      append(dst, "texgen");
      break;
   case STATE_FOG_COLOR:
      append(dst, "fog.color");
      break;
   case STATE_FOG_PARAMS:
      append(dst, "fog.params");
      break;
   case STATE_CLIPPLANE:
      append(dst, "clip");
      break;
   case STATE_POINT_SIZE:
      append(dst, "point.size");
      break;
   case STATE_POINT_ATTENUATION:
      append(dst, "point.attenuation");
      break;
   case STATE_MODELVIEW_MATRIX:
      append(dst, "matrix.modelview");
      break;
   case STATE_PROJECTION_MATRIX:
      append(dst, "matrix.projection");
      break;
   case STATE_MVP_MATRIX:
      append(dst, "matrix.mvp");
      break;
   case STATE_TEXTURE_MATRIX:
      append(dst, "matrix.texture");
      break;
   case STATE_PROGRAM_MATRIX:
      append(dst, "matrix.program");
      break;
   case STATE_MATRIX_INVERSE:
      append(dst, ".inverse");
      break;
   case STATE_MATRIX_TRANSPOSE:
      append(dst, ".transpose");
      break;
   case STATE_MATRIX_INVTRANS:
      append(dst, ".invtrans");
      break;
   case STATE_AMBIENT:
      append(dst, ".ambient");
      break;
   case STATE_DIFFUSE:
      append(dst, ".diffuse");
      break;
   case STATE_SPECULAR:
      append(dst, ".specular");
      break;
   case STATE_EMISSION:
      append(dst, ".emission");
      break;
   case STATE_SHININESS:
      append(dst, "lshininess");
      break;
   case STATE_HALF_VECTOR:
      append(dst, ".half");
      break;
   case STATE_POSITION:
      append(dst, ".position");
      break;
   case STATE_ATTENUATION:
      append(dst, ".attenuation");
      break;
   case STATE_SPOT_DIRECTION:
      append(dst, ".spot.direction");
      break;
   case STATE_SPOT_CUTOFF:
      append(dst, ".spot.cutoff");
      break;
   case STATE_TEXGEN_EYE_S:
      append(dst, ".eye.s");
      break;
   case STATE_TEXGEN_EYE_T:
      append(dst, ".eye.t");
      break;
   case STATE_TEXGEN_EYE_R:
      append(dst, ".eye.r");
      break;
   case STATE_TEXGEN_EYE_Q:
      append(dst, ".eye.q");
      break;
   case STATE_TEXGEN_OBJECT_S:
      append(dst, ".object.s");
      break;
   case STATE_TEXGEN_OBJECT_T:
      append(dst, ".object.t");
      break;
   case STATE_TEXGEN_OBJECT_R:
      append(dst, ".object.r");
      break;
   case STATE_TEXGEN_OBJECT_Q:
      append(dst, ".object.q");
      break;
   case STATE_TEXENV_COLOR:
      append(dst, "texenv");
      break;
   case STATE_DEPTH_RANGE:
      append(dst, "depth.range");
      break;
   case STATE_VERTEX_PROGRAM:
   case STATE_FRAGMENT_PROGRAM:
      break;
   case STATE_ENV:
      append(dst, "env");
      break;
   case STATE_LOCAL:
      append(dst, "local");
      break;
   /* BEGIN internal state vars */
   case STATE_INTERNAL:
      append(dst, ".internal.");
      break;
   case STATE_CURRENT_ATTRIB:
      append(dst, "current");
      break;
   case STATE_NORMAL_SCALE:
      append(dst, "normalScale");
      break;
   case STATE_TEXRECT_SCALE:
      append(dst, "texrectScale");
      break;
   case STATE_FOG_PARAMS_OPTIMIZED:
      append(dst, "fogParamsOptimized");
      break;
   case STATE_POINT_SIZE_CLAMPED:
      append(dst, "pointSizeClamped");
      break;
   case STATE_POINT_SIZE_IMPL_CLAMP:
      append(dst, "pointSizeImplClamp");
      break;
   case STATE_LIGHT_SPOT_DIR_NORMALIZED:
      append(dst, "lightSpotDirNormalized");
      break;
   case STATE_LIGHT_POSITION:
      append(dst, "lightPosition");
      break;
   case STATE_LIGHT_POSITION_NORMALIZED:
      append(dst, "light.position.normalized");
      break;
   case STATE_LIGHT_HALF_VECTOR:
      append(dst, "lightHalfVector");
      break;
   case STATE_PT_SCALE:
      append(dst, "PTscale");
      break;
   case STATE_PT_BIAS:
      append(dst, "PTbias");
      break;
   case STATE_SHADOW_AMBIENT:
      append(dst, "CompareFailValue");
      break;
   case STATE_FB_SIZE:
      append(dst, "FbSize");
      break;
   case STATE_FB_WPOS_Y_TRANSFORM:
      append(dst, "FbWposYTransform");
      break;
   case STATE_ROT_MATRIX_0:
      append(dst, "rotMatrixRow0");
      break;
   case STATE_ROT_MATRIX_1:
      append(dst, "rotMatrixRow1");
      break;
   default:
      /* probably STATE_INTERNAL_DRIVER+i (driver private state) */
      append(dst, "driverState");
   }
}

static void
append_face(char *dst, GLint face)
{
   if (face == 0)
      append(dst, "front.");
   else
      append(dst, "back.");
}

static void
append_index(char *dst, GLint index)
{
   char s[20];
   sprintf(s, "[%d]", index);
   append(dst, s);
}

/**
 * Make a string from the given state vector.
 * For example, return "state.matrix.texture[2].inverse".
 * Use free() to deallocate the string.
 */
char *
_mesa_program_state_string(const gl_state_index state[STATE_LENGTH])
{
   char str[1000] = "";
   char tmp[30];

   append(str, "state.");
   append_token(str, state[0]);

   switch (state[0]) {
   case STATE_MATERIAL:
      append_face(str, state[1]);
      append_token(str, state[2]);
      break;
   case STATE_LIGHT:
      append_index(str, state[1]); /* light number [i]. */
      append_token(str, state[2]); /* coefficients */
      break;
   case STATE_LIGHTMODEL_AMBIENT:
      append(str, "lightmodel.ambient");
      break;
   case STATE_LIGHTMODEL_SCENECOLOR:
      if (state[1] == 0) {
         append(str, "lightmodel.front.scenecolor");
      }
      else {
         append(str, "lightmodel.back.scenecolor");
      }
      break;
   case STATE_LIGHTPROD:
      append_index(str, state[1]); /* light number [i]. */
      append_face(str, state[2]);
      append_token(str, state[3]);
      break;
   case STATE_TEXGEN:
      append_index(str, state[1]); /* tex unit [i] */
      append_token(str, state[2]); /* plane coef */
      break;
   case STATE_TEXENV_COLOR:
      append_index(str, state[1]); /* tex unit [i] */
      append(str, "color");
      break;
   case STATE_CLIPPLANE:
      append_index(str, state[1]); /* plane [i] */
      append(str, ".plane");
      break;
   case STATE_MODELVIEW_MATRIX:
   case STATE_PROJECTION_MATRIX:
   case STATE_MVP_MATRIX:
   case STATE_TEXTURE_MATRIX:
   case STATE_PROGRAM_MATRIX:
      {
         /* state[0] = modelview, projection, texture, etc. */
         /* state[1] = which texture matrix or program matrix */
         /* state[2] = first row to fetch */
         /* state[3] = last row to fetch */
         /* state[4] = transpose, inverse or invtrans */
         const gl_state_index mat = state[0];
         const GLuint index = (GLuint) state[1];
         const GLuint firstRow = (GLuint) state[2];
         const GLuint lastRow = (GLuint) state[3];
         const gl_state_index modifier = state[4];
         if (index ||
             mat == STATE_TEXTURE_MATRIX ||
             mat == STATE_PROGRAM_MATRIX)
            append_index(str, index);
         if (modifier)
            append_token(str, modifier);
         if (firstRow == lastRow)
            sprintf(tmp, ".row[%d]", firstRow);
         else
            sprintf(tmp, ".row[%d..%d]", firstRow, lastRow);
         append(str, tmp);
      }
      break;
   case STATE_POINT_SIZE:
      break;
   case STATE_POINT_ATTENUATION:
      break;
   case STATE_FOG_PARAMS:
      break;
   case STATE_FOG_COLOR:
      break;
   case STATE_DEPTH_RANGE:
      break;
   case STATE_FRAGMENT_PROGRAM:
   case STATE_VERTEX_PROGRAM:
      /* state[1] = {STATE_ENV, STATE_LOCAL} */
      /* state[2] = parameter index          */
      append_token(str, state[1]);
      append_index(str, state[2]);
      break;
   case STATE_NORMAL_SCALE:
      break;
   case STATE_INTERNAL:
      append_token(str, state[1]);
      if (state[1] == STATE_CURRENT_ATTRIB)
         append_index(str, state[2]);
       break;
   default:
      _mesa_problem(NULL, "Invalid state in _mesa_program_state_string");
      break;
   }

   return _mesa_strdup(str);
}

/**
 * Free memory which was allocated with either _mesa_align_malloc()
 * or _mesa_align_calloc().
 * \param ptr pointer to the memory to be freed.
 * The actual address to free is stored in the word immediately before the
 * address the client sees.
 */
void
_mesa_align_free(void *ptr)
{
#if defined(HAVE_POSIX_MEMALIGN)
   free(ptr);
#elif defined(_WIN32) && defined(_MSC_VER)
   _aligned_free(ptr);
#else
   void **cubbyHole = (void **) ((char *) ptr - sizeof(void *));
   void *realAddr = *cubbyHole;
   free(realAddr);
#endif /* defined(HAVE_POSIX_MEMALIGN) */
}

/**********************************************************************/
/** \name Memory */
/*@{*/

/**
 * Allocate aligned memory.
 *
 * \param bytes number of bytes to allocate.
 * \param alignment alignment (must be greater than zero).
 * 
 * Allocates extra memory to accommodate rounding up the address for
 * alignment and to record the real malloc address.
 *
 * \sa _mesa_align_free().
 */
void *
_mesa_align_malloc(size_t bytes, unsigned long alignment)
{
#if defined(HAVE_POSIX_MEMALIGN)
   void *mem;
   int err = posix_memalign(& mem, alignment, bytes);
   (void) err;
   return mem;
#elif defined(_WIN32) && defined(_MSC_VER)
   return _aligned_malloc(bytes, alignment);
#else
   uintptr_t ptr, buf;

   ASSERT( alignment > 0 );

   ptr = (uintptr_t) malloc(bytes + alignment + sizeof(void *));
   if (!ptr)
      return NULL;

   buf = (ptr + alignment + sizeof(void *)) & ~(uintptr_t)(alignment - 1);
   *(uintptr_t *)(buf - sizeof(void *)) = ptr;

#ifdef DEBUG
   /* mark the non-aligned area */
   while ( ptr < buf - sizeof(void *) ) {
      *(unsigned long *)ptr = 0xcdcdcdcd;
      ptr += sizeof(unsigned long);
   }
#endif

   return (void *) buf;
#endif /* defined(HAVE_POSIX_MEMALIGN) */
}

/**********************************************************************/
/** \name String */
/*@{*/

/**
 * Implemented using malloc() and strcpy.
 * Note that NULL is handled accordingly.
 */
char *
_mesa_strdup( const char *s )
{
   if (s) {
      size_t l = strlen(s);
      char *s2 = (char *) malloc(l + 1);
      if (s2)
         strcpy(s2, s);
      return s2;
   }
   else {
      return NULL;
   }
}

/**
 * Reallocate memory, with alignment.
 */
void *
_mesa_align_realloc(void *oldBuffer, size_t oldSize, size_t newSize,
                    unsigned long alignment)
{
#if defined(_WIN32) && defined(_MSC_VER)
   (void) oldSize;
   return _aligned_realloc(oldBuffer, newSize, alignment);
#else
   const size_t copySize = (oldSize < newSize) ? oldSize : newSize;
   void *newBuf = _mesa_align_malloc(newSize, alignment);
   if (newBuf && oldBuffer && copySize > 0) {
      memcpy(newBuf, oldBuffer, copySize);
   }
   if (oldBuffer)
      _mesa_align_free(oldBuffer);
   return newBuf;
#endif
}



/** Reallocate memory */
void *
_mesa_realloc(void *oldBuffer, size_t oldSize, size_t newSize)
{
   const size_t copySize = (oldSize < newSize) ? oldSize : newSize;
   void *newBuffer = malloc(newSize);
   if (newBuffer && oldBuffer && copySize > 0)
      memcpy(newBuffer, oldBuffer, copySize);
   if (oldBuffer)
      free(oldBuffer);
   return newBuffer;
}

/**
 * Free a parameter list and all its parameters
 */
void
_mesa_free_parameter_list(struct gl_program_parameter_list *paramList)
{
   GLuint i;
   for (i = 0; i < paramList->NumParameters; i++) {
      if (paramList->Parameters[i].Name)
	 free((void *) paramList->Parameters[i].Name);
   }
   free(paramList->Parameters);
   if (paramList->ParameterValues)
      _mesa_align_free(paramList->ParameterValues);
   free(paramList);
}

void
_mesa_free_uniform_list(struct gl_uniform_list *list)
{
   GLuint i;

   if (!list)
      return;

   for (i = 0; i < list->NumUniforms; i++) {
      free((void *) list->Uniforms[i].Name);
   }
   free(list->Uniforms);
   free(list);
}

/**
 * Report debug information.  Print error message to stderr via fprintf().
 * No-op if DEBUG mode not enabled.
 * 
 * \param ctx GL context.
 * \param fmtString printf()-style format string, followed by optional args.
 */
void
_mesa_debug( const struct gl_context *ctx, const char *fmtString, ... )
{
#ifdef DEBUG
   char s[MAXSTRING];
   va_list args;
   va_start(args, fmtString);
   vsnprintf(s, MAXSTRING, fmtString, args);
   va_end(args);
   output_if_debug("Mesa", s, GL_FALSE);
#endif /* DEBUG */
   (void) ctx;
   (void) fmtString;
}


#define MAXSTRING 4000  /* for vsnprintf() */

/**********************************************************************/
/** \name Diagnostics */
/*@{*/

static void
output_if_debug(const char *prefixString, const char *outputString,
                GLboolean newline)
{
   static int debug = -1;

   /* Check the MESA_DEBUG environment variable if it hasn't
    * been checked yet.  We only have to check it once...
    */
   if (debug == -1) {
      char *env = _mesa_getenv("MESA_DEBUG");

      /* In a debug build, we print warning messages *unless*
       * MESA_DEBUG is 0.  In a non-debug build, we don't
       * print warning messages *unless* MESA_DEBUG is
       * set *to any value*.
       */
#ifdef DEBUG
      debug = (env != NULL && atoi(env) == 0) ? 0 : 1;
#else
      debug = (env != NULL) ? 1 : 0;
#endif
   }

   /* Now only print the string if we're required to do so. */
   if (debug) {
      fprintf(stderr, "%s: %s", prefixString, outputString);
      if (newline)
         fprintf(stderr, "\n");

#if defined(_WIN32) && !defined(_WIN32_WCE)
      /* stderr from windows applications without console is not usually 
       * visible, so communicate with the debugger instead */ 
      {
         char buf[4096];
         _mesa_snprintf(buf, sizeof(buf), "%s: %s%s", prefixString, outputString, newline ? "\n" : "");
         OutputDebugStringA(buf);
      }
#endif
   }
}


/**
 * Return string version of GL error code.
 */
static const char *
error_string( GLenum error )
{
   switch (error) {
   case GL_NO_ERROR:
      return "GL_NO_ERROR";
   case GL_INVALID_VALUE:
      return "GL_INVALID_VALUE";
   case GL_INVALID_ENUM:
      return "GL_INVALID_ENUM";
   case GL_INVALID_OPERATION:
      return "GL_INVALID_OPERATION";
   case GL_STACK_OVERFLOW:
      return "GL_STACK_OVERFLOW";
   case GL_STACK_UNDERFLOW:
      return "GL_STACK_UNDERFLOW";
   case GL_OUT_OF_MEMORY:
      return "GL_OUT_OF_MEMORY";
   case GL_TABLE_TOO_LARGE:
      return "GL_TABLE_TOO_LARGE";
   case GL_INVALID_FRAMEBUFFER_OPERATION_EXT:
      return "GL_INVALID_FRAMEBUFFER_OPERATION";
   default:
      return "unknown";
   }
}

/**
 * When a new type of error is recorded, print a message describing
 * previous errors which were accumulated.
 */
static void
flush_delayed_errors( struct gl_context *ctx )
{
   char s[MAXSTRING];

   if (ctx->ErrorDebugCount) {
      _mesa_snprintf(s, MAXSTRING, "%d similar %s errors", 
                     ctx->ErrorDebugCount,
                     error_string(ctx->ErrorValue));

      output_if_debug("Mesa", s, GL_TRUE);

      ctx->ErrorDebugCount = 0;
   }
}

/**********************************************************************/
/** \name Environment vars */
/*@{*/

/**
 * Wrapper for getenv().
 */
char *
_mesa_getenv( const char *var )
{
#if defined(_XBOX) || defined(_WIN32_WCE)
   return NULL;
#else
   return getenv(var);
#endif
}

/**
 * Record an OpenGL state error.  These usually occur when the user
 * passes invalid parameters to a GL function.
 *
 * If debugging is enabled (either at compile-time via the DEBUG macro, or
 * run-time via the MESA_DEBUG environment variable), report the error with
 * _mesa_debug().
 * 
 * \param ctx the GL context.
 * \param error the error value.
 * \param fmtString printf() style format string, followed by optional args
 */
void
_mesa_error( struct gl_context *ctx, GLenum error, const char *fmtString, ... )
{
   static GLint debug = -1;

   /* Check debug environment variable only once:
    */
   if (debug == -1) {
      const char *debugEnv = _mesa_getenv("MESA_DEBUG");

#ifdef DEBUG
      if (debugEnv && strstr(debugEnv, "silent"))
         debug = GL_FALSE;
      else
         debug = GL_TRUE;
#else
      if (debugEnv)
         debug = GL_TRUE;
      else
         debug = GL_FALSE;
#endif
   }

   if (debug) {      
      if (ctx->ErrorValue == error &&
          ctx->ErrorDebugFmtString == fmtString) {
         ctx->ErrorDebugCount++;
      }
      else {
         char s[MAXSTRING], s2[MAXSTRING];
         va_list args;

         flush_delayed_errors( ctx );
         
         va_start(args, fmtString);
         vsnprintf(s, MAXSTRING, fmtString, args);
         va_end(args);

         _mesa_snprintf(s2, MAXSTRING, "%s in %s", error_string(error), s);
         output_if_debug("Mesa: User error", s2, GL_TRUE);
         
         ctx->ErrorDebugFmtString = fmtString;
         ctx->ErrorDebugCount = 0;
      }
   }

   //_mesa_record_error(ctx, error);
   assert(0);
}

/**
 * Return the location/index of the named uniform in the uniform list,
 * or -1 if not found.
 */
GLint
_mesa_lookup_uniform(const struct gl_uniform_list *list, const char *name)
{
   GLuint i;
   for (i = 0; list && i < list->NumUniforms; i++) {
      if (!strcmp(list->Uniforms[i].Name, name)) {
         return i;
      }
   }
   return -1;
}

/**
 * Initialize a new vertex/fragment program object.
 */
static struct gl_program *
_mesa_init_program_struct( struct gl_context *ctx, struct gl_program *prog,
                           GLenum target, GLuint id)
{
   (void) ctx;
   if (prog) {
      GLuint i;
      memset(prog, 0, sizeof(*prog));
      prog->Id = id;
      prog->Target = target;
      prog->Resident = GL_TRUE;
      prog->RefCount = 1;
      prog->Format = GL_PROGRAM_FORMAT_ASCII_ARB;

      /* default mapping from samplers to texture units */
      for (i = 0; i < MAX_SAMPLERS; i++)
         prog->SamplerUnits[i] = i;
   }

   return prog;
}

/**
 * Initialize a new fragment program object.
 */
struct gl_program *
_mesa_init_fragment_program( struct gl_context *ctx, struct gl_fragment_program *prog,
                             GLenum target, GLuint id)
{
   if (prog)
      return _mesa_init_program_struct( ctx, &prog->Base, target, id );
   else
      return NULL;
}


/**
 * Initialize a new vertex program object.
 */
struct gl_program *
_mesa_init_vertex_program( struct gl_context *ctx, struct gl_vertex_program *prog,
                           GLenum target, GLuint id)
{
   if (prog)
      return _mesa_init_program_struct( ctx, &prog->Base, target, id );
   else
      return NULL;
}


/**
 * Initialize a new geometry program object.
 */
struct gl_program *
_mesa_init_geometry_program( struct gl_context *ctx, struct gl_geometry_program *prog,
                             GLenum target, GLuint id)
{
   if (prog)
      return _mesa_init_program_struct( ctx, &prog->Base, target, id );
   else
      return NULL;
}

/**
 * Allocate and initialize a new fragment/vertex program object but
 * don't put it into the program hash table.  Called via
 * ctx->Driver.NewProgram.  May be overridden (ie. replaced) by a
 * device driver function to implement OO deriviation with additional
 * types not understood by this function.
 *
 * \param ctx  context
 * \param id   program id/number
 * \param target  program target/type
 * \return  pointer to new program object
 */
struct gl_program *
_mesa_new_program(struct gl_context *ctx, GLenum target, GLuint id)
{
   struct gl_program *prog;
   switch (target) {
   case GL_VERTEX_PROGRAM_ARB: /* == GL_VERTEX_PROGRAM_NV */
   case GL_VERTEX_STATE_PROGRAM_NV:
      prog = _mesa_init_vertex_program(ctx, CALLOC_STRUCT(gl_vertex_program),
                                       target, id );
      break;
   case GL_FRAGMENT_PROGRAM_NV:
   case GL_FRAGMENT_PROGRAM_ARB:
      prog =_mesa_init_fragment_program(ctx,
                                         CALLOC_STRUCT(gl_fragment_program),
                                         target, id );
      break;
   case MESA_GEOMETRY_PROGRAM:
      prog = _mesa_init_geometry_program(ctx,
                                         CALLOC_STRUCT(gl_geometry_program),
                                         target, id);
      break;
   default:
      _mesa_problem(ctx, "bad target in _mesa_new_program");
      prog = NULL;
   }
   return prog;
}

#define MAX_LOOP_NESTING 50
/* MAX_PROGRAM_TEMPS is a low number (256), and we want to be able to
 * register allocate many temporary values into that small number of
 * temps.  So allow large temporary indices coming into the register
 * allocator.
 */
#define REG_ALLOCATE_MAX_PROGRAM_TEMPS	((1 << INST_INDEX_BITS) - 1)

static GLboolean dbg = GL_FALSE;

#define NO_MASK 0xf

/**
 * Returns the mask of channels (bitmask of WRITEMASK_X,Y,Z,W) which
 * are read from the given src in this instruction, We also provide
 * one optional masks which may mask other components in the dst
 * register
 */
static GLuint
get_src_arg_mask(const struct prog_instruction *inst,
                 GLuint arg, GLuint dst_mask)
{
   GLuint read_mask, channel_mask;
   GLuint comp;

   ASSERT(arg < _mesa_num_inst_src_regs(inst->Opcode));

   /* Form the dst register, find the written channels */
   if (inst->CondUpdate) {
      channel_mask = WRITEMASK_XYZW;
   }
   else {
      switch (inst->Opcode) {
      case OPCODE_MOV:
      case OPCODE_MIN:
      case OPCODE_MAX:
      case OPCODE_ABS:
      case OPCODE_ADD:
      case OPCODE_MAD:
      case OPCODE_MUL:
      case OPCODE_SUB:
         channel_mask = inst->DstReg.WriteMask & dst_mask;
         break;
      case OPCODE_RCP:
      case OPCODE_SIN:
      case OPCODE_COS:
      case OPCODE_RSQ:
      case OPCODE_POW:
      case OPCODE_EX2:
      case OPCODE_LOG:
         channel_mask = WRITEMASK_X;
         break;
      case OPCODE_DP2:
         channel_mask = WRITEMASK_XY;
         break;
      case OPCODE_DP3:
      case OPCODE_XPD:
         channel_mask = WRITEMASK_XYZ;
         break;
      default:
         channel_mask = WRITEMASK_XYZW;
         break;
      }
   }

   /* Now, given the src swizzle and the written channels, find which
    * components are actually read
    */
   read_mask = 0x0;
   for (comp = 0; comp < 4; ++comp) {
      const GLuint coord = GET_SWZ(inst->SrcReg[arg].Swizzle, comp);
      ASSERT(coord < 4);
      if (channel_mask & (1 << comp) && coord <= SWIZZLE_W)
         read_mask |= 1 << coord;
   }

   return read_mask;
}


/**
 * For a MOV instruction, compute a write mask when src register also has
 * a mask
 */
static GLuint
get_dst_mask_for_mov(const struct prog_instruction *mov, GLuint src_mask)
{
   const GLuint mask = mov->DstReg.WriteMask;
   GLuint comp;
   GLuint updated_mask = 0x0;

   ASSERT(mov->Opcode == OPCODE_MOV);

   for (comp = 0; comp < 4; ++comp) {
      GLuint src_comp;
      if ((mask & (1 << comp)) == 0)
         continue;
      src_comp = GET_SWZ(mov->SrcReg[0].Swizzle, comp);
      if ((src_mask & (1 << src_comp)) == 0)
         continue;
      updated_mask |= 1 << comp;
   }

   return updated_mask;
}


/**
 * Ensure that the swizzle is regular.  That is, all of the swizzle
 * terms are SWIZZLE_X,Y,Z,W and not SWIZZLE_ZERO or SWIZZLE_ONE.
 */
static GLboolean
is_swizzle_regular(GLuint swz)
{
   return GET_SWZ(swz,0) <= SWIZZLE_W &&
          GET_SWZ(swz,1) <= SWIZZLE_W &&
          GET_SWZ(swz,2) <= SWIZZLE_W &&
          GET_SWZ(swz,3) <= SWIZZLE_W;
}

/**
 * Free an array of instructions
 */
void
_mesa_free_instructions(struct prog_instruction *inst, GLuint count)
{
   GLuint i;
   for (i = 0; i < count; i++) {
      if (inst[i].Data)
         free(inst[i].Data);
      if (inst[i].Comment)
         free((char *) inst[i].Comment);
   }
   free(inst);
}

/**
 * Copy an array of program instructions.
 * \param dest  pointer to destination.
 * \param src  pointer to source.
 * \param n  number of instructions to copy.
 * \return pointer to destination.
 */
struct prog_instruction *
_mesa_copy_instructions(struct prog_instruction *dest,
                        const struct prog_instruction *src, GLuint n)
{
   GLuint i;
   memcpy(dest, src, n * sizeof(struct prog_instruction));
   for (i = 0; i < n; i++) {
      if (src[i].Comment)
         dest[i].Comment = _mesa_strdup(src[i].Comment);
   }
   return dest;
}

/**
 * Allocate an array of program instructions.
 * \param numInst  number of instructions
 * \return pointer to instruction memory
 */
struct prog_instruction *
_mesa_alloc_instructions(GLuint numInst)
{
   return (struct prog_instruction *)
      calloc(1, numInst * sizeof(struct prog_instruction));
}

/**
 * Delete 'count' instructions at 'start' in the given program.
 * Adjust branch targets accordingly.
 */
GLboolean
_mesa_delete_instructions(struct gl_program *prog, GLuint start, GLuint count)
{
   const GLuint origLen = prog->NumInstructions;
   const GLuint newLen = origLen - count;
   struct prog_instruction *newInst;
   GLuint i;

   /* adjust branches */
   for (i = 0; i < prog->NumInstructions; i++) {
      struct prog_instruction *inst = prog->Instructions + i;
      if (inst->BranchTarget > 0) {
         if (inst->BranchTarget > (GLint) start) {
            inst->BranchTarget -= count;
         }
      }
   }

   /* Alloc storage for new instructions */
   newInst = _mesa_alloc_instructions(newLen);
   if (!newInst) {
      return GL_FALSE;
   }

   /* Copy 'start' instructions into new instruction buffer */
   _mesa_copy_instructions(newInst, prog->Instructions, start);

   /* Copy the remaining/tail instructions to new inst buffer */
   _mesa_copy_instructions(newInst + start,
                           prog->Instructions + start + count,
                           newLen - start);

   /* free old instructions */
   _mesa_free_instructions(prog->Instructions, origLen);

   /* install new instructions */
   prog->Instructions = newInst;
   prog->NumInstructions = newLen;

   return GL_TRUE;
}

/**
 * In 'prog' remove instruction[i] if removeFlags[i] == TRUE.
 * \return number of instructions removed
 */
static GLuint
remove_instructions(struct gl_program *prog, const GLboolean *removeFlags)
{
   GLint i, removeEnd = 0, removeCount = 0;
   GLuint totalRemoved = 0;

   /* go backward */
   for (i = prog->NumInstructions - 1; i >= 0; i--) {
      if (removeFlags[i]) {
         totalRemoved++;
         if (removeCount == 0) {
            /* begin a run of instructions to remove */
            removeEnd = i;
            removeCount = 1;
         }
         else {
            /* extend the run of instructions to remove */
            removeCount++;
         }
      }
      else {
         /* don't remove this instruction, but check if the preceeding
          * instructions are to be removed.
          */
         if (removeCount > 0) {
            GLint removeStart = removeEnd - removeCount + 1;
            _mesa_delete_instructions(prog, removeStart, removeCount);
            removeStart = removeCount = 0; /* reset removal info */
         }
      }
   }
   /* Finish removing if the first instruction was to be removed. */
   if (removeCount > 0) {
      GLint removeStart = removeEnd - removeCount + 1;
      _mesa_delete_instructions(prog, removeStart, removeCount);
   }
   return totalRemoved;
}


/**
 * Remap register indexes according to map.
 * \param prog  the program to search/replace
 * \param file  the type of register file to search/replace
 * \param map  maps old register indexes to new indexes
 */
static void
replace_regs(struct gl_program *prog, gl_register_file file, const GLint map[])
{
   GLuint i;

   for (i = 0; i < prog->NumInstructions; i++) {
      struct prog_instruction *inst = prog->Instructions + i;
      const GLuint numSrc = _mesa_num_inst_src_regs(inst->Opcode);
      GLuint j;
      for (j = 0; j < numSrc; j++) {
         if (inst->SrcReg[j].File == file) {
            GLuint index = inst->SrcReg[j].Index;
            ASSERT(map[index] >= 0);
            inst->SrcReg[j].Index = map[index];
         }
      }
      if (inst->DstReg.File == file) {
         const GLuint index = inst->DstReg.Index;
         ASSERT(map[index] >= 0);
         inst->DstReg.Index = map[index];
      }
   }
}

/**
 * Basic info about each instruction
 */
struct instruction_info
{
   gl_inst_opcode Opcode;
   const char *Name;
   GLuint NumSrcRegs;
   GLuint NumDstRegs;
};

/**
 * Instruction info
 * \note Opcode should equal array index!
 */
static const struct instruction_info InstInfo[MAX_OPCODE] = {
   { OPCODE_NOP,    "NOP",     0, 0 },
   { OPCODE_ABS,    "ABS",     1, 1 },
   { OPCODE_ADD,    "ADD",     2, 1 },
   { OPCODE_AND,    "AND",     2, 1 },
   { OPCODE_ARA,    "ARA",     1, 1 },
   { OPCODE_ARL,    "ARL",     1, 1 },
   { OPCODE_ARL_NV, "ARL_NV",  1, 1 },
   { OPCODE_ARR,    "ARL",     1, 1 },
   { OPCODE_BGNLOOP,"BGNLOOP", 0, 0 },
   { OPCODE_BGNSUB, "BGNSUB",  0, 0 },
   { OPCODE_BRA,    "BRA",     0, 0 },
   { OPCODE_BRK,    "BRK",     0, 0 },
   { OPCODE_CAL,    "CAL",     0, 0 },
   { OPCODE_CMP,    "CMP",     3, 1 },
   { OPCODE_CONT,   "CONT",    0, 0 },
   { OPCODE_COS,    "COS",     1, 1 },
   { OPCODE_DDX,    "DDX",     1, 1 },
   { OPCODE_DDY,    "DDY",     1, 1 },
   { OPCODE_DP2,    "DP2",     2, 1 },
   { OPCODE_DP2A,   "DP2A",    3, 1 },
   { OPCODE_DP3,    "DP3",     2, 1 },
   { OPCODE_DP4,    "DP4",     2, 1 },
   { OPCODE_DPH,    "DPH",     2, 1 },
   { OPCODE_DST,    "DST",     2, 1 },
   { OPCODE_ELSE,   "ELSE",    0, 0 },
   { OPCODE_EMIT_VERTEX,   "EMIT_VERTEX",    0, 0 },
   { OPCODE_END,    "END",     0, 0 },
   { OPCODE_END_PRIMITIVE,    "END_PRIMITIVE",     0, 0 },
   { OPCODE_ENDIF,  "ENDIF",   0, 0 },
   { OPCODE_ENDLOOP,"ENDLOOP", 0, 0 },
   { OPCODE_ENDSUB, "ENDSUB",  0, 0 },
   { OPCODE_EX2,    "EX2",     1, 1 },
   { OPCODE_EXP,    "EXP",     1, 1 },
   { OPCODE_FLR,    "FLR",     1, 1 },
   { OPCODE_FRC,    "FRC",     1, 1 },
   { OPCODE_IF,     "IF",      1, 0 },
   { OPCODE_KIL,    "KIL",     1, 0 },
   { OPCODE_KIL_NV, "KIL_NV",  0, 0 },
   { OPCODE_LG2,    "LG2",     1, 1 },
   { OPCODE_LIT,    "LIT",     1, 1 },
   { OPCODE_LOG,    "LOG",     1, 1 },
   { OPCODE_LRP,    "LRP",     3, 1 },
   { OPCODE_MAD,    "MAD",     3, 1 },
   { OPCODE_MAX,    "MAX",     2, 1 },
   { OPCODE_MIN,    "MIN",     2, 1 },
   { OPCODE_MOV,    "MOV",     1, 1 },
   { OPCODE_MUL,    "MUL",     2, 1 },
   { OPCODE_NOISE1, "NOISE1",  1, 1 },
   { OPCODE_NOISE2, "NOISE2",  1, 1 },
   { OPCODE_NOISE3, "NOISE3",  1, 1 },
   { OPCODE_NOISE4, "NOISE4",  1, 1 },
   { OPCODE_NOT,    "NOT",     1, 1 },
   { OPCODE_NRM3,   "NRM3",    1, 1 },
   { OPCODE_NRM4,   "NRM4",    1, 1 },
   { OPCODE_OR,     "OR",      2, 1 },
   { OPCODE_PK2H,   "PK2H",    1, 1 },
   { OPCODE_PK2US,  "PK2US",   1, 1 },
   { OPCODE_PK4B,   "PK4B",    1, 1 },
   { OPCODE_PK4UB,  "PK4UB",   1, 1 },
   { OPCODE_POW,    "POW",     2, 1 },
   { OPCODE_POPA,   "POPA",    0, 0 },
   { OPCODE_PRINT,  "PRINT",   1, 0 },
   { OPCODE_PUSHA,  "PUSHA",   0, 0 },
   { OPCODE_RCC,    "RCC",     1, 1 },
   { OPCODE_RCP,    "RCP",     1, 1 },
   { OPCODE_RET,    "RET",     0, 0 },
   { OPCODE_RFL,    "RFL",     1, 1 },
   { OPCODE_RSQ,    "RSQ",     1, 1 },
   { OPCODE_SCS,    "SCS",     1, 1 },
   { OPCODE_SEQ,    "SEQ",     2, 1 },
   { OPCODE_SFL,    "SFL",     0, 1 },
   { OPCODE_SGE,    "SGE",     2, 1 },
   { OPCODE_SGT,    "SGT",     2, 1 },
   { OPCODE_SIN,    "SIN",     1, 1 },
   { OPCODE_SLE,    "SLE",     2, 1 },
   { OPCODE_SLT,    "SLT",     2, 1 },
   { OPCODE_SNE,    "SNE",     2, 1 },
   { OPCODE_SSG,    "SSG",     1, 1 },
   { OPCODE_STR,    "STR",     0, 1 },
   { OPCODE_SUB,    "SUB",     2, 1 },
   { OPCODE_SWZ,    "SWZ",     1, 1 },
   { OPCODE_TEX,    "TEX",     1, 1 },
   { OPCODE_TXB,    "TXB",     1, 1 },
   { OPCODE_TXD,    "TXD",     3, 1 },
   { OPCODE_TXL,    "TXL",     1, 1 },
   { OPCODE_TXP,    "TXP",     1, 1 },
   { OPCODE_TXP_NV, "TXP_NV",  1, 1 },
   { OPCODE_TRUNC,  "TRUNC",   1, 1 },
   { OPCODE_UP2H,   "UP2H",    1, 1 },
   { OPCODE_UP2US,  "UP2US",   1, 1 },
   { OPCODE_UP4B,   "UP4B",    1, 1 },
   { OPCODE_UP4UB,  "UP4UB",   1, 1 },
   { OPCODE_X2D,    "X2D",     3, 1 },
   { OPCODE_XOR,    "XOR",     2, 1 },
   { OPCODE_XPD,    "XPD",     2, 1 }
};

/**
 * Return the number of dst registers for the given instruction/opcode.
 */
GLuint
_mesa_num_inst_dst_regs(gl_inst_opcode opcode)
{
   ASSERT(opcode < MAX_OPCODE);
   ASSERT(opcode == InstInfo[opcode].Opcode);
   ASSERT(OPCODE_XPD == InstInfo[OPCODE_XPD].Opcode);
   return InstInfo[opcode].NumDstRegs;
}

/**
 * Remove dead instructions from the given program.
 * This is very primitive for now.  Basically look for temp registers
 * that are written to but never read.  Remove any instructions that
 * write to such registers.  Be careful with condition code setters.
 */
static GLboolean
_mesa_remove_dead_code_global(struct gl_program *prog)
{
   GLboolean tempRead[REG_ALLOCATE_MAX_PROGRAM_TEMPS][4];
   GLboolean *removeInst; /* per-instruction removal flag */
   GLuint i, rem = 0, comp;

   memset(tempRead, 0, sizeof(tempRead));

   if (dbg) {
      printf("Optimize: Begin dead code removal\n");
      /*_mesa_print_program(prog);*/
   }

   removeInst = (GLboolean *)
      calloc(1, prog->NumInstructions * sizeof(GLboolean));

   /* Determine which temps are read and written */
   for (i = 0; i < prog->NumInstructions; i++) {
      const struct prog_instruction *inst = prog->Instructions + i;
      const GLuint numSrc = _mesa_num_inst_src_regs(inst->Opcode);
      GLuint j;

      /* check src regs */
      for (j = 0; j < numSrc; j++) {
         if (inst->SrcReg[j].File == PROGRAM_TEMPORARY) {
            const GLuint index = inst->SrcReg[j].Index;
            GLuint read_mask;
            ASSERT(index < REG_ALLOCATE_MAX_PROGRAM_TEMPS);
	    read_mask = get_src_arg_mask(inst, j, NO_MASK);

            if (inst->SrcReg[j].RelAddr) {
               if (dbg)
                  printf("abort remove dead code (indirect temp)\n");
               goto done;
            }

	    for (comp = 0; comp < 4; comp++) {
	       const GLuint swz = GET_SWZ(inst->SrcReg[j].Swizzle, comp);
	       ASSERT(swz < 4);
               if ((read_mask & (1 << swz)) == 0)
		  continue;
               if (swz <= SWIZZLE_W)
                  tempRead[index][swz] = GL_TRUE;
	    }
         }
      }

      /* check dst reg */
      if (inst->DstReg.File == PROGRAM_TEMPORARY) {
         const GLuint index = inst->DstReg.Index;
         ASSERT(index < REG_ALLOCATE_MAX_PROGRAM_TEMPS);

         if (inst->DstReg.RelAddr) {
            if (dbg)
               printf("abort remove dead code (indirect temp)\n");
            goto done;
         }

         if (inst->CondUpdate) {
            /* If we're writing to this register and setting condition
             * codes we cannot remove the instruction.  Prevent removal
             * by setting the 'read' flag.
             */
            tempRead[index][0] = GL_TRUE;
            tempRead[index][1] = GL_TRUE;
            tempRead[index][2] = GL_TRUE;
            tempRead[index][3] = GL_TRUE;
         }
      }
   }

   /* find instructions that write to dead registers, flag for removal */
   for (i = 0; i < prog->NumInstructions; i++) {
      struct prog_instruction *inst = prog->Instructions + i;
      const GLuint numDst = _mesa_num_inst_dst_regs(inst->Opcode);

      if (numDst != 0 && inst->DstReg.File == PROGRAM_TEMPORARY) {
         GLint chan, index = inst->DstReg.Index;

	 for (chan = 0; chan < 4; chan++) {
	    if (!tempRead[index][chan] &&
		inst->DstReg.WriteMask & (1 << chan)) {
	       if (dbg) {
		  printf("Remove writemask on %u.%c\n", i,
			       chan == 3 ? 'w' : 'x' + chan);
	       }
	       inst->DstReg.WriteMask &= ~(1 << chan);
	       rem++;
	    }
	 }

	 if (inst->DstReg.WriteMask == 0) {
	    /* If we cleared all writes, the instruction can be removed. */
	    if (dbg)
	       printf("Remove instruction %u: \n", i);
	    removeInst[i] = GL_TRUE;
	 }
      }
   }

   /* now remove the instructions which aren't needed */
   rem = remove_instructions(prog, removeInst);

   if (dbg) {
      printf("Optimize: End dead code removal.\n");
      printf("  %u channel writes removed\n", rem);
      printf("  %u instructions removed\n", rem);
      /*_mesa_print_program(prog);*/
   }

done:
   free(removeInst);
   return rem != 0;
}


enum inst_use
{
   READ,
   WRITE,
   FLOW,
   END
};


/**
 * Scan forward in program from 'start' for the next occurances of TEMP[index].
 * We look if an instruction reads the component given by the masks and if they
 * are overwritten.
 * Return READ, WRITE, FLOW or END to indicate the next usage or an indicator
 * that we can't look further.
 */
static enum inst_use
find_next_use(const struct gl_program *prog,
              GLuint start,
              GLuint index,
              GLuint mask)
{
   GLuint i;

   for (i = start; i < prog->NumInstructions; i++) {
      const struct prog_instruction *inst = prog->Instructions + i;
      switch (inst->Opcode) {
      case OPCODE_BGNLOOP:
      case OPCODE_BGNSUB:
      case OPCODE_BRA:
      case OPCODE_CAL:
      case OPCODE_CONT:
      case OPCODE_IF:
      case OPCODE_ELSE:
      case OPCODE_ENDIF:
      case OPCODE_ENDLOOP:
      case OPCODE_ENDSUB:
      case OPCODE_RET:
         return FLOW;
      case OPCODE_END:
         return END;
      default:
         {
            const GLuint numSrc = _mesa_num_inst_src_regs(inst->Opcode);
            GLuint j;
            for (j = 0; j < numSrc; j++) {
               if (inst->SrcReg[j].RelAddr ||
                   (inst->SrcReg[j].File == PROGRAM_TEMPORARY &&
                   inst->SrcReg[j].Index == index &&
                   (get_src_arg_mask(inst,j,NO_MASK) & mask)))
                  return READ;
            }
            if (_mesa_num_inst_dst_regs(inst->Opcode) == 1 &&
                inst->DstReg.File == PROGRAM_TEMPORARY &&
                inst->DstReg.Index == index) {
               mask &= ~inst->DstReg.WriteMask;
               if (mask == 0)
                  return WRITE;
            }
         }
      }
   }
   return END;
}


/**
 * Is the given instruction opcode a flow-control opcode?
 * XXX maybe move this into prog_instruction.[ch]
 */
static GLboolean
_mesa_is_flow_control_opcode(enum prog_opcode opcode)
{
   switch (opcode) {
   case OPCODE_BGNLOOP:
   case OPCODE_BGNSUB:
   case OPCODE_BRA:
   case OPCODE_CAL:
   case OPCODE_CONT:
   case OPCODE_IF:
   case OPCODE_ELSE:
   case OPCODE_END:
   case OPCODE_ENDIF:
   case OPCODE_ENDLOOP:
   case OPCODE_ENDSUB:
   case OPCODE_RET:
      return GL_TRUE;
   default:
      return GL_FALSE;
   }
}


/**
 * Test if the given instruction is a simple MOV (no conditional updating,
 * not relative addressing, no negation/abs, etc).
 */
static GLboolean
can_downward_mov_be_modifed(const struct prog_instruction *mov)
{
   return
      mov->Opcode == OPCODE_MOV &&
      mov->CondUpdate == GL_FALSE &&
      mov->SrcReg[0].RelAddr == 0 &&
      mov->SrcReg[0].Negate == 0 &&
      mov->SrcReg[0].Abs == 0 &&
      mov->SrcReg[0].HasIndex2 == 0 &&
      mov->SrcReg[0].RelAddr2 == 0 &&
      mov->DstReg.RelAddr == 0 &&
      mov->DstReg.CondMask == COND_TR &&
      mov->SaturateMode == SATURATE_OFF;
}


static GLboolean
can_upward_mov_be_modifed(const struct prog_instruction *mov)
{
   return
      can_downward_mov_be_modifed(mov) &&
      mov->DstReg.File == PROGRAM_TEMPORARY;
}


/**
 * Try to remove use of extraneous MOV instructions, to free them up for dead
 * code removal.
 */
static void
_mesa_remove_extra_move_use(struct gl_program *prog)
{
   GLuint i, j;

   if (dbg) {
      printf("Optimize: Begin remove extra move use\n");
      _mesa_print_program(prog);
   }

   /*
    * Look for sequences such as this:
    *    MOV tmpX, arg0;
    *    ...
    *    FOO tmpY, tmpX, arg1;
    * and convert into:
    *    MOV tmpX, arg0;
    *    ...
    *    FOO tmpY, arg0, arg1;
    */

   for (i = 0; i + 1 < prog->NumInstructions; i++) {
      const struct prog_instruction *mov = prog->Instructions + i;
      GLuint dst_mask, src_mask;
      if (can_upward_mov_be_modifed(mov) == GL_FALSE)
         continue;

      /* Scanning the code, we maintain the components which are still active in
       * these two masks
       */
      dst_mask = mov->DstReg.WriteMask;
      src_mask = get_src_arg_mask(mov, 0, NO_MASK);

      /* Walk through remaining instructions until the or src reg gets
       * rewritten or we get into some flow-control, eliminating the use of
       * this MOV.
       */
      for (j = i + 1; j < prog->NumInstructions; j++) {
	 struct prog_instruction *inst2 = prog->Instructions + j;
         GLuint arg;

	 if (_mesa_is_flow_control_opcode(inst2->Opcode))
	     break;

	 /* First rewrite this instruction's args if appropriate. */
	 for (arg = 0; arg < _mesa_num_inst_src_regs(inst2->Opcode); arg++) {
	    GLuint comp, read_mask;

	    if (inst2->SrcReg[arg].File != mov->DstReg.File ||
		inst2->SrcReg[arg].Index != mov->DstReg.Index ||
		inst2->SrcReg[arg].RelAddr ||
		inst2->SrcReg[arg].Abs)
	       continue;
            read_mask = get_src_arg_mask(inst2, arg, NO_MASK);

	    /* Adjust the swizzles of inst2 to point at MOV's source if ALL the
             * components read still come from the mov instructions
             */
            if (is_swizzle_regular(inst2->SrcReg[arg].Swizzle) &&
               (read_mask & dst_mask) == read_mask) {
               for (comp = 0; comp < 4; comp++) {
                  const GLuint inst2_swz =
                     GET_SWZ(inst2->SrcReg[arg].Swizzle, comp);
                  const GLuint s = GET_SWZ(mov->SrcReg[0].Swizzle, inst2_swz);
                  inst2->SrcReg[arg].Swizzle &= ~(7 << (3 * comp));
                  inst2->SrcReg[arg].Swizzle |= s << (3 * comp);
                  inst2->SrcReg[arg].Negate ^= (((mov->SrcReg[0].Negate >>
                                                  inst2_swz) & 0x1) << comp);
               }
               inst2->SrcReg[arg].File = mov->SrcReg[0].File;
               inst2->SrcReg[arg].Index = mov->SrcReg[0].Index;
            }
	 }

	 /* The source of MOV is written. This potentially deactivates some
          * components from the src and dst of the MOV instruction
          */
	 if (inst2->DstReg.File == mov->DstReg.File &&
	     (inst2->DstReg.RelAddr ||
	      inst2->DstReg.Index == mov->DstReg.Index)) {
            dst_mask &= ~inst2->DstReg.WriteMask;
            src_mask = get_src_arg_mask(mov, 0, dst_mask);
         }

         /* Idem when the destination of mov is written */
	 if (inst2->DstReg.File == mov->SrcReg[0].File &&
	     (inst2->DstReg.RelAddr ||
	      inst2->DstReg.Index == mov->SrcReg[0].Index)) {
            src_mask &= ~inst2->DstReg.WriteMask;
            dst_mask &= get_dst_mask_for_mov(mov, src_mask);
         }
         if (dst_mask == 0)
            break;
      }
   }

   if (dbg) {
      printf("Optimize: End remove extra move use.\n");
      /*_mesa_print_program(prog);*/
   }
}


/**
 * Complements dead_code_global. Try to remove code in block of code by
 * carefully monitoring the swizzles. Both functions should be merged into one
 * with a proper control flow graph
 */
static GLboolean
_mesa_remove_dead_code_local(struct gl_program *prog)
{
   GLboolean *removeInst;
   GLuint i, arg, rem = 0;

   removeInst = (GLboolean *)
      calloc(1, prog->NumInstructions * sizeof(GLboolean));

   for (i = 0; i < prog->NumInstructions; i++) {
      const struct prog_instruction *inst = prog->Instructions + i;
      const GLuint index = inst->DstReg.Index;
      const GLuint mask = inst->DstReg.WriteMask;
      enum inst_use use;

      /* We must deactivate the pass as soon as some indirection is used */
      if (inst->DstReg.RelAddr)
         goto done;
      for (arg = 0; arg < _mesa_num_inst_src_regs(inst->Opcode); arg++)
         if (inst->SrcReg[arg].RelAddr)
            goto done;

      if (_mesa_is_flow_control_opcode(inst->Opcode) ||
          _mesa_num_inst_dst_regs(inst->Opcode) == 0 ||
          inst->DstReg.File != PROGRAM_TEMPORARY ||
          inst->DstReg.RelAddr)
         continue;

      use = find_next_use(prog, i+1, index, mask);
      if (use == WRITE || use == END)
         removeInst[i] = GL_TRUE;
   }

   rem = remove_instructions(prog, removeInst);

done:
   free(removeInst);
   return rem != 0;
}


/**
 * Try to inject the destination of mov as the destination of inst and recompute
 * the swizzles operators for the sources of inst if required. Return GL_TRUE
 * of the substitution was possible, GL_FALSE otherwise
 */
static GLboolean
_mesa_merge_mov_into_inst(struct prog_instruction *inst,
                          const struct prog_instruction *mov)
{
   /* Indirection table which associates destination and source components for
    * the mov instruction
    */
   const GLuint mask = get_src_arg_mask(mov, 0, NO_MASK);

   /* Some components are not written by inst. We cannot remove the mov */
   if (mask != (inst->DstReg.WriteMask & mask))
      return GL_FALSE;

   /* Depending on the instruction, we may need to recompute the swizzles.
    * Also, some other instructions (like TEX) are not linear. We will only
    * consider completely active sources and destinations
    */
   switch (inst->Opcode) {

   /* Carstesian instructions: we compute the swizzle */
   case OPCODE_MOV:
   case OPCODE_MIN:
   case OPCODE_MAX:
   case OPCODE_ABS:
   case OPCODE_ADD:
   case OPCODE_MAD:
   case OPCODE_MUL:
   case OPCODE_SUB:
   {
      GLuint dst_to_src_comp[4] = {0,0,0,0};
      GLuint dst_comp, arg;
      for (dst_comp = 0; dst_comp < 4; ++dst_comp) {
         if (mov->DstReg.WriteMask & (1 << dst_comp)) {
            const GLuint src_comp = GET_SWZ(mov->SrcReg[0].Swizzle, dst_comp);
            ASSERT(src_comp < 4);
            dst_to_src_comp[dst_comp] = src_comp;
         }
      }

      /* Patch each source of the instruction */
      for (arg = 0; arg < _mesa_num_inst_src_regs(inst->Opcode); arg++) {
         const GLuint arg_swz = inst->SrcReg[arg].Swizzle;
         inst->SrcReg[arg].Swizzle = 0;

         /* Reset each active component of the swizzle */
         for (dst_comp = 0; dst_comp < 4; ++dst_comp) {
            GLuint src_comp, arg_comp;
            if ((mov->DstReg.WriteMask & (1 << dst_comp)) == 0)
               continue;
            src_comp = dst_to_src_comp[dst_comp];
            ASSERT(src_comp < 4);
            arg_comp = GET_SWZ(arg_swz, src_comp);
            ASSERT(arg_comp < 4);
            inst->SrcReg[arg].Swizzle |= arg_comp << (3*dst_comp);
         }
      }
      inst->DstReg = mov->DstReg;
      return GL_TRUE;
   }

   /* Dot products and scalar instructions: we only change the destination */
   case OPCODE_RCP:
   case OPCODE_SIN:
   case OPCODE_COS:
   case OPCODE_RSQ:
   case OPCODE_POW:
   case OPCODE_EX2:
   case OPCODE_LOG:
   case OPCODE_DP2:
   case OPCODE_DP3:
   case OPCODE_DP4:
      inst->DstReg = mov->DstReg;
      return GL_TRUE;

   /* All other instructions require fully active components with no swizzle */
   default:
      if (mov->SrcReg[0].Swizzle != SWIZZLE_XYZW ||
          inst->DstReg.WriteMask != WRITEMASK_XYZW)
         return GL_FALSE;
      inst->DstReg = mov->DstReg;
      return GL_TRUE;
   }
}


/**
 * Try to remove extraneous MOV instructions from the given program.
 */
static GLboolean
_mesa_remove_extra_moves(struct gl_program *prog)
{
   GLboolean *removeInst; /* per-instruction removal flag */
   GLuint i, rem = 0, nesting = 0;

   if (dbg) {
      printf("Optimize: Begin remove extra moves\n");
      _mesa_print_program(prog);
   }

   removeInst = (GLboolean *)
      calloc(1, prog->NumInstructions * sizeof(GLboolean));

   /*
    * Look for sequences such as this:
    *    FOO tmpX, arg0, arg1;
    *    MOV tmpY, tmpX;
    * and convert into:
    *    FOO tmpY, arg0, arg1;
    */

   for (i = 0; i < prog->NumInstructions; i++) {
      const struct prog_instruction *mov = prog->Instructions + i;

      switch (mov->Opcode) {
      case OPCODE_BGNLOOP:
      case OPCODE_BGNSUB:
      case OPCODE_IF:
         nesting++;
         break;
      case OPCODE_ENDLOOP:
      case OPCODE_ENDSUB:
      case OPCODE_ENDIF:
         nesting--;
         break;
      case OPCODE_MOV:
         if (i > 0 &&
             can_downward_mov_be_modifed(mov) &&
             mov->SrcReg[0].File == PROGRAM_TEMPORARY &&
             nesting == 0)
         {

            /* see if this MOV can be removed */
            const GLuint id = mov->SrcReg[0].Index;
            struct prog_instruction *prevInst;
            GLuint prevI;

            /* get pointer to previous instruction */
            prevI = i - 1;
            while (prevI > 0 && removeInst[prevI])
               prevI--;
            prevInst = prog->Instructions + prevI;

            if (prevInst->DstReg.File == PROGRAM_TEMPORARY &&
                prevInst->DstReg.Index == id &&
                prevInst->DstReg.RelAddr == 0 &&
                prevInst->DstReg.CondSrc == 0 && 
                prevInst->DstReg.CondMask == COND_TR) {

               const GLuint dst_mask = prevInst->DstReg.WriteMask;
               enum inst_use next_use = find_next_use(prog, i+1, id, dst_mask);

               if (next_use == WRITE || next_use == END) {
                  /* OK, we can safely remove this MOV instruction.
                   * Transform:
                   *   prevI: FOO tempIndex, x, y;
                   *       i: MOV z, tempIndex;
                   * Into:
                   *   prevI: FOO z, x, y;
                   */
                  if (_mesa_merge_mov_into_inst(prevInst, mov)) {
                     removeInst[i] = GL_TRUE;
                     if (dbg) {
                        printf("Remove MOV at %u\n", i);
                        printf("new prev inst %u: ", prevI);
                        _mesa_print_instruction(prevInst);
                     }
                  }
               }
            }
         }
         break;
      default:
         ; /* nothing */
      }
   }

   /* now remove the instructions which aren't needed */
   rem = remove_instructions(prog, removeInst);

   free(removeInst);

   if (dbg) {
      printf("Optimize: End remove extra moves.  %u instructions removed\n", rem);
      /*_mesa_print_program(prog);*/
   }

   return rem != 0;
}


/** A live register interval */
struct interval
{
   GLuint Reg;         /** The temporary register index */
   GLuint Start, End;  /** Start/end instruction numbers */
};


/** A list of register intervals */
struct interval_list
{
   GLuint Num;
   struct interval Intervals[REG_ALLOCATE_MAX_PROGRAM_TEMPS];
};


static void
append_interval(struct interval_list *list, const struct interval *inv)
{
   list->Intervals[list->Num++] = *inv;
}


/** Insert interval inv into list, sorted by interval end */
static void
insert_interval_by_end(struct interval_list *list, const struct interval *inv)
{
   /* XXX we could do a binary search insertion here since list is sorted */
   GLint i = list->Num - 1;
   while (i >= 0 && list->Intervals[i].End > inv->End) {
      list->Intervals[i + 1] = list->Intervals[i];
      i--;
   }
   list->Intervals[i + 1] = *inv;
   list->Num++;

#ifdef DEBUG
   {
      GLuint i;
      for (i = 0; i + 1 < list->Num; i++) {
         ASSERT(list->Intervals[i].End <= list->Intervals[i + 1].End);
      }
   }
#endif
}


/** Remove the given interval from the interval list */
static void
remove_interval(struct interval_list *list, const struct interval *inv)
{
   /* XXX we could binary search since list is sorted */
   GLuint k;
   for (k = 0; k < list->Num; k++) {
      if (list->Intervals[k].Reg == inv->Reg) {
         /* found, remove it */
         ASSERT(list->Intervals[k].Start == inv->Start);
         ASSERT(list->Intervals[k].End == inv->End);
         while (k < list->Num - 1) {
            list->Intervals[k] = list->Intervals[k + 1];
            k++;
         }
         list->Num--;
         return;
      }
   }
}


/** called by qsort() */
static int
compare_start(const void *a, const void *b)
{
   const struct interval *ia = (const struct interval *) a;
   const struct interval *ib = (const struct interval *) b;
   if (ia->Start < ib->Start)
      return -1;
   else if (ia->Start > ib->Start)
      return +1;
   else
      return 0;
}


/** sort the interval list according to interval starts */
static void
sort_interval_list_by_start(struct interval_list *list)
{
   qsort(list->Intervals, list->Num, sizeof(struct interval), compare_start);
#ifdef DEBUG
   {
      GLuint i;
      for (i = 0; i + 1 < list->Num; i++) {
         ASSERT(list->Intervals[i].Start <= list->Intervals[i + 1].Start);
      }
   }
#endif
}

struct loop_info
{
   GLuint Start, End;  /**< Start, end instructions of loop */
};

/**
 * Update the intermediate interval info for register 'index' and
 * instruction 'ic'.
 */
static void
update_interval(GLint intBegin[], GLint intEnd[],
		struct loop_info *loopStack, GLuint loopStackDepth,
		GLuint index, GLuint ic)
{
   int i;

   /* If the register is used in a loop, extend its lifetime through the end
    * of the outermost loop that doesn't contain its definition.
    */
   for (i = 0; i < loopStackDepth; i++) {
      if (intBegin[index] < loopStack[i].Start) {
	 ic = loopStack[i].End;
	 break;
      }
   }

   ASSERT(index < REG_ALLOCATE_MAX_PROGRAM_TEMPS);
   if (intBegin[index] == -1) {
      ASSERT(intEnd[index] == -1);
      intBegin[index] = intEnd[index] = ic;
   }
   else {
      intEnd[index] = ic;
   }
}


/**
 * Find first/last instruction that references each temporary register.
 */
GLboolean
_mesa_find_temp_intervals(const struct prog_instruction *instructions,
                          GLuint numInstructions,
                          GLint intBegin[REG_ALLOCATE_MAX_PROGRAM_TEMPS],
                          GLint intEnd[REG_ALLOCATE_MAX_PROGRAM_TEMPS])
{
   struct loop_info loopStack[MAX_LOOP_NESTING];
   GLuint loopStackDepth = 0;
   GLuint i;

   for (i = 0; i < REG_ALLOCATE_MAX_PROGRAM_TEMPS; i++){
      intBegin[i] = intEnd[i] = -1;
   }

   /* Scan instructions looking for temporary registers */
   for (i = 0; i < numInstructions; i++) {
      const struct prog_instruction *inst = instructions + i;
      if (inst->Opcode == OPCODE_BGNLOOP) {
         loopStack[loopStackDepth].Start = i;
         loopStack[loopStackDepth].End = inst->BranchTarget;
         loopStackDepth++;
      }
      else if (inst->Opcode == OPCODE_ENDLOOP) {
         loopStackDepth--;
      }
      else if (inst->Opcode == OPCODE_CAL) {
         return GL_FALSE;
      }
      else {
         const GLuint numSrc = 3;/*_mesa_num_inst_src_regs(inst->Opcode);*/
         GLuint j;
         for (j = 0; j < numSrc; j++) {
            if (inst->SrcReg[j].File == PROGRAM_TEMPORARY) {
               const GLuint index = inst->SrcReg[j].Index;
               if (inst->SrcReg[j].RelAddr)
                  return GL_FALSE;
               update_interval(intBegin, intEnd, loopStack, loopStackDepth,
			       index, i);
            }
         }
         if (inst->DstReg.File == PROGRAM_TEMPORARY) {
            const GLuint index = inst->DstReg.Index;
            if (inst->DstReg.RelAddr)
               return GL_FALSE;
            update_interval(intBegin, intEnd, loopStack, loopStackDepth,
			    index, i);
         }
      }
   }

   return GL_TRUE;
}


/**
 * Find the live intervals for each temporary register in the program.
 * For register R, the interval [A,B] indicates that R is referenced
 * from instruction A through instruction B.
 * Special consideration is needed for loops and subroutines.
 * \return GL_TRUE if success, GL_FALSE if we cannot proceed for some reason
 */
static GLboolean
find_live_intervals(struct gl_program *prog,
                    struct interval_list *liveIntervals)
{
   GLint intBegin[REG_ALLOCATE_MAX_PROGRAM_TEMPS];
   GLint intEnd[REG_ALLOCATE_MAX_PROGRAM_TEMPS];
   GLuint i;

   /*
    * Note: we'll return GL_FALSE below if we find relative indexing
    * into the TEMP register file.  We can't handle that yet.
    * We also give up on subroutines for now.
    */

   if (dbg) {
      printf("Optimize: Begin find intervals\n");
   }

   /* build intermediate arrays */
   if (!_mesa_find_temp_intervals(prog->Instructions, prog->NumInstructions,
                                  intBegin, intEnd))
      return GL_FALSE;

   /* Build live intervals list from intermediate arrays */
   liveIntervals->Num = 0;
   for (i = 0; i < REG_ALLOCATE_MAX_PROGRAM_TEMPS; i++) {
      if (intBegin[i] >= 0) {
         struct interval inv;
         inv.Reg = i;
         inv.Start = intBegin[i];
         inv.End = intEnd[i];
         append_interval(liveIntervals, &inv);
      }
   }

   /* Sort the list according to interval starts */
   sort_interval_list_by_start(liveIntervals);

   if (dbg) {
      /* print interval info */
      for (i = 0; i < liveIntervals->Num; i++) {
         const struct interval *inv = liveIntervals->Intervals + i;
         printf("Reg[%d] live [%d, %d]:",
                      inv->Reg, inv->Start, inv->End);
         if (1) {
            GLuint j;
            for (j = 0; j < inv->Start; j++)
               printf(" ");
            for (j = inv->Start; j <= inv->End; j++)
               printf("x");
         }
         printf("\n");
      }
   }

   return GL_TRUE;
}


/** Scan the array of used register flags to find free entry */
static GLint
alloc_register(GLboolean usedRegs[REG_ALLOCATE_MAX_PROGRAM_TEMPS])
{
   GLuint k;
   for (k = 0; k < REG_ALLOCATE_MAX_PROGRAM_TEMPS; k++) {
      if (!usedRegs[k]) {
         usedRegs[k] = GL_TRUE;
         return k;
      }
   }
   return -1;
}


/**
 * This function implements "Linear Scan Register Allocation" to reduce
 * the number of temporary registers used by the program.
 *
 * We compute the "live interval" for all temporary registers then
 * examine the overlap of the intervals to allocate new registers.
 * Basically, if two intervals do not overlap, they can use the same register.
 */
static void
_mesa_reallocate_registers(struct gl_program *prog)
{
   struct interval_list liveIntervals;
   GLint registerMap[REG_ALLOCATE_MAX_PROGRAM_TEMPS];
   GLboolean usedRegs[REG_ALLOCATE_MAX_PROGRAM_TEMPS];
   GLuint i;
   GLint maxTemp = -1;

   if (dbg) {
      printf("Optimize: Begin live-interval register reallocation\n");
      _mesa_print_program(prog);
   }

   for (i = 0; i < REG_ALLOCATE_MAX_PROGRAM_TEMPS; i++){
      registerMap[i] = -1;
      usedRegs[i] = GL_FALSE;
   }

   if (!find_live_intervals(prog, &liveIntervals)) {
      if (dbg)
         printf("Aborting register reallocation\n");
      return;
   }

   {
      struct interval_list activeIntervals;
      activeIntervals.Num = 0;

      /* loop over live intervals, allocating a new register for each */
      for (i = 0; i < liveIntervals.Num; i++) {
         const struct interval *live = liveIntervals.Intervals + i;

         if (dbg)
            printf("Consider register %u\n", live->Reg);

         /* Expire old intervals.  Intervals which have ended with respect
          * to the live interval can have their remapped registers freed.
          */
         {
            GLint j;
            for (j = 0; j < (GLint) activeIntervals.Num; j++) {
               const struct interval *inv = activeIntervals.Intervals + j;
               if (inv->End >= live->Start) {
                  /* Stop now.  Since the activeInterval list is sorted
                   * we know we don't have to go further.
                   */
                  break;
               }
               else {
                  /* Interval 'inv' has expired */
                  const GLint regNew = registerMap[inv->Reg];
                  ASSERT(regNew >= 0);

                  if (dbg)
                     printf("  expire interval for reg %u\n", inv->Reg);

                  /* remove interval j from active list */
                  remove_interval(&activeIntervals, inv);
                  j--;  /* counter-act j++ in for-loop above */

                  /* return register regNew to the free pool */
                  if (dbg)
                     printf("  free reg %d\n", regNew);
                  ASSERT(usedRegs[regNew] == GL_TRUE);
                  usedRegs[regNew] = GL_FALSE;
               }
            }
         }

         /* find a free register for this live interval */
         {
            const GLint k = alloc_register(usedRegs);
            if (k < 0) {
               /* out of registers, give up */
               return;
            }
            registerMap[live->Reg] = k;
            maxTemp = MAX2(maxTemp, k);
            if (dbg)
               printf("  remap register %u -> %d\n", live->Reg, k);
         }

         /* Insert this live interval into the active list which is sorted
          * by increasing end points.
          */
         insert_interval_by_end(&activeIntervals, live);
      }
   }

   if (maxTemp + 1 < (GLint) liveIntervals.Num) {
      /* OK, we've reduced the number of registers needed.
       * Scan the program and replace all the old temporary register
       * indexes with the new indexes.
       */
      replace_regs(prog, PROGRAM_TEMPORARY, registerMap);

      prog->NumTemporaries = maxTemp + 1;
   }

   if (dbg) {
      printf("Optimize: End live-interval register reallocation\n");
      printf("Num temp regs before: %u  after: %u\n",
                   liveIntervals.Num, maxTemp + 1);
      _mesa_print_program(prog);
   }
}


#if 0
static void
print_it(struct gl_context *ctx, struct gl_program *program, const char *txt) {
   fprintf(stderr, "%s (%u inst):\n", txt, program->NumInstructions);
   _mesa_print_program(program);
   _mesa_print_program_parameters(ctx, program);
   fprintf(stderr, "\n\n");
}
#endif


/**
 * Apply optimizations to the given program to eliminate unnecessary
 * instructions, temp regs, etc.
 */
void
_mesa_optimize_program(struct gl_context *ctx, struct gl_program *program)
{
   GLboolean any_change;

   /* Stop when no modifications were output */
   do {
      any_change = GL_FALSE;
      _mesa_remove_extra_move_use(program);
      if (_mesa_remove_dead_code_global(program))
         any_change = GL_TRUE;
      if (_mesa_remove_extra_moves(program))
         any_change = GL_TRUE;
      if (_mesa_remove_dead_code_local(program))
         any_change = GL_TRUE;
      _mesa_reallocate_registers(program);
   } while (any_change);
}


} /* extern "C" */
