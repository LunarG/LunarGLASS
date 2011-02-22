# Makefile for Linux

TOP := ../..
LLVM := $(TOP)/../LLVM/llvm-2.8/build/install/usr/local

ifeq ($(wildcard $(LLVM)/bin/llvm-config),)
$(error Invalid path to LLVM: $(LLVM))
endif

mesa_SOURCES := \
	hash_table.c \
	ir_to_mesa.cpp \
	prog_print.c \
	symbol_table.c
mesa_SOURCES := $(addprefix ../mesa/program/, $(mesa_SOURCES))

talloc_SOURCES := \
	talloc.c
talloc_SOURCES := $(addprefix ../talloc/, $(talloc_SOURCES))

lunarglass_SOURCES := \
	$(shell echo ../LunarGLASS/*.cpp)

glsl_SOURCES := \
	glcpp/glcpp-lex.c \
	glcpp/glcpp-parse.c \
	glcpp/pp.c \
	ast_expr.cpp \
	ast_function.cpp \
	ast_to_hir.cpp \
	ast_type.cpp \
	builtin_function.cpp \
	glsl_lexer.cpp \
	glsl_parser.cpp \
	glsl_parser_extras.cpp \
	glsl_symbol_table.cpp \
	glsl_types.cpp \
	hir_field_selection.cpp \
	ir_algebraic.cpp \
	ir_basic_block.cpp \
	ir_clone.cpp \
	ir_constant_expression.cpp \
	ir_constant_folding.cpp \
	ir_constant_propagation.cpp \
	ir_constant_variable.cpp \
	ir_copy_propagation.cpp \
	ir.cpp \
	ir_dead_code.cpp \
	ir_dead_code_local.cpp \
	ir_dead_functions.cpp \
	ir_div_to_mul_rcp.cpp \
	ir_explog_to_explog2.cpp \
	ir_expression_flattening.cpp \
	ir_function_can_inline.cpp \
	ir_function.cpp \
	ir_function_inlining.cpp \
	ir_hierarchical_visitor.cpp \
	ir_hv_accept.cpp \
	ir_if_simplification.cpp \
	ir_if_to_cond_assign.cpp \
	ir_import_prototypes.cpp \
	ir_lower_jumps.cpp \
	ir_mat_op_to_vec.cpp \
	ir_mod_to_fract.cpp \
	ir_noop_swizzle.cpp \
	ir_print_visitor.cpp \
	ir_reader.cpp \
	ir_rvalue_visitor.cpp \
	ir_set_program_inouts.cpp \
	ir_structure_splitting.cpp \
	ir_sub_to_add_neg.cpp \
	ir_swizzle_swizzle.cpp \
	ir_tree_grafting.cpp \
	ir_validate.cpp \
	ir_variable.cpp \
	ir_variable_refcount.cpp \
	ir_vec_index_to_cond_assign.cpp \
	ir_vec_index_to_swizzle.cpp \
	linker.cpp \
	link_functions.cpp \
	loop_analysis.cpp \
	loop_controls.cpp \
	loop_unroll.cpp \
	lower_noise.cpp \
	lower_variable_index_to_cond_assign.cpp \
	main.cpp \
	opt_redundant_jumps.cpp \
	s_expression.cpp

StandAlone_SOURCES := \
	$(mesa_SOURCES) \
	$(talloc_SOURCES) \
	$(lunarglass_SOURCES) \
	$(glsl_SOURCES)

StandAlone_C_SOURCES := $(filter %.c, $(StandAlone_SOURCES))
StandAlone_CXX_SOURCES := $(filter %.cpp, $(StandAlone_SOURCES))
StandAlone_OBJECTS := \
	$(StandAlone_C_SOURCES:.c=.o) \
	$(StandAlone_CXX_SOURCES:.cpp=.o)

StandAlone_CPPFLAGS := \
	-I$(TOP)/include \
	-I$(TOP)/src/glsl \
	-I$(TOP)/src/mapi \
	-I$(TOP)/src/talloc \
	-I$(TOP)/src/mesa \
	-I$(TOP)/src/mesa/main \
	-I$(TOP)/src/mesa/program \
	-I$(TOP)/src/LunarGLASS

StandAlone_CFLAGS := \
	$(shell $(LLVM)/bin/llvm-config --cflags) \
	-Wall -g -O0

StandAlone_CXXFLAGS := \
	$(shell $(LLVM)/bin/llvm-config --cxxflags) \
	-Wall -g -O0 -Wno-overloaded-virtual \
	-Wno-sign-compare -Wno-switch -Wno-cast-qual

StandAlone_LDFLAGS := \
	$(shell $(LLVM)/bin/llvm-config --ldflags)

StandAlone_LIBS := \
	$(shell $(LLVM)/bin/llvm-config --libs)

all: StandAlone

StandAlone: $(StandAlone_OBJECTS)
	$(CXX) -o $@ $(StandAlone_LDFLAGS) $(LDFLAGS) \
		$(StandAlone_OBJECTS) $(StandAlone_LIBS)

.c.o:
	$(CC) -c -o $@ $(StandAlone_CPPFLAGS) $(StandAlone_CFLAGS) \
		-Dtrue=1 -Dfalse=0 -Dbool=char $(CPPFLAGS) $(CFLAGS) $<

.cpp.o:
	$(CXX) -c -o $@ $(StandAlone_CPPFLAGS) $(StandAlone_CXXFLAGS) \
		$(CPPFLAGS) $(CXXFLAGS) $<

clean:
	rm -f StandAlone
	rm -f $(StandAlone_OBJECTS)
	rm -f depend

depend: $(StandAlone_SOURCES)
	rm -f depend
	touch depend
	makedepend -fdepend $(StandAlone_CPPFLAGS) $(StandAlone_SOURCES) \
		2> /dev/null

-include depend
