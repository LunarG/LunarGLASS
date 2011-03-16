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
	symbol_table.c \
	sampler.cpp
mesa_SOURCES := $(addprefix ../mesa/program/, $(mesa_SOURCES))

passes_SOURCES := \
    $(shell find ../LunarGLASS/Passes -type f -name "*.cpp")
lunarglass_SOURCES := \
	$(wildcard ../LunarGLASS/*.cpp) \
	$(passes_SOURCES)

glsl_SOURCES := \
	strtod.c \
	ralloc.c \
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
	ir_basic_block.cpp \
	ir_clone.cpp \
	ir_constant_expression.cpp \
	ir.cpp \
	ir_expression_flattening.cpp \
	ir_function_can_inline.cpp \
	ir_function.cpp \
	ir_hierarchical_visitor.cpp \
	ir_hv_accept.cpp \
	ir_import_prototypes.cpp \
	ir_print_visitor.cpp \
	ir_reader.cpp \
	ir_rvalue_visitor.cpp \
	ir_set_program_inouts.cpp \
	ir_validate.cpp \
	ir_variable.cpp \
	ir_variable_refcount.cpp \
	linker.cpp \
	link_functions.cpp \
	loop_analysis.cpp \
	loop_controls.cpp \
	loop_unroll.cpp \
	lower_discard.cpp \
	lower_if_to_cond_assign.cpp \
	lower_instructions.cpp \
	lower_jumps.cpp \
	lower_mat_op_to_vec.cpp \
	lower_noise.cpp \
	lower_texture_projection.cpp \
	lower_variable_index_to_cond_assign.cpp \
	lower_vec_index_to_cond_assign.cpp \
	lower_vec_index_to_swizzle.cpp \
	lower_vector.cpp \
	main.cpp \
	opt_algebraic.cpp \
	opt_constant_folding.cpp \
	opt_constant_propagation.cpp \
	opt_constant_variable.cpp \
	opt_copy_propagation.cpp \
	opt_dead_code.cpp \
	opt_dead_code_local.cpp \
	opt_dead_functions.cpp \
	opt_discard_simplification.cpp \
	opt_function_inlining.cpp \
	opt_if_simplification.cpp \
	opt_noop_swizzle.cpp \
	opt_redundant_jumps.cpp \
	opt_structure_splitting.cpp \
	opt_swizzle_swizzle.cpp \
	opt_tree_grafting.cpp \
	s_expression.cpp

StandAlone_SOURCES := \
	$(mesa_SOURCES) \
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
	-I$(TOP)/src/mesa \
	-I$(TOP)/src/mesa/main \
	-I$(TOP)/src/mesa/program \
	-I$(TOP)/src/LunarGLASS

StandAlone_CFLAGS := \
	$(shell $(LLVM)/bin/llvm-config --cflags) \
	-Wall -g -O0

StandAlone_CXXFLAGS := \
	$(shell $(LLVM)/bin/llvm-config --cxxflags) \
	-Wall -U NDEBUG -g -O0 -Wno-overloaded-virtual \
	-Wno-sign-compare -Wno-switch -Wno-cast-qual \
	-D GLA_REVISION=\"$(shell svnversion)\"

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
