# Makefile for Linux

CORE := ../Core
GLSLANG := ../../glslang/
LLVM := $(CORE)/LLVM/llvm-3.2.src/
LLVMCONFIG := $(LLVM)build/Release+Asserts/bin/llvm-config

glslang_INCLUDES := \
	-I$(GLSLANG)/glslang \
	-I$(GLSLANG)/glslang/MachineIndependent \
	-I$(GLSLANG)/glslang/OSDependent/Linux \
	-I$(GLSLANG)/OGLCompilersDLL \
	-I$(GLSLANG)

LunarGLASS_INCLUDES := \
	-I$(CORE) \
	-I. \
	-I..

ifeq ($(wildcard $(LLVMCONFIG)),)
$(error Invalid path to LLVM config: $(LLVMCONFIG))
endif

commonbuilder_SOURCES := \
    ../Core/Exceptions.cpp \
    ../Core/Options.cpp \
    ../Core/TopBuilder.cpp

glslangadapter_SOURCES := \
    ../Frontends/glslang/GlslangToTop.cpp \
    ../Frontends/glslang/GlslangToTopVisitor.cpp

glslang_SOURCES := \
    ../Frontends/glslang/CodeGen.cpp \
    ../Frontends/glslang/Link.cpp \
    ../Frontends/glslang/main.cpp \
    ../Standalone/LunarGManager.cpp \
    ../Standalone/OptionParse.cpp \
    ../Backends/Dummy/BottomToDummy.cpp \
    ../Backends/GLSL/BottomToGLSL.cpp \
    ../Backends/TGSI/BottomToTgsi.cpp \
    ../../glslang/glslang/MachineIndependent/Constant.cpp \
    ../../glslang/glslang/MachineIndependent/gen_glslang_tab.cpp \
    ../../glslang/glslang/MachineIndependent/InfoSink.cpp \
    ../../glslang/glslang/MachineIndependent/Initialize.cpp \
    ../../glslang/glslang/MachineIndependent/Intermediate.cpp \
    ../../glslang/glslang/MachineIndependent/intermOut.cpp \
    ../../glslang/glslang/MachineIndependent/linkValidate.cpp \
	../../glslang/glslang/MachineIndependent/limits.cpp \
    ../../glslang/glslang/MachineIndependent/IntermTraverse.cpp \
    ../../glslang/glslang/MachineIndependent/parseConst.cpp \
    ../../glslang/glslang/MachineIndependent/ParseHelper.cpp \
    ../../glslang/glslang/MachineIndependent/PoolAlloc.cpp \
    ../../glslang/glslang/MachineIndependent/QualifierAlive.cpp \
    ../../glslang/glslang/MachineIndependent/RemoveTree.cpp \
    ../../glslang/glslang/MachineIndependent/reflection.cpp \
    ../../glslang/glslang/MachineIndependent/ShaderLang.cpp \
    ../../glslang/glslang/MachineIndependent/SymbolTable.cpp \
    ../../glslang/glslang/MachineIndependent/Versions.cpp \
    ../../glslang/glslang/MachineIndependent/Scan.cpp \
    ../../glslang/glslang/OSDependent/Linux/ossource.cpp \
    ../../glslang/glslang/MachineIndependent/preprocessor/Pp.cpp \
    ../../glslang/glslang/MachineIndependent/preprocessor/PpAtom.cpp \
    ../../glslang/glslang/MachineIndependent/preprocessor/PpContext.cpp \
    ../../glslang/glslang/MachineIndependent/preprocessor/PpMemory.cpp \
    ../../glslang/glslang/MachineIndependent/preprocessor/PpScanner.cpp \
    ../../glslang/glslang/MachineIndependent/preprocessor/PpSymbols.cpp \
    ../../glslang/glslang/MachineIndependent/preprocessor/PpTokens.cpp \
    ../../glslang/OGLCompilersDLL/InitializeDll.cpp

lunarglass_SOURCES := \
    ../Core/Backend.cpp \
    ../Core/BottomIR.cpp \
    ../Core/BottomTranslator.cpp \
    ../Core/Passes/Analysis/IdentifyStructures.cpp \
    ../Core/Passes/Immutable/BackEndPointer.cpp \
    ../Core/Passes/Transforms/CanonicalizeCFG.cpp \
    ../Core/Passes/Transforms/CanonicalizeInsts.cpp \
    ../Core/Passes/Transforms/CoalesceInserts.cpp \
    ../Core/Passes/Transforms/DecomposeInsts.cpp \
    ../Core/Passes/Transforms/FlattenConditionalAssignments.cpp \
    ../Core/Passes/Transforms/GatherInsts.cpp \
    ../Core/Passes/Transforms/IntrinsicCombine.cpp \
    ../Core/Passes/Transforms/Scalarize.cpp \
    ../Core/Passes/Util/BasicBlockUtil.cpp \
    ../Core/Passes/Util/ConditionalUtil.cpp \
    ../Core/Passes/Util/InstructionUtil.cpp \
    ../Core/TopToBottom.cpp \
    ../Core/Util.cpp

glslangadapter_OBJECTS = $(glslangadapter_SOURCES:%.cpp=%.o)
commonbuilder_OBJECTS = $(commonbuilder_SOURCES:%.cpp=%.o)
lunarglass_OBJECTS = $(lunarglass_SOURCES:%.cpp=%.o)

glslang_C_SOURCES := $(filter %.c, $(glslang_SOURCES))
glslang_CXX_SOURCES := $(filter %.cpp, $(glslang_SOURCES))
glslang_OBJECTS := \
	$(glslang_C_SOURCES:.c=.o) \
	$(glslang_CXX_SOURCES:.cpp=.o)

glslang_CPPFLAGS := \
	$(LunarGLASS_INCLUDES) \
	$(glslang_INCLUDES)

ifeq ($(BUILD),release)

glslang_CFLAGS := \
	$(shell $(LLVMCONFIG) --cflags) \
	-Wall -s -O2

glslang_CXXFLAGS := \
	$(shell $(LLVMCONFIG) --cxxflags) \
	-Wall -DNDEBUG -s -O2 -Wno-overloaded-virtual \
	-Wno-sign-compare -Wno-switch -Wno-cast-qual

build_DIR := Release

else

glslang_CFLAGS := \
	$(shell $(LLVMCONFIG) --cflags) \
	-Wall -g -O0

glslang_CXXFLAGS := \
	$(shell $(LLVMCONFIG) --cxxflags) \
	-Wall -U NDEBUG -g -O0 -Wno-overloaded-virtual \
	-Wno-sign-compare -Wno-switch -Wno-cast-qual

build_DIR := Debug

endif

glslang_LDFLAGS := \
	$(shell $(LLVMCONFIG) --ldflags)

glslang_LIBS := \
	$(shell $(LLVMCONFIG) --libs) -lpthread -ldl

.PHONY: all
all: $(build_DIR)/glslang

$(build_DIR)/libglslangadapter.a: $(glslangadapter_OBJECTS)
	rm -f $@
	ar cq $@ $(glslangadapter_OBJECTS)

$(build_DIR)/libcommonbuilder.a: $(commonbuilder_OBJECTS)
	rm -f $@
	ar cq $@ $(commonbuilder_OBJECTS)

$(build_DIR)/liblunarglass.a: $(lunarglass_OBJECTS)
	rm -f $@
	ar cq $@ $(lunarglass_OBJECTS)

.PHONY: release
release:
	+make "BUILD=release" Release/glslang

.PHONY: debug
debug:
	+make "BUILD=debug" Debug/glslang

all_libs := $(build_DIR)/libglslangadapter.a $(build_DIR)/liblunarglass.a $(build_DIR)/libcommonbuilder.a

$(build_DIR)/glslang: build_dir $(CORE)/Revision.h $(glslang_OBJECTS) $(all_libs)
	$(CXX) -o $@ $(LDFLAGS) $(glslang_LDFLAGS) -L$(build_DIR) \
		$(glslang_OBJECTS) \
		-lglslangadapter -llunarglass -lcommonbuilder \
		$(glslang_LIBS)
	cp $@ ../test/glslang.exe

build_dir:
	mkdir -p $(build_DIR)

$(CORE)/Revision.h: $(CORE)/RevisionPrefix ../LastKnownGood
	cat $(CORE)/RevisionPrefix ../LastKnownGood > $(CORE)/Revision.h

.c.o:
	$(CC) -c -o $@ $(glslang_CPPFLAGS) $(glslang_CFLAGS) \
		-Dtrue=1 -Dfalse=0 -Dbool=char $(CPPFLAGS) $(CFLAGS) $<

.cpp.o:
	$(CXX) -c -o $@ $(glslang_CPPFLAGS) $(glslang_CXXFLAGS) \
		$(CPPFLAGS) $(CXXFLAGS) $<

.PHONY: clean
clean:
	rm -f $(glslang_OBJECTS) $(glslangadapter_OBJECTS) $(commonbuilder_OBJECTS) $(lunarglass_OBJECTS)
	rm -f Release/glslang Debug/glslang ../test/glslang.exe depend depend.bak
	rm -f Release/*.a Debug/*.a

depend: $(glslang_SOURCES) $(glslangadapter_SOURCES) $(lunarglass_SOURCES) $(commonbuilder_SOURCES)
	rm -f depend
	touch depend
	makedepend -fdepend $(glslang_CPPFLAGS) $(glslang_SOURCES) \
		2> /dev/null

-include depend
