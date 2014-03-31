cmake_minimum_required(VERSION 2.8)

include_directories(. ${LLVMINCLUDES})

set(SOURCES
    Backend.cpp
    BottomIR.cpp
    BottomTranslator.cpp
    Exceptions.cpp
    Options.cpp
    TopBuilder.cpp
    TopToBottom.cpp
    Util.cpp
    Passes/Analysis/IdentifyStructures.cpp
    Passes/Immutable/BackEndPointer.cpp
    Passes/Transforms/CanonicalizeCFG.cpp
    Passes/Transforms/CanonicalizeInsts.cpp
    Passes/Transforms/CoalesceInserts.cpp
    Passes/Transforms/DecomposeInsts.cpp
    Passes/Transforms/FlattenConditionalAssignments.cpp
    Passes/Transforms/GatherInsts.cpp
    Passes/Transforms/IntrinsicCombine.cpp
    Passes/Transforms/Scalarize.cpp
    Passes/Util/BasicBlockUtil.cpp
    Passes/Util/ConditionalUtil.cpp
    Passes/Util/InstructionUtil.cpp)

set(HEADERS
    Backend.h
    BottomIR.h
    Exceptions.h
    LunarGLASSManager.h
    LunarGLASSTopIR.h
    metadata.h
    Options.h
    PrivateManager.h
    Revision.h
    TopBuilder.h
    Util.h
    Passes/InitializePasses.h
    Passes/Passes.h
    Passes/PassSupport.h
    Passes/Analysis/IdentifyStructures.h
    Passes/Immutable/BackEndPointer.h
    Passes/Util/ADT.h
    Passes/Util/BasicBlockUtil.h
    Passes/Util/ConditionalUtil.h
    Passes/Util/ConstantUtil.h
    Passes/Util/DominatorsUtil.h
    Passes/Util/FunctionUtil.h
    Passes/Util/InstructionUtil.h
    Passes/Util/LoopUtil.h)

add_library(core STATIC ${SOURCES} ${HEADERS})

if(WIN32)
    source_group("Core" REGULAR_EXPRESSION "[^/]*")
    source_group("Passes\\Analysis" REGULAR_EXPRESSION "Passes/Analysis/*")
    source_group("Passes\\Immutable" REGULAR_EXPRESSION "Passes/Immutable/*")
    source_group("Passes\\Transforms" REGULAR_EXPRESSION "Passes/Transforms/*")
    source_group("Passes\\Util" REGULAR_EXPRESSION "Passes/Util/*")
endif(WIN32)

install(TARGETS core 
        ARCHIVE DESTINATION lib)
