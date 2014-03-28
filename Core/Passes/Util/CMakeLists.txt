cmake_minimum_required(VERSION 2.8)

include_directories(../.. ${LLVMINCLUDES})

set(SOURCES
    BasicBlockUtil.cpp
    ConditionalUtil.cpp
    InstructionUtil.cpp)

set(HEADERS
    ADT.h
    BasicBlockUtil.h
    ConditionalUtil.h
    ConstantUtil.h
    DominatorsUtil.h
    FunctionUtil.h
    InstructionUtil.h
    LoopUtil.h)

add_library(util STATIC ${SOURCES} ${HEADERS})

install(TARGETS util 
        ARCHIVE DESTINATION lib)
