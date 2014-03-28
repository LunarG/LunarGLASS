cmake_minimum_required(VERSION 2.8)

include_directories(. ../.. ../../.. ${LLVMINCLUDES})
set(SOURCES
    CanonicalizeCFG.cpp
    CanonicalizeInsts.cpp
    CoalesceInserts.cpp
    DecomposeInsts.cpp
    FlattenConditionalAssignments.cpp
    GatherInsts.cpp
    IntrinsicCombine.cpp
    Scalarize.cpp)

add_library(transforms STATIC ${SOURCES})

install(TARGETS transforms 
        ARCHIVE DESTINATION lib)