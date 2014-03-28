cmake_minimum_required(VERSION 2.8)

include_directories(../.. ${LLVMINCLUDES})

set(SOURCES
    IdentifyStructures.cpp)

set(HEADERS
    IdentifyStructures.h)

add_library(analysis STATIC ${SOURCES} ${HEADERS})

install(TARGETS analysis 
        ARCHIVE DESTINATION lib)