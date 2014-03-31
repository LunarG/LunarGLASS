cmake_minimum_required(VERSION 2.8)

include_directories(. ../.. ../../Core ${GLSLANGINCLUDES} ${LLVMINCLUDES})

set(SOURCES
    GlslangToTop.cpp
    GlslangToTopVisitor.cpp)

set(HEADERS
    GlslangToTop.h
    GlslangToTopVisitor.h)

add_library(glslangFrontend STATIC ${SOURCES} ${HEADERS})

if(WIN32)
    source_group("Source" REGULAR_EXPRESSION "[^/]*")
endif(WIN32)

install(TARGETS glslangFrontend
        ARCHIVE DESTINATION lib)