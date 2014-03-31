cmake_minimum_required(VERSION 2.8)

include_directories(. ../.. ../../Core ${GLSLANGINCLUDES} ${LLVMINCLUDES})

set(SOURCES
    BottomToGLSL.cpp)

set(HEADERS
    GlslManager.h
    GlslTranslator.h
    GlslTarget.h)

add_library(GLSLBackend STATIC ${SOURCES} ${HEADERS})

if(WIN32)
    source_group("Source" REGULAR_EXPRESSION "[^/]*")
endif(WIN32)

install(TARGETS GLSLBackend
        ARCHIVE DESTINATION lib)