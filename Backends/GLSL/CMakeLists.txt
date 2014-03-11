cmake_minimum_required(VERSION 2.8)

include_directories(. ../../Core ${GLSLANGINCLUDES} ${LLVMINCLUDES})

set(SOURCES
    BottomToGLSL.cpp)

set(HEADERS
    GlslTarget.h)

add_library(GLSLBackend STATIC ${SOURCES} ${HEADERS})
