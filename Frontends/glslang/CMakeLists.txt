cmake_minimum_required(VERSION 2.8)

include_directories(. ../.. ../../Core ${GLSLANGINCLUDES} ${LLVMINCLUDES})

set(SOURCES
    CodeGen.cpp
    GlslangToTop.cpp
    GlslangToTopVisitor.cpp
    Link.cpp
    main.cpp)

set(HEADERS
    GlslangToTop.h
    GlslangToTopVisitor.h)

add_library(glslangFrontend STATIC ${SOURCES} ${HEADERS})

install(TARGETS glslangFrontend
        ARCHIVE DESTINATION lib)