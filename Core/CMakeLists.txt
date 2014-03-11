cmake_minimum_required(VERSION 2.8)

add_subdirectory(Passes)

include_directories(. ${LLVMINCLUDES})

set(SOURCES
    Backend.cpp
    BottomIR.cpp
    BottomTranslator.cpp
    Exceptions.cpp
    Options.cpp
    TopBuilder.cpp
    TopToBottom.cpp
    Util.cpp)

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
    Util.h)

add_library(core STATIC ${SOURCES} ${HEADERS})
