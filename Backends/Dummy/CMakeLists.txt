cmake_minimum_required(VERSION 2.8)

include_directories(. ../../Core ${GLSLANGINCLUDES} ${LLVMINCLUDES})

set(SOURCES
    BottomToDummy.cpp)

set(HEADERS
    DummyTarget.h)

add_library(DummyBackend STATIC ${SOURCES} ${HEADERS})
