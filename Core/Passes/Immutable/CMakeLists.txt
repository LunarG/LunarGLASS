cmake_minimum_required(VERSION 2.8)

include_directories(../.. ${LLVMINCLUDES})

set(SOURCES
    BackEndPointer.cpp)

set(HEADERS
    BackEndPointer.h)

add_library(immutable STATIC ${SOURCES} ${HEADERS})
