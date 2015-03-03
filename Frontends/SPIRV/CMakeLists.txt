cmake_minimum_required(VERSION 2.8)

include_directories(. ../.. ../../Core ${GLSLANGINCLUDES} ${LLVMINCLUDES})

set(SOURCES
    SpvToTop.cpp)

set(HEADERS
    SpvToTop.h)

add_library(SpvFrontend STATIC ${SOURCES} ${HEADERS})

if(WIN32)
    source_group("Source" REGULAR_EXPRESSION "[^/]*")
endif(WIN32)

install(TARGETS SpvFrontend
        ARCHIVE DESTINATION lib)
