cmake_minimum_required(VERSION 2.8)

include_directories(. ../../Core ${GLSLANGINCLUDES} ${LLVMINCLUDES})

set(SOURCES
    BottomToTgsi.cpp)

set(HEADERS
    TgsiTarget.h)

add_library(TgsiBackend STATIC ${SOURCES} ${HEADERS})

if(WIN32)
    source_group("Source" REGULAR_EXPRESSION "[^/]*")
endif(WIN32)

install(TARGETS TgsiBackend 
        ARCHIVE DESTINATION lib)