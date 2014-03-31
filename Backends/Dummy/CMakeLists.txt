cmake_minimum_required(VERSION 2.8)

include_directories(. ../../Core ${GLSLANGINCLUDES} ${LLVMINCLUDES})

set(SOURCES
    BottomToDummy.cpp)

set(HEADERS
    DummyTarget.h)

add_library(DummyBackend STATIC ${SOURCES} ${HEADERS})

if(WIN32)
    source_group("Source" REGULAR_EXPRESSION "[^/]*")
endif(WIN32)

install(TARGETS DummyBackend 
        ARCHIVE DESTINATION lib)
