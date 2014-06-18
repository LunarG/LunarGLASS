cmake_minimum_required(VERSION 2.8)

include_directories(.. ../Core ${GLSLANGINCLUDES} ${LLVMINCLUDES})
if(WIN32)
    include_directories(${GLSLANG_PATH}/glslang/OSDependent/Windows)
elseif(UNIX)
    include_directories(${GLSLANG_PATH}/glslang/OSDependent/Linux)
else(WIN32)
    message("unkown platform")
endif(WIN32)

set(SOURCES
    main.cpp
    OptionParse.cpp
    CodeGen.cpp
    Link.cpp)

set(HEADERS
    OptionParse.h)

add_executable(LunarGOO ${SOURCES} ${HEADERS})

if(WIN32)
    source_group("Source" REGULAR_EXPRESSION "[^/]*")
endif(WIN32)

target_link_libraries(LunarGOO
                      glslangFrontend
                      GLSLBackend
                      core
                      ${GLSLANGLIBS}
                      ${LLVMLIBS}
                      ${SYSTEM_LIBS})

install(TARGETS LunarGOO
        RUNTIME DESTINATION bin)
