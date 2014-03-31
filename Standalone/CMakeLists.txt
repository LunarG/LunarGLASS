cmake_minimum_required(VERSION 2.8)

include_directories(.. ../Core ${GLSLANGINCLUDES} ${LLVMINCLUDES})

set(SOURCES
    main.cpp
    OptionParse.cpp
    CodeGen.cpp
    Link.cpp)

add_executable(LunarGOO ${SOURCES})

if(WIN32)
    source_group("Source" REGULAR_EXPRESSION "[^/]*")
endif(WIN32)

target_link_libraries(LunarGOO
                      glslangFrontend
                      GLSLBackend
                      core
                      ${GLSLANGLIBS}
                      glslangFrontend
                      ${LLVMLIBS}
                      ${LLVMLIBS}
                      ${SYSTEM_LIBS})

install(TARGETS LunarGOO
        RUNTIME DESTINATION bin)
