cmake_minimum_required(VERSION 2.8)

include_directories(.. ../Core ${LLVMINCLUDES})

set(SOURCES
    LunarGManager.cpp
    OptionParse.cpp)

add_executable(LunarGOO ${SOURCES})

target_link_libraries(LunarGOO
                      glslangFrontend
                      GLSLBackend
                      DummyBackend
                      TgsiBackend
                      core
                      analysis
                      immutable
                      transforms
                      util
                      ${GLSLANGLIBS}
                      glslangFrontend
                      ${LLVMLIBS}
                      ${LLVMLIBS}
                      ${SYSTEM_LIBS})

install(TARGETS LunarGOO
        RUNTIME DESTINATION bin)
