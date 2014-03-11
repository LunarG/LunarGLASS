cmake_minimum_required(VERSION 2.8)

include_directories(.. ../Core ${LLVMINCLUDES})

set(SOURCES
    LunarGManager.cpp
    OptionParse.cpp)

add_executable(LunarGOO ${SOURCES})

target_link_libraries(LunarGOO
                      core
                      glslangFrontend
                      GLSLBackend
                      DummyBackend
                      TgsiBackend
                      analysis
                      immutable
                      transforms
                      util
                      ${CMAKE_SOURCE_DIR}/../glslang/build/install/lib/glslang.lib
                      ${CMAKE_SOURCE_DIR}/../glslang/build/install/lib/Preprocessor.lib
                      ${CMAKE_SOURCE_DIR}/../glslang/build/install/lib/OSDependent.lib
                      ${CMAKE_SOURCE_DIR}/../glslang/build/install/lib/OGLCompiler.lib
                      ${LLVMLIBS})

install(TARGETS LunarGOO
        RUNTIME DESTINATION bin)
