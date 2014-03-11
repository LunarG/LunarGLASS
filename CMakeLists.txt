cmake_minimum_required(VERSION 2.8)

project(LunarGLASS)

set(LLVMINCLUDES ${CMAKE_SOURCE_DIR}/Core/LLVM/llvm-3.4/build/install/include)
set(GLSLANGINCLUDES ${CMAKE_SOURCE_DIR}/../glslang)

link_directories(${CMAKE_SOURCE_DIR}/Core/LLVM/llvm-3.4/build/install/lib)

set(LLVMLIBS
    LLVMCore.lib
    LLVMAsmParser.lib
    LLVMipa.lib
    LLVMLinker.lib
    LLVMTransformUtils.lib
    LLVMTarget.lib
    LLVMAnalysis.lib
    LLVMScalarOpts.lib
    LLVMSupport.lib
    LLVMipo.lib
    LLVMInstCombine.lib)

add_definitions(-D_SCL_SECURE_NO_WARNINGS)

add_subdirectory(Frontends)
add_subdirectory(Core)
add_subdirectory(Backends)
add_subdirectory(StandAlone)
