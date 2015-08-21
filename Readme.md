# Licensing
 
LunarGLASS is available via a BSD-style open source license.

# Resources

See http://www.lunarglass.org/ for a variety of information about LunarGLASS.

Google code: On 8/21/2015, LunarGLASS was transfered from https://code.google.com/p/lunarglass/.

# Downloading and Building

The standard way to build is using glslang as the GLSL front end.  This is currently done by making them sibling directories:

```
PathOfYourChoice/glslang
PathOfYourChoice/LunarGLASS
```

(See https://github.com/KhronosGroup/glslang for details about glslang.)

## Getting the code

1. In PathOfYourChoice, clone the glslang repository from https://github.com/KhronosGroup/glslang, making the glslang subdirectory.
2. In PathOfYourChoice, clone the LunarGLASS repository from https://github.com/LunarG/LunarGLASS, making the LunarGLASS subdirectory.
3. Get LLVM.  Download the LLVM 3.4 source code from http://llvm.org/releases/download.html#3.4 into your "LunarGLASS/Core/LLVM" directory, then extract it:

  ```
  cd LunarGLASS/Core/LLVM
  tar --gzip -xf llvm-3.4.src.tar.gz
  ```

4. The previous step overrwrote some LLVM files that LunarGLASS changes.  Restore them to LunarGLASS's versions, while still in the LunarGLASS/Core/LLVM directory:

  ```
  git checkout -f .  # put back the LunarGLASS versions of some LLVM files
  ```

## Building

LunarGLASS must be built after glslang and LLVM have been built.

### Building glslang

Use CMake, building in the directory PathOfYourChoice/glslang/build.

### Building LLVM

#### Building LLVM for Windows

1. Use version 2.7.6 of python (Get it from http://www.python.org/download/releases/2.7.6/.)
2. Run CMake.
3. Put the full path to your LunarGLASS/Core/LLVM/llvm-3.4 location in "Where is the source code:" and add "/build" to it (LunarGLASS/Core/LLVM/llvm-3.4/build) in "Where to build the binaries:"
4. Press "Configure" button in CMake and say yes to create the build directory.
5. Select your Visual Studio and "Use default native compilers" and "Finish".
6. Change CMAKE_INSTALL_PREFIX to "install" (no path).
7. Press "Configure" again.
8. Press "Generate" button in CMake.
9. Open the LLVM.sln just created in llvm-3.4/build and build the INSTALL project.

#### Building LLVM for Linux

Summary:  LLVM uses a configure script while glslang and LunarGLASS use CMake.

1. First build set up:
 
  ```
  # first time only
  cd LunarGLASS/Core/LLVM/llvm-3.4
  mkdir build
  cd build
  ../configure
  ```

2. Build:

  ```
  # build or rebuild
  cd LunarGLASS/Core/LLVM/llvm-3.4/build
  make -j 8
  make install DESTDIR=`pwd`/install
  ```
  
### Building LunarGLASS (the LunarGOO standalone tool)

By default, this builds a command-line tool that translates GLSL -> LunarGLASS -> GLSL.

Use CMake, building in the directory PathOfYourChoice/LunarGLASS/build.

# Modes of use

Use a LunarGLASS stack to compile from a driver:

Per driver initialization:

1. Include LunarGLASSManager.h.  This header includes no other headers.
2. Use gla::getManager() to get a manager.  (From below, your compiler 
implementation will have derived a concrete class and factory for
it.)

Per compile:
 
 3. Pass this manager to a front end that builds LunarGLASS Top IR and 
    saves the llvm module into the manager using manager->setModule()
    and saves symbol tables using manager->set*Symbols().
 4. Use manager->translateTopToBottom() to create bottom IR.
 5. Use manager->translateBottomToTarget() to translate that to the
    compiler's back end target language.
 6. Use manager->clear() to free up structures specific to the 
    compile, e.g., things set in step 3.

Make a LunarGLASS back end:

 1. Make a manager that derives from the gla::PrivateManager in 
    PrivateManager.h and provides a factory gla::getManager()
    that makes one of these.
 2. Make a back end that derives from the gla::BackEnd()
    in Backend.h and have it supply methods to describe what form
    of Bottom IR it wants to consume.
 3. Also have your back end derive from gla::BackEndTranslator()
    to fill in all the methods needed to translate from Bottom IR
    to your target.  
 4. You will need to include BottomIR.h to consume the details of 
    Bottom IR.  Between this file, the LunarGLASSTopIR.h file it
    includes, and the specification, you have everything you need
    to interpret Bottom IR.
 5. Your private manager (from 1) will have to make one of these
    back ends (from 2) and back end translators (from 3) when it 
    is created.

Make a LunarGLASS front end:

 1. Include LunarGLASSManager.h and LunarGLASSTopIR.h in your 
    front end.  Between these and the specification, you have
    everything you need to create Top IR.
 2. Call your front end (as in step 3 in "Use a LunarGLASS 
    stack...") to translate your source language to Top IR
    and leave the llvm module in the manager.
