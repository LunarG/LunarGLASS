//
// Copyright (C) 2002-2005  3Dlabs Inc. Ltd.
// Copyright (C) 2010-2014 LunarG, Inc.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 
//     Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.
// 
//     Redistributions in binary form must reproduce the above
//     copyright notice, this list of conditions and the following
//     disclaimer in the documentation and/or other materials provided
//     with the distribution.
// 
//     Neither the name of LunarG Inc. nor the names of its
//     contributors may be used to endorse or promote products derived
//     from this software without specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
// LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
// ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//

#define _CRT_SECURE_NO_WARNINGS

// glslang includes
#include "glslang/Include/ShHandle.h"
#include "glslang/Public/ShaderLang.h"

// LunarGLASS includes
#include "Frontends/glslang/GlslangToTop.h"
#include "Core/Options.h"
#include "Backends/GLSL/GlslManager.h"

#include <string.h>

#ifdef _WIN32
    #include <windows.h>
    #include <psapi.h>
#else
    #include <cstdlib>
#endif

//#define USE_DEPRECATED_GLSLANG

// For overriding the output language version.
int TargetDefinitionVersion = 0;
EProfile TargetDefinitionProfile = EBadProfile;

//
// Just placeholders for testing purposes.  The stand-alone environment
// can't actually do a full link without something specifying real
// attribute bindings.
//
ShBinding FixedAttributeBindings[] = {
    { "gl_Vertex", 15 },
    { "gl_Color", 10 },
    { "gl_Normal", 7 },
};

ShBindingTable FixedAttributeTable = { 3, FixedAttributeBindings };

namespace {

//
// Return codes from main.
//
enum TFailCode {
    ESuccess = 0,
    EFailUsage,
    EFailCompile,
    EFailLink,
    EFailCompilerCreate,
    EFailLinkerCreate
};

int Options = 0;
const char* ExecutableName;

// Globally track if any compile or link failure.
bool CompileFailed = false;
bool LinkFailed = false;

//
// Set up the per compile resources
//
void GenerateResources(TBuiltInResource& resources)
{
    resources.maxLights = 32;
    resources.maxClipPlanes = 6;
    resources.maxTextureUnits = 32;
    resources.maxTextureCoords = 32;
    resources.maxVertexAttribs = 64;
    resources.maxVertexUniformComponents = 4096;
    resources.maxVaryingFloats = 64;
    resources.maxVertexTextureImageUnits = 32;
    resources.maxCombinedTextureImageUnits = 80;
    resources.maxTextureImageUnits = 32;
    resources.maxFragmentUniformComponents = 4096;
    resources.maxDrawBuffers = 32;
    resources.maxVertexUniformVectors = 128;
    resources.maxVaryingVectors = 8;
    resources.maxFragmentUniformVectors = 16;
    resources.maxVertexOutputVectors = 16;
    resources.maxFragmentInputVectors = 15;
    resources.minProgramTexelOffset = -8;
    resources.maxProgramTexelOffset = 7;
    resources.maxClipDistances = 8;
    resources.maxComputeWorkGroupCountX = 65535;
    resources.maxComputeWorkGroupCountY = 65535;
    resources.maxComputeWorkGroupCountZ = 65535;
    resources.maxComputeWorkGroupSizeX = 1024;
    resources.maxComputeWorkGroupSizeX = 1024;
    resources.maxComputeWorkGroupSizeZ = 64;
    resources.maxComputeUniformComponents = 1024;
    resources.maxComputeTextureImageUnits = 16;
    resources.maxComputeImageUniforms = 8;
    resources.maxComputeAtomicCounters = 8;
    resources.maxComputeAtomicCounterBuffers = 1;
    resources.maxVaryingComponents = 60;
    resources.maxVertexOutputComponents = 64;
    resources.maxGeometryInputComponents = 64;
    resources.maxGeometryOutputComponents = 128;
    resources.maxFragmentInputComponents = 128;
    resources.maxImageUnits = 8;
    resources.maxCombinedImageUnitsAndFragmentOutputs = 8;
    resources.maxImageSamples = 0;
    resources.maxVertexImageUniforms = 0;
    resources.maxTessControlImageUniforms = 0;
    resources.maxTessEvaluationImageUniforms = 0;
    resources.maxGeometryImageUniforms = 0;
    resources.maxFragmentImageUniforms = 8;
    resources.maxCombinedImageUniforms = 8;
    resources.maxGeometryTextureImageUnits = 16;
    resources.maxGeometryOutputVertices = 256;
    resources.maxGeometryTotalOutputComponents = 1024;
    resources.maxGeometryUniformComponents = 1024;
    resources.maxGeometryVaryingComponents = 64;
    resources.maxTessControlInputComponents = 128;
    resources.maxTessControlOutputComponents = 128;
    resources.maxTessControlTextureImageUnits = 16;
    resources.maxTessControlUniformComponents = 1024;
    resources.maxTessControlTotalOutputComponents = 4096;
    resources.maxTessEvaluationInputComponents = 128;
    resources.maxTessEvaluationOutputComponents = 128;
    resources.maxTessEvaluationTextureImageUnits = 16;
    resources.maxTessEvaluationUniformComponents = 1024;
    resources.maxTessPatchComponents = 120;
    resources.maxPatchVertices = 32;
    resources.maxTessGenLevel = 64;
    resources.maxViewports = 16;
    resources.maxVertexAtomicCounters = 0;
    resources.maxTessControlAtomicCounters = 0;
    resources.maxTessEvaluationAtomicCounters = 0;
    resources.maxGeometryAtomicCounters = 0;
    resources.maxFragmentAtomicCounters = 8;
    resources.maxCombinedAtomicCounters = 8;
    resources.maxAtomicCounterBindings = 1;
    resources.maxVertexAtomicCounterBuffers = 0;
    resources.maxTessControlAtomicCounterBuffers = 0;
    resources.maxTessEvaluationAtomicCounterBuffers = 0;
    resources.maxGeometryAtomicCounterBuffers = 0;
    resources.maxFragmentAtomicCounterBuffers = 1;
    resources.maxCombinedAtomicCounterBuffers = 1;
    resources.maxAtomicCounterBufferSize = 16384;
    resources.maxTransformFeedbackBuffers = 4;
    resources.maxTransformFeedbackInterleavedComponents = 64;
    resources.limits.nonInductiveForLoops = 1;
    resources.limits.whileLoops = 1;
    resources.limits.doWhileLoops = 1;
    resources.limits.generalUniformIndexing = 1;
    resources.limits.generalAttributeMatrixVectorIndexing = 1;
    resources.limits.generalVaryingIndexing = 1;
    resources.limits.generalSamplerIndexing = 1;
    resources.limits.generalVariableIndexing = 1;
    resources.limits.generalConstantMatrixVectorIndexing = 1;
}

//
//   print usage to stdout
//
void usage(bool advanced)
{
    if (! advanced) {
        printf("Basic usage:\n"
               "%s [options] <filename>\n"
               "\n"
               "Where: each 'file' ends in\n"
               "    .vert for a vertex shader\n"
               "    .tesc for a tessellation control shader\n"
               "    .tese for a tessellation evaluation shader\n"
               "    .geom for a geometry shader\n"
               "    .frag for a fragment shader\n"
               "    .comp for a compute shader\n"
               "\n"
               "Standard Output will receive new shader.\n"
               "Standard Error will receive an information log.\n", ExecutableName);

        printf("\n");
        printf("Basic options:\n"
               "-<version>: set output version, where <version> is 100, 110, ..., 300es, ..., 430core, 430compatibility \n"
               "  -o  obfuscate\n"
               "  -r  relaxed semantic error-checking mode\n"
               "  -s  silent mode\n"
               "  -w  suppress warnings (except as required by #extension : warn)\n"
               "  -z  see developer options\n");
    }
    
    if (advanced) {
        printf("Developer options:\n"
               "  -a  dump LunarGLASS Top IR and Bottom IR\n"
               "  -i  intermediate tree (glslang AST) is printed out\n"
               "  -m  memory leak mode\n");
    }
}

TFailCode ParseCommandLine(int argc, char* argv[], std::vector<const char*>& names)
{
    ExecutableName = argv[0] + strlen(argv[0]) - 1;
    while (*ExecutableName == '/' || *ExecutableName == '\\')
        --ExecutableName;
    while (*ExecutableName != '/' && *ExecutableName != '\\' && ExecutableName - argv[0] > 0)
        --ExecutableName;
    if (*ExecutableName == '/' || *ExecutableName == '\\')
        ++ExecutableName;

    if (argc < 2) {
        usage(false);
        return EFailUsage;
    }

    argc--;
    argv++;
    for (; argc >= 1; argc--, argv++) {
        if (argv[0][0] == '-') {
            switch (argv[0][1]) {
            case '1':
            case '2':
            case '3':
            case '4':
            {
                const char* versionStr = &argv[0][1];
                const char* profileStr = &argv[0][4];
                if (versionStr[1] < '0' || versionStr[1] > '9' || versionStr[2] != '0') {
                    usage(false);
                    return EFailUsage;
                }
                TargetDefinitionVersion = 100 * (versionStr[0] - '0') + 10 * (versionStr[1] - '0');
                if (profileStr[0] == 0) {
                    if (TargetDefinitionVersion == 100 || TargetDefinitionVersion == 300)                            
                        TargetDefinitionProfile = EEsProfile;
                    else if (TargetDefinitionVersion < 150)
                        TargetDefinitionProfile = ENoProfile;
                    else
                        TargetDefinitionProfile = ECoreProfile;
                } else if (strcmp(profileStr, "es") == 0)
                    TargetDefinitionProfile = EEsProfile;
                else if (strcmp(profileStr, "core") == 0)
                    TargetDefinitionProfile = ECoreProfile;
                else if (strcmp(profileStr, "compatibility") == 0)
                    TargetDefinitionProfile = ECompatibilityProfile;
                else {
                    usage(false);
                    return EFailUsage;
                }
                break;
            }
            case 'a':
                Options |= gla::EOptionAssembly;
                break;
            case 'i': 
                Options |= gla::EOptionIntermediate;       
                break;
            case 'm':
                Options |= gla::EOptionMemoryLeakMode;
                break;
            case 'o':
                gla::Options.obfuscate = true;
                break;
            case 'r':
                Options |= gla::EOptionRelaxedErrors;
                break;
            case 's':
                Options |= gla::EOptionSuppressInfolog;
                break;
            case 'w':
                Options |= gla::EOptionSuppressWarnings;
                break;
            case 'z':
                usage(true);
                return EFailUsage;
            default:
                usage(false);
                return EFailUsage;
            }
        } else
            names.push_back(argv[0]);
    }

    return ESuccess;
}

void SetMessageOptions(EShMessages& messages)
{
    if (Options & gla::EOptionRelaxedErrors)
        messages = (EShMessages)(messages | EShMsgRelaxedErrors);
    if (Options & gla::EOptionIntermediate)
        messages = (EShMessages)(messages | EShMsgAST);
    if (Options & gla::EOptionSuppressWarnings)
        messages = (EShMessages)(messages | EShMsgSuppressWarnings);
}

//
//   Deduce the language from the filename.  Files must end in one of the
//   following extensions:
//
//   .vert = vertex
//   .tesc = tessellation control
//   .tese = tessellation evaluation
//   .geom = geometry
//   .frag = fragment
//   .comp = compute
//
EShLanguage FindLanguage(const std::string& name)
{
    size_t ext = name.rfind('.');
    if (ext == std::string::npos) {
        usage(false);
        return EShLangVertex;
    }

    std::string suffix = name.substr(ext + 1, std::string::npos);
    if (suffix == "vert")
        return EShLangVertex;
    else if (suffix == "tesc")
        return EShLangTessControl;
    else if (suffix == "tese")
        return EShLangTessEvaluation;
    else if (suffix == "geom")
        return EShLangGeometry;
    else if (suffix == "frag")
        return EShLangFragment;
    else if (suffix == "comp")
        return EShLangCompute;

    usage(false);
    return EShLangVertex;
}

#ifndef _WIN32

#include <errno.h>

int fopen_s(
   FILE** pFile,
   const char* filename,
   const char* mode
)
{
   if (!pFile || !filename || !mode) {
      return EINVAL;
   }

   FILE* f = fopen(filename, mode);
   if (! f) {
      if (errno != 0) {
         return errno;
      } else {
         return ENOENT;
      }
   }
   *pFile = f;

   return 0;
}

#endif

//
//   Malloc a set of strings (just 1 though, the point is to match the API) of sufficient size and read a file into it.
//
char** ReadFileData(const char *fileName)
{
    FILE *in;
    int errorCode = fopen_s(&in, fileName, "r");
    int count = 0;    
    char** fileData = (char**)malloc(sizeof(char *));
    fileData[0] = 0;

    if (errorCode) {
        printf("Error: unable to open input file: %s\n", fileName);
        return 0;
    }

    while (fgetc(in) != EOF)
        count++;

    fseek(in, 0, SEEK_SET);
    
    if (! (fileData[0] = (char*)malloc(count + 2))) {
        printf("Error allocating memory\n");
        return 0;
    }
    if (fread(fileData[0], 1, count, in) != count) {
        printf("Error reading input file: %s\n", fileName);
        return 0;
    }
    fileData[0][count] = '\0';
    fclose(in);

    return fileData;
}

void FreeFileData(char **fileData)
{
    if (fileData) {
        if (fileData[0])
            free(fileData[0]);
        free(fileData);
    }
}

#ifdef USE_DEPRECATED_GLSLANG

//
//  Read a file's data into a string, and compile it using ShCompile, the old glslang interface
//
bool CompileFile(const char *fileName, ShHandle compiler, int Options, const TBuiltInResource *resources)
{
    int ret;
    char **data = ReadFileData(fileName);

#ifdef _WIN32
    PROCESS_MEMORY_COUNTERS counters;  // just for memory leak testing
#endif

    if (! data)
        return false;

    EShMessages messages = EShMsgDefault;
    if (! (Options & EDebugOpGiveWarnings))
        messages = (EShMessages)(messages | EShMsgSuppressWarnings);
    if (Options & EDebugOpRelaxedErrors)
        messages = (EShMessages)(messages | EShMsgRelaxedErrors);

    for (int i = 0; i < ((Options & EDebugOpMemoryLeakMode) ? 100 : 1); ++i) {
        for (int j = 0; j < ((Options & EDebugOpMemoryLeakMode) ? 100 : 1); ++j)
            ret = ShCompile(compiler, data, 1, 0, EShOptNone, resources, Options, 100, false, EShMsgDefault);

#ifdef _WIN32
        if (Options & EDebugOpMemoryLeakMode) {
            GetProcessMemoryInfo(GetCurrentProcess(), &counters, sizeof(counters));
            printf("Working set size: %d\n", counters.WorkingSetSize);
        }
#endif
    }

    FreeFileData(data);

    return ret ? true : false;
}

#endif

//
// Uses the new glslang C++ interface instead of the old handle-based interface.
//
void TranslateShaders(const std::vector<const char*>& names, const TBuiltInResource *resources)
{
    // keep track of what to free
    std::list<glslang::TShader*> shaders;
    
    EShMessages messages = EShMsgDefault;
    SetMessageOptions(messages);

    //
    // Per-shader front-end processing...
    //

    glslang::TProgram& program = *new glslang::TProgram;
    for (int n = 0; n < (int)names.size(); ++n) {
        EShLanguage stage = FindLanguage(names[n]);
        glslang::TShader* shader = new glslang::TShader(stage);
        shaders.push_back(shader);
    
        char** shaderStrings = ReadFileData(names[n]);
        if (! shaderStrings) {
            usage(false);
            return;
        }

        shader->setStrings(shaderStrings, 1);

        if (! shader->parse(resources, 100, false, messages)) {
            CompileFailed = true;
            if (! (Options & gla::EOptionSuppressInfolog)) {
                puts(names[n]);
                puts(shader->getInfoLog());
            }

            return;
        }
        
        program.addShader(shader);

        FreeFileData(shaderStrings);
    }

    //
    // Program-level front-end processing...
    //

    if (! program.link(messages)) {
        LinkFailed = true;
        if (! (Options & gla::EOptionSuppressInfolog))
            puts(program.getInfoLog());

        return;
    }

    if (Options & gla::EOptionDumpReflection) {
        program.buildReflection();
        program.dumpReflection();
    }

    //
    // For each populated stage, translate the linked result through to the back end.
    //
    for (int stage = 0; stage < EShLangCount; ++stage) {
        const glslang::TIntermediate* intermediate = program.getIntermediate((EShLanguage)stage);
        if (! intermediate)
            continue;

        const int numReps = 1;
        for (int reps = 0; reps < numReps; ++reps) {
            gla::GlslManager manager;

            // Generate the Top IR
            TranslateGlslangToTop(*intermediate, manager);

            // Optionally override any versioning/extensions here.
            // (If this is not done, it will inherit from the original shader source.)
            if (TargetDefinitionVersion != 0)
                manager.setVersion(TargetDefinitionVersion);
            if (TargetDefinitionProfile != EBadProfile)
                manager.setProfile(TargetDefinitionProfile);

            if (reps + 1 == numReps)
                if (Options & gla::EOptionAssembly)
                    manager.dump("\nTop IR:\n");

            // Generate the Bottom IR
            manager.translateTopToBottom();
    
            if (reps + 1 == numReps)
                if (Options & gla::EOptionAssembly)
                    manager.dump("\n\nBottom IR:\n");

            // Generate the GLSL output
            manager.translateBottomToTarget();

            // Get and print the generated GLSL output
            if (reps + 1 == numReps)
                if (manager.getGeneratedShader())
                    printf("%s\n", manager.getGeneratedShader());
        }
    }

    // Free everything up, program has to go before the shaders
    // because it might have merged stuff from the shaders, and
    // the stuff from the shaders has to have its destructors called
    // before the pools holding the memory in the shaders is freed.
    delete &program;
    while (shaders.size() > 0) {
        delete shaders.back();
        shaders.pop_back();
    }
}

void InfoLogMsg(const char* msg, const char* name, const int num)
{
    fprintf(stderr, num >= 0 ? "#### %s %s %d INFO LOG ####\n" :
           "#### %s %s INFO LOG ####\n", msg, name, num);
}

}; // end anonymous namespace

int C_DECL main(int argc, char* argv[])
{
    std::vector<const char*> names;
    TFailCode failCode = ParseCommandLine(argc, argv, names);
    if (failCode)
        return failCode;

#ifdef USE_DEPRECATED_GLSLANG
    int numCompilers = 0;
    ShHandle compilers[EShLangCount];
    ShInitialize();

    for (int n = 0; n < (int)names.size(); ++n) {
        compilers[numCompilers] = ShConstructCompiler(FindLanguage(names[n]), Options);
        if (compilers[numCompilers] == 0)
            return EFailCompilerCreate;
        ++numCompilers;

        TBuiltInResource resources;
        GenerateResources(resources);
        if (! CompileFile(names[n], compilers[numCompilers-1], Options, &resources))
            CompileFailed = true;
    }

    if (! numCompilers) {
        usage(false);

        return EFailUsage;
    }

    if (! (Options & EDebugOpSuppressInfolog)) {
        for (int i = 0; i < numCompilers; ++i) {
            InfoLogMsg("BEGIN", "COMPILER", i);
            fprintf(stderr, "%s", ShGetInfoLog(compilers[i]));
            InfoLogMsg("END", "COMPILER", i);
        }
    }

    for (int i = 0; i < numCompilers; ++i)
        ShDestruct(compilers[i]);

    ShFinalize();
#else

    glslang::InitializeProcess();

    TBuiltInResource resources;
    GenerateResources(resources);

    TranslateShaders(names, &resources);

    glslang::FinalizeProcess();

#endif  // USE_DEPRECATED_GLSLANG

    if (CompileFailed)
        return EFailCompile;
    if (LinkFailed)
        return EFailLink;

    return 0;
}
