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

#include "glslang/Include/ShHandle.h"
#include "glslang/Public/ShaderLang.h"
#include <string.h>
#include <math.h>

#ifdef _WIN32
    #include <windows.h>
    #include <psapi.h>
#else
    #include <cstdlib>
#endif

#include "Options.h"

extern "C" {
    SH_IMPORT_EXPORT void ShOutputHtml();
}

// For overriding the output language version.
// Would be good to have a way of passing a target definition through the front end, or next to it...
extern int TargetDefinitionVersion;
extern EProfile TargetDefinitionProfile;

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

int debugOptions = EDebugOpGiveWarnings | EDebugOpRelaxedErrors;
bool delay = false;
const char* executableName;

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
void usage(const char* executableName, bool advanced)
{
    if (! advanced) {
        printf("Basic usage:\n"
               "%s [options] <filename>\n"
               "<filename> ends in .frag or .vert\n"
               "Standard Output will receive new shader.\n"
               "Standard Error will receive an information log.\n", executableName);

        printf("\n");
        printf("Basic options:\n"
               "-<version>: set output version, where <version> is 100, 110, ..., 300es, ..., 430core, 430compatibility \n"
               "-o: obfuscate\n"
               "-r: restrictive error checking (give all required errors)\n"
               "-s: silent mode (no information log)\n"
               "-w: suppress warnings\n"
               "-z: see developer options\n");
    }
    
    if (advanced) {
        printf("Developer options:\n"
               "-a: dump LunarGLASS Top IR and Bottom IR\n"
#ifdef _WIN32
               "-d: delay exit\n"
#endif
               "-i: dump AST\n"
               "-l: memory leak mode\n");
    }
}

TFailCode ParseCommandLine(int argc, char* argv[], std::vector<const char*>& names)
{
    executableName = argv[0] + strlen(argv[0]) - 1;
    while (*executableName == '/' || *executableName == '\\')
        --executableName;
    while (*executableName != '/' && *executableName != '\\' && executableName - argv[0] > 0)
        --executableName;
    if (*executableName == '/' || *executableName == '\\')
        ++executableName;

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
                    usage(executableName, false);
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
                    usage(executableName, false);
                    return EFailUsage;
                }
                break;
            }
            case 'a':
                debugOptions |= EDebugOpAssembly;
                break;
            case 'd':
                delay = true;
                break;
            case 'i': 
                debugOptions |= EDebugOpIntermediate;       
                break;
            case 'l':
                debugOptions |= EDebugOpMemoryLeakMode;
                break;
            case 'o':
                gla::Options.obfuscate = true;
                break;
            case 'r':
                debugOptions &= ~EDebugOpRelaxedErrors;
                break;
            case 's':
                debugOptions |= EDebugOpSuppressInfolog;
                break;
            case 'w':
                debugOptions &= ~EDebugOpGiveWarnings;
                break;
            case 'z':
                usage(executableName, true);
                return EFailUsage;
            default:
                usage(executableName, false);
                return EFailUsage;
            }
        } else
            names.push_back(argv[0]);
    }

    return ESuccess;
}

//
//   Deduce the language from the filename.  Files must end in one of the
//   following extensions:
//
//   .frag*    = fragment programs
//   .vert*    = vertex programs
//
EShLanguage FindLanguage(const char *name)
{
    if (!name)
        return EShLangVertex;

    const char *ext = strrchr(name, '.');

    if (ext && strcmp(ext, ".sl") == 0)
        for (; ext > name && ext[0] != '.'; ext--);

    ext = strrchr(name, '.');
    if (ext) {
        if (strncmp(ext, ".frag", 4) == 0) 
            return EShLangFragment;
    }

    return EShLangVertex;
}

//
//   Malloc a string of sufficient size and read a string into it.
//
#define MAX_SOURCE_STRINGS 5
char** ReadFileData(const char *fileName)
{
    FILE *in = fopen(fileName, "r");
    char *fdata;
    int count = 0;
    char**return_data=(char**)malloc(MAX_SOURCE_STRINGS+1);

    //return_data[MAX_SOURCE_STRINGS]=NULL;
    if (in == 0) {
        printf("Error: unable to open input file: %s\n", fileName);
        return 0;
    }

    while (fgetc(in) != EOF)
        count++;

	fseek(in, 0, SEEK_SET);
	
	
	if (!(fdata = (char *)malloc(count+2))) {
            printf("Error allocating memory\n");
            return 0;
    }
	if (fread(fdata,1,count, in)!=count) {
            printf("Error reading input file: %s\n", fileName);
            return 0;
    }
    fdata[count] = '\0';
    fclose(in);
    if(count==0){
        return_data[0]=(char*)malloc(count+2);
        return_data[0][0]='\0';
        return return_data;
    }

	int len = count;
    int ptr_len=0,i=0;
	while(count>0){
		return_data[i]=(char*)malloc(len+2);
		memcpy(return_data[i],fdata+ptr_len,len);
		return_data[i][len]='\0';
		count-=(len);
		ptr_len+=(len);
		if(count<len){
            if(count==0){
               break;
            }
           len = count;
		}
		++i;
	}
    return return_data;
}

void FreeFileData(char **data)
{
    free(data[0]);
}

//
//   Read a file's data into a string, and compile it using ShCompile
//
bool CompileFile(const char *fileName, ShHandle compiler, int debugOptions, const TBuiltInResource *resources)
{
    int ret;
    char **data = ReadFileData(fileName);

#ifdef _WIN32
    PROCESS_MEMORY_COUNTERS counters;  // just for memory leak testing
#endif

    if (! data)
        return false;

    EShMessages messages = EShMsgDefault;
    if (! (debugOptions & EDebugOpGiveWarnings))
        messages = (EShMessages)(messages | EShMsgSuppressWarnings);
    if (debugOptions & EDebugOpRelaxedErrors)
        messages = (EShMessages)(messages | EShMsgRelaxedErrors);

    for (int i = 0; i < ((debugOptions & EDebugOpMemoryLeakMode) ? 100 : 1); ++i) {
        for (int j = 0; j < ((debugOptions & EDebugOpMemoryLeakMode) ? 100 : 1); ++j)
            ret = ShCompile(compiler, data, 1, 0, EShOptNone, resources, debugOptions, 100, false, EShMsgDefault);

#ifdef _WIN32
        if (debugOptions & EDebugOpMemoryLeakMode) {
            GetProcessMemoryInfo(GetCurrentProcess(), &counters, sizeof(counters));
            printf("Working set size: %d\n", counters.WorkingSetSize);
        }
#endif
    }

    FreeFileData(data);

    return ret ? true : false;
}

void InfoLogMsg(const char* msg, const char* name, const int num)
{
    fprintf(stderr, num >= 0 ? "#### %s %s %d INFO LOG ####\n" :
           "#### %s %s INFO LOG ####\n", msg, name, num);
}

}; // end anonymous namespace

int C_DECL main(int argc, char* argv[])
{
    int numCompilers = 0;
    bool compileFailed = false;
    bool linkFailed = false;
    int i;
    TargetDefinitionProfile = EBadProfile;
    TargetDefinitionVersion = 0;

    ShHandle    compilers[EShLangCount];

    ShInitialize();

    std::vector<const char*> names;
    TFailCode failCode = ParseCommandLine(argc, argv, names);
    if (failCode)
        return failCode;

    for (int n = 0; n < (int)names.size(); ++n) {
        compilers[numCompilers] = ShConstructCompiler(FindLanguage(names[n]), debugOptions);
        if (compilers[numCompilers] == 0)
            return EFailCompilerCreate;
        ++numCompilers;

        TBuiltInResource resources;
        GenerateResources(resources);
        if (! CompileFile(names[n], compilers[numCompilers-1], debugOptions, &resources))
            compileFailed = true;
    }

    if (! numCompilers) {
        usage(executableName, false);

        return EFailUsage;
    }

    if (! (debugOptions & EDebugOpSuppressInfolog)) {
        for (i = 0; i < numCompilers; ++i) {
            InfoLogMsg("BEGIN", "COMPILER", i);
            fprintf(stderr, "%s", ShGetInfoLog(compilers[i]));
            InfoLogMsg("END", "COMPILER", i);
        }
    }

    for (i = 0; i < numCompilers; ++i)
        ShDestruct(compilers[i]);

#ifdef _WIN32
    if (delay)
        Sleep(1000000);
#endif

    if (compileFailed)
        return EFailCompile;
    if (linkFailed)
        return EFailLink;

    return 0;
}
