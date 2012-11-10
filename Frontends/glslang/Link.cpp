//===- Link.cpp - Translate GLSL IR to LunarGLASS Top IR -----------------===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (C) 2012 LunarG, Inc.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice (including the next
// paragraph) shall be included in all copies or substantial portions of the
// Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//
//===----------------------------------------------------------------------===//
//
// Author: John Kessenich, LunarG
//
// Stubs for glslang link step.
//
//===----------------------------------------------------------------------===//

#include "../glslang/Include/Common.h"
#include "../glslang/Include/ShHandle.h"

//
// Actual link object, derived from the shader handle base classes.
//
class TGenericLinker : public TLinker {
public:
    TGenericLinker(EShExecutable e, int dOptions) : TLinker(e, infoSink), debugOptions(dOptions) { }
    bool link(TCompilerList&, TUniformMap*) { return true; }
	void getAttributeBindings(ShBindingTable const **t) const { }
    TInfoSink infoSink;
    int debugOptions;
};

//
// The internal view of a uniform/float object exchanged with the driver.
//
class TUniformLinkedMap : public TUniformMap {
public:
    TUniformLinkedMap() { }
    virtual int getLocation(const char* name) { return 0; }
};

TShHandleBase* ConstructLinker(EShExecutable executable, int debugOptions)
{
    return new TGenericLinker(executable, debugOptions);
}

void DeleteLinker(TShHandleBase* linker)
{
    delete linker;
}

TUniformMap* ConstructUniformMap()
{
    return new TUniformLinkedMap();
}

void DeleteUniformMap(TUniformMap* map)
{
    delete map;
}

TShHandleBase* ConstructBindings()
{
    return 0;
}

void DeleteBindingList(TShHandleBase* bindingList)
{
    delete bindingList;
}
