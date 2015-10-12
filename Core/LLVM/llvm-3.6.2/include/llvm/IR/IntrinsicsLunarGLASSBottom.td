//===- IntrinsicsLunarGLASS.td - Defines LunarGLASS intrinsics -*- tablegen -*-===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
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
//===--------------------------------------------------------------------------===//
//
// This file defines all of the LunarGLASS intrinsics that are exclusive to
// Bottom IR.
//
//===--------------------------------------------------------------------------===//

// Pipeline ops
let TargetPrefix = "gla" in {  // All intrinsics start with "llvm.gla.".
  // writeDataComponent location, data, component
  def int_gla_writeComponent  : Intrinsic<[], [llvm_i32_ty, llvm_anyint_ty, llvm_i32_ty]>;
  def int_gla_fWriteComponent : Intrinsic<[], [llvm_i32_ty, llvm_anyfloat_ty, llvm_i32_ty]>;

  // writeInterpolantComponent location, flags, data, component
  def int_gla_fWriteInterpolantComponent : Intrinsic<[], [llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_i32_ty]>;

  // dst = loadComponent ptr, component
  def int_gla_loadComponent  : Intrinsic<[llvm_anyint_ty], [llvm_anyptr_ty, llvm_i32_ty], [IntrReadMem]>;
  def int_gla_fLoadComponent : Intrinsic<[llvm_anyfloat_ty], [llvm_anyptr_ty, llvm_i32_ty], [IntrReadMem]>;

  // dst = readComponent location, component
  def int_gla_readComponent  : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty], [IntrReadMem]>;
  def int_gla_fReadComponent : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty], [IntrReadMem]>;

  // dst = readInterpolantComponent location, flags, component
  def int_gla_readInterpolantComponent  : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty], [IntrReadMem]>;
  def int_gla_fReadInterpolantComponent : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty], [IntrReadMem]>;

  // dst = fReadInterpolantSampleComponent location, flags, sampleIdx, component
  def int_gla_fReadInterpolantSampleComponent : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_i32_ty], [IntrReadMem]>;

  // dst = fReadInterpolantOffsetComponent location, flags, offset, component
  def int_gla_fReadInterpolantOffsetComponent : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_i32_ty], [IntrReadMem]>;
}


// Bit Operations
let TargetPrefix = "gla" in {
  def int_gla_swizzle : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fSwizzle : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_multiInsert : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_i32_ty, llvm_anyint_ty, llvm_i32_ty, llvm_anyint_ty, llvm_i32_ty, llvm_anyint_ty, llvm_i32_ty, llvm_anyint_ty, llvm_i32_ty], [IntrNoMem]>;
  def int_gla_fMultiInsert : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_i32_ty], [IntrNoMem]>;
}

// Texture Sampling
let TargetPrefix = "gla" in {
  def int_gla_rTextureSample1 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRTextureSample1 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSample2 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRTextureSample2 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSample3 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRTextureSample3 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSample4 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRTextureSample4 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZ1 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZ1 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZ2 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZ2 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZ3 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZ3 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZ4 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZ4 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZOffset1 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZOffset1 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZOffset2 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZOffset2 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZOffset3 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZOffset3 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZOffset4 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZOffset4 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZOffsetGrad1 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZOffsetGrad1 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZOffsetGrad2 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZOffsetGrad2 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZOffsetGrad3 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZOffsetGrad3 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZOffsetGrad4 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZOffsetGrad4 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
}
