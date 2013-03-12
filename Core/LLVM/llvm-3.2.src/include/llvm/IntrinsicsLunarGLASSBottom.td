//===- IntrinsicsLunarGLASS.td - Defines LunarGLASS intrinsics -*- tablegen -*-===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (C) 2010-2012 LunarG, Inc.
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; version 2 of the
// License.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301, USA.
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
