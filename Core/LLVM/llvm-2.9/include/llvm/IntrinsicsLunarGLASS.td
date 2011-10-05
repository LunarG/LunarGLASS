//===- IntrinsicsLunarGLASS.td - Defines LunarGLASS intrinsics -*- tablegen -*-===//
//
// LunarGLASS: An Open Modular Shader Compiler Architecture
// Copyright (C) 2010-2011 LunarG, Inc.
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
// This file defines all of the LunarGLASS intrinsics.
//
//===--------------------------------------------------------------------------===//

// Pipeline ops
let TargetPrefix = "gla" in {  // All intrinsics start with "llvm.gla.".
  def int_gla_readData : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty], [IntrReadMem]>;
  def int_gla_fReadData : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty], [IntrReadMem]>;
  def int_gla_fWriteInterpolant : Intrinsic<[], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty]>;
  def int_gla_fReadInterpolant : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty], [IntrNoMem]>;
  def int_gla_fReadInterpolantOffset : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty], [IntrReadMem]>;
  def int_gla_getInterpolant : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty], [IntrReadMem]>;
  def int_gla_writeData : Intrinsic<[], [llvm_i32_ty, llvm_i32_ty, llvm_anyint_ty]>;
  def int_gla_fWriteData : Intrinsic<[], [llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty]>;
}

// Floating-Point and Integer Operations
let TargetPrefix = "gla" in {
  def int_gla_abs : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fAbs : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_sMin : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_uMin : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fMin : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_sMax : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_uMax : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fMax : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_sClamp : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_uClamp : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fClamp : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
}

// Floating-Point Only Operations
let TargetPrefix = "gla" in {
  def int_gla_fRadians : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fDegrees : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fSin : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fCos : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fTan : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fAsin : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fAcos : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fAtan : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fAtan2 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fSinh : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fCosh : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fTanh : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fAsinh : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fAcosh : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fAtanh : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fPow : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fPowi : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fExp : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fLog : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fExp2 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fLog2 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fExp10 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fLog10 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fSqrt : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fInverseSqrt : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fSign : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fFloor : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fCeiling : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRoundEven : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRoundFast : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fFraction : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fModF : Intrinsic<[llvm_anyfloat_ty, llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fMix : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fStep : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fSmoothStep : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fIsNan : Intrinsic<[llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fIsInf : Intrinsic<[llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fFma : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
}

// Integer-Only Operations
let TargetPrefix = "gla" in {
  def int_gla_addCarry : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty], [llvm_anyint_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_subBorrow : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty], [llvm_anyint_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_umulExtended : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty], [llvm_anyint_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_smulExtended : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty], [llvm_anyint_ty, llvm_anyint_ty], [IntrNoMem]>;
}

// Value Preserving Conversions
let TargetPrefix = "gla" in {
}

// Bit Operations
let TargetPrefix = "gla" in {
  def int_gla_fFloatBitsToInt : Intrinsic<[llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fIntBitsTofloat : Intrinsic<[llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_sBitFieldExtract : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_i32_ty, llvm_i32_ty], [IntrNoMem]>;
  def int_gla_uBitFieldExtract : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_i32_ty, llvm_i32_ty], [IntrNoMem]>;
  def int_gla_bitFieldInsert : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty], [llvm_anyint_ty, llvm_i32_ty, llvm_i32_ty], [IntrNoMem]>;
  def int_gla_bitReverse : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_bitCount : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_findLSB : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_sFindMSB : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_uFindMSB : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_swizzle : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fSwizzle : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_multiInsert : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_i32_ty, llvm_anyint_ty, llvm_i32_ty, llvm_anyint_ty, llvm_i32_ty, llvm_anyint_ty, llvm_i32_ty, llvm_anyint_ty, llvm_i32_ty], [IntrNoMem]>;
  def int_gla_fMultiInsert : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_i32_ty], [IntrNoMem]>;
}

// Pack and Unpack
let TargetPrefix = "gla" in {
  def int_gla_fFrexp : Intrinsic<[llvm_anyfloat_ty, llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fLdexp : Intrinsic<[llvm_anyfloat_ty, llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fPackUnorm2x16 : Intrinsic<[llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fPackUnorm4x8 : Intrinsic<[llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fPackSnorm4x8 : Intrinsic<[llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fUnpackUnorm2x16 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fUnpackUnorm4x8 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fUnpackSnorm4x8 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fPackDouble2x32 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fUnpackDouble2x32 : Intrinsic<[llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
}

// Texture Sampling
let TargetPrefix = "gla" in {
  def int_gla_textureSample : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fTextureSample : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSample1 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSample1 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_rTextureSample2 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSample2 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_rTextureSample3 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSample3 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_rTextureSample4 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSample4 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_textureSampleLodRefZ : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty, llvm_i32_ty], [IntrNoMem]>;
  def int_gla_fTextureSampleLodRefZ : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty, llvm_float_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZ1 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZ1 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZ2 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZ2 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZ3 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZ3 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZ4 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZ4 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_textureSampleLodRefZOffset : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty, llvm_i32_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fTextureSampleLodRefZOffset : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty, llvm_float_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZOffset1 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZOffset1 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZOffset2 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZOffset2 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZOffset3 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZOffset3 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZOffset4 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZOffset4 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_textureSampleLodRefZOffsetGrad : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty, llvm_i32_ty, llvm_anyint_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fTextureSampleLodRefZOffsetGrad : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty, llvm_float_ty, llvm_anyint_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZOffsetGrad1 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZOffsetGrad1 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZOffsetGrad2 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZOffsetGrad2 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZOffsetGrad3 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZOffsetGrad3 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodRefZOffsetGrad4 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodRefZOffsetGrad4 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_texelFetchOffset : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fTexelFetchOffset : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_texelGather : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fTexelGather : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_texelGatherOffset : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fTexelGatherOffset : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_texelGatherOffsets : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fTexelGatherOffsets : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_anyint_ty, llvm_anyfloat_ty], [IntrNoMem]>;
}

// Texture Query
let TargetPrefix = "gla" in {
  def int_gla_queryTextureSize : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty], [IntrNoMem]>;
  def int_gla_fQueryTextureLod : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty], [IntrNoMem]>;
}

// Geometry
let TargetPrefix = "gla" in {
  def int_gla_fLength : Intrinsic<[llvm_float_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fDistance : Intrinsic<[llvm_float_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fDot2 : Intrinsic<[llvm_float_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fDot3 : Intrinsic<[llvm_float_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fDot4 : Intrinsic<[llvm_float_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fCross : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fNormalize : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fNormalize3D : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fLit : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fFaceForward : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fReflect : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRefract : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
}

// Derivative and Transform
let TargetPrefix = "gla" in {
  def int_gla_fDFdx : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fDFdy : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fFilterWidth : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fFixedTransform : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
}

// Comparison
let TargetPrefix = "gla" in {
}

// Vector Logical
let TargetPrefix = "gla" in {
  def int_gla_not : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_any : Intrinsic<[llvm_i1_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_all : Intrinsic<[llvm_i1_ty], [llvm_anyint_ty], [IntrNoMem]>;
}

// Flow control
let TargetPrefix = "gla" in {
  def int_gla_discard : Intrinsic<[], [], []>;
  def int_gla_discardConditional : Intrinsic<[], [llvm_anyint_ty], []>;
}
