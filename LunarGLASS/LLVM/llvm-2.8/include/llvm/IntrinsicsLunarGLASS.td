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
  def int_gla_getInterpolant : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty], [IntrReadMem]>; 
  def int_gla_writeData : Intrinsic<[], [llvm_i32_ty, llvm_anyfloat_ty]>; 
  def int_gla_writeDepth : Intrinsic<[], [llvm_anyfloat_ty]>; 
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
  def int_gla_swizzle : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_i32_ty], [IntrNoMem]>;
  def int_gla_fSwizzle : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_i32_ty], [IntrNoMem]>;
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
  def int_gla_textureSampleLod : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty], [IntrNoMem]>;
  def int_gla_fTextureSampleLod : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLod1 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLod1 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLod2 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLod2 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLod3 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLod3 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLod4 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLod4 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_textureSampleLodOffset : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fTextureSampleLodOffset : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodOffset1 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodOffset1 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodOffset2 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodOffset2 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodOffset3 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodOffset3 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodOffset4 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodOffset4 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_textureSampleLodOffsetGrad : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty, llvm_anyint_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fTextureSampleLodOffsetGrad : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty, llvm_anyint_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodOffsetGrad1 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodOffsetGrad1 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodOffsetGrad2 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodOffsetGrad2 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodOffsetGrad3 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodOffsetGrad3 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_rTextureSampleLodOffsetGrad4 : Intrinsic<[llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
  def int_gla_fRTextureSampleLodOffsetGrad4 : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyfloat_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty, llvm_anyvector_ty], [IntrNoMem]>;
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
  def int_gla_fDot : Intrinsic<[llvm_float_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
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
