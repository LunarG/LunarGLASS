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
// This file defines all of the LunarGLASS intrinsics that are not exclusive to
// Bottom IR.
//
//===--------------------------------------------------------------------------===//

// Pipeline ops
let TargetPrefix = "gla" in {  // All intrinsics start with "llvm.gla.".
  def int_gla_readData : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty], [IntrReadMem]>;
  def int_gla_fReadData : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty], [IntrReadMem]>;
  def int_gla_fWriteInterpolant : Intrinsic<[], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty]>;
  def int_gla_fReadInterpolant : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty], [IntrNoMem]>;
  def int_gla_fReadInterpolantSample : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_i32_ty], [IntrReadMem]>;
  def int_gla_fReadInterpolantOffset : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty], [IntrReadMem]>;
  def int_gla_getInterpolant : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty], [IntrReadMem]>;
  def int_gla_writeData : Intrinsic<[], [llvm_i32_ty, llvm_i32_ty, llvm_anyint_ty]>;
  def int_gla_fWriteData : Intrinsic<[], [llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty]>;

  def int_gla_fSamplePosition : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty], [IntrReadMem]>;
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

  // fSaturate x === fClamp x, 0, 1
  def int_gla_fSaturate : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
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
  def int_gla_sign : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fSign : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fFloor : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fCeiling : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRoundEven : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRoundZero : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fRoundFast : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fFraction : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fModF : Intrinsic<[llvm_anyfloat_ty, llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fMix : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fbMix : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty, llvm_anyint_ty], [IntrNoMem]>;
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
  def int_gla_uFma : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_sFma : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_anyint_ty, llvm_anyint_ty], [IntrNoMem]>;
}

// Value Preserving Conversions
let TargetPrefix = "gla" in {
}

// Bit Operations
let TargetPrefix = "gla" in {
  def int_gla_fFloatBitsToInt : Intrinsic<[llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fIntBitsTofloat : Intrinsic<[llvm_anyfloat_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_sBitFieldExtract : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_i32_ty, llvm_i32_ty], [IntrNoMem]>;
  def int_gla_uBitFieldExtract : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_i32_ty, llvm_i32_ty], [IntrNoMem]>;
  def int_gla_bitFieldInsert : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty, llvm_anyint_ty, llvm_i32_ty, llvm_i32_ty], [IntrNoMem]>;
  def int_gla_bitReverse : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_bitCount : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_findLSB : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_sFindMSB : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_uFindMSB : Intrinsic<[llvm_anyint_ty], [llvm_anyint_ty], [IntrNoMem]>;
}

// Pack and Unpack
let TargetPrefix = "gla" in {
  def int_gla_fFrexp : Intrinsic<[llvm_anyfloat_ty, llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fLdexp : Intrinsic<[llvm_anyfloat_ty, llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fPackUnorm2x16 : Intrinsic<[llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fPackSnorm2x16 : Intrinsic<[llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fPackUnorm4x8 : Intrinsic<[llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fPackSnorm4x8 : Intrinsic<[llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fUnpackUnorm2x16 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fUnpackSnorm2x16 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fUnpackUnorm4x8 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fUnpackSnorm4x8 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fPackHalf2x16 : Intrinsic<[llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fUnpackHalf2x16 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fPackDouble2x32 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fUnpackDouble2x32 : Intrinsic<[llvm_anyint_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
}

// Texture Sampling
let TargetPrefix = "gla" in {
  def int_gla_textureSample : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fTextureSample : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_textureSampleLodRefZ : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty, llvm_float_ty], [IntrNoMem]>;
  def int_gla_fTextureSampleLodRefZ : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty, llvm_float_ty], [IntrNoMem]>;
  def int_gla_textureSampleLodRefZOffset : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty, llvm_float_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fTextureSampleLodRefZOffset : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty, llvm_float_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_textureSampleLodRefZOffsetGrad : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty, llvm_float_ty, llvm_anyint_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fTextureSampleLodRefZOffsetGrad : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_float_ty, llvm_float_ty, llvm_anyint_ty, llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_texelFetchOffset : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_float_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_fTexelFetchOffset : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyint_ty, llvm_anyint_ty, llvm_float_ty, llvm_anyint_ty], [IntrNoMem]>;
  def int_gla_texelGather :         Intrinsic<[llvm_anyint_ty],   [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_i32_ty, llvm_float_ty], [IntrNoMem]>;
  def int_gla_fTexelGather :        Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_i32_ty, llvm_float_ty], [IntrNoMem]>;
  def int_gla_texelGatherOffset :   Intrinsic<[llvm_anyint_ty],   [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_i32_ty, llvm_float_ty, llvm_v2i32_ty], [IntrNoMem]>;
  def int_gla_fTexelGatherOffset :  Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_i32_ty, llvm_float_ty, llvm_v2i32_ty], [IntrNoMem]>;
  def int_gla_texelGatherOffsets :  Intrinsic<[llvm_anyint_ty],   [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_i32_ty, llvm_float_ty, llvm_v2i32_ty, llvm_v2i32_ty, llvm_v2i32_ty, llvm_v2i32_ty], [IntrNoMem]>;
  def int_gla_fTexelGatherOffsets : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty, llvm_i32_ty, llvm_float_ty, llvm_v2i32_ty, llvm_v2i32_ty, llvm_v2i32_ty, llvm_v2i32_ty], [IntrNoMem]>;
}

// Texture Query
let TargetPrefix = "gla" in {
  def int_gla_queryTextureSize : Intrinsic<[llvm_anyint_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty], [IntrNoMem]>;
  def int_gla_fQueryTextureLod : Intrinsic<[llvm_anyfloat_ty], [llvm_i32_ty, llvm_i32_ty, llvm_anyfloat_ty], [IntrNoMem]>;
}

// Geometry
let TargetPrefix = "gla" in {
  def int_gla_fLength : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fDistance : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fDot2 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fDot3 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
  def int_gla_fDot4 : Intrinsic<[llvm_anyfloat_ty], [llvm_anyfloat_ty, llvm_anyfloat_ty], [IntrNoMem]>;
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
