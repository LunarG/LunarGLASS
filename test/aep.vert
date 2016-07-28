#version 310 es

#extension GL_ANDROID_extension_pack_es31a : enable

// GL_OES_shader_image_atomic
// GL_OES_texture_storage_multisample_2d_array
// GL_EXT_gpu_shader5
// GL_EXT_shader_io_blocks
// GL_EXT_texture_buffer
// GL_EXT_texture_cube_map_array

uniform sampler2D sArray[4];
uniform ubName { vec2 p; } ubInst[4];
uniform writeonly lowp image2D iArray[5];
uniform int index;

const ivec2 constOffsets[4] = ivec2[4](ivec2(1), ivec2(2), ivec2(3), ivec2(4));

in vec2 inf, ing, inch;

out outName {
    vec4 color;
} outInst;

vec4 bufferT();
vec4 CAT();
vec4 MSA();
void goodImageAtom();

void main()
{
    // GL_EXT_gpu_shader5
    precise vec2 p = fma(inf, ing, inch);
    iArray[index];
    vec4 color = vec4(0.0);
    color += textureGatherOffset(sArray[index], ubInst[index].p, ivec2(inf));
    color += textureGatherOffsets(sArray[index], p, constOffsets);
    outInst.color = color;
    outInst.color += bufferT();
    outInst.color += CAT();
    outInst.color += MSA();
    goodImageAtom();
}

// GL_EXT_texture_buffer

uniform highp samplerBuffer  bufSamp1;          
uniform highp isamplerBuffer bufSamp2;          
uniform highp usamplerBuffer bufSamp3;          
uniform highp writeonly imageBuffer    bufSamp4;
uniform highp writeonly iimageBuffer   bufSamp5;
uniform highp writeonly uimageBuffer   bufSamp6;

vec4 bufferT()
{
    vec4 v = vec4(1.0);

    highp int s1;
    s1 = textureSize(bufSamp1);
    v *= vec4(s1);
    s1 = textureSize(bufSamp2);
    v *= vec4(s1);
    s1 = textureSize(bufSamp3);
    v *= vec4(s1);

    s1 = imageSize(bufSamp4);
    v *= vec4(s1);
    s1 = imageSize(bufSamp5);
    v *= vec4(s1);
    s1 = imageSize(bufSamp6);
    v *= vec4(s1);
    
    v *= texelFetch(bufSamp1, s1);
    v *= vec4(texelFetch(bufSamp2, s1));
    v *= vec4(texelFetch(bufSamp3, s1));

    return v;
}

// GL_OES_texture_cube_map_array

uniform highp writeonly imageCubeArray  CA1;
uniform highp writeonly iimageCubeArray CA2;
uniform highp writeonly uimageCubeArray CA3;
uniform highp samplerCubeArray          CA4;
uniform highp samplerCubeArrayShadow    CA5;
uniform highp isamplerCubeArray         CA6;
uniform highp usamplerCubeArray         CA7;

vec4 CAT()
{
    highp ivec3 iv = ivec3(0);
    iv += textureSize(CA4, 1);
    iv += textureSize(CA5, 1);
    iv += textureSize(CA6, 1);
    iv += textureSize(CA7, 1);

    iv += imageSize(CA1);
    iv += imageSize(CA2);
    iv += imageSize(CA3);
    
    vec4 v = vec4(iv, 1.0);
    v *= texture(CA4, vec4(0.5));
    v *= texture(CA5, vec4(0.5), 3.0);
    v *= vec4(texture(CA6, vec4(0.5)));
    v *= vec4(texture(CA7, vec4(0.5)));

    v *= textureLod(CA4, vec4(0.5), 0.24);
    v *= vec4(textureLod(CA6, vec4(0.5), 0.26));
    v *= vec4(textureLod(CA7, vec4(0.5), 0.27));

    v *= textureGrad(CA4, vec4(0.5), vec3(0.1), vec3(0.2));
    v *= vec4(textureGrad(CA6, vec4(0.5), vec3(0.1), vec3(0.2)));
    v *= vec4(textureGrad(CA7, vec4(0.5), vec3(0.1), vec3(0.2)));

    v *= textureGather(CA4, vec4(0.5));
    v *= textureGather(CA4, vec4(0.5), 2);
    v *= vec4(textureGather(CA6, vec4(0.5)));
    v *= vec4(textureGather(CA6, vec4(0.5), 1));
    v *= vec4(textureGather(CA7, vec4(0.5)));
    v *= vec4(textureGather(CA7, vec4(0.5), 0));

    v *= textureGather(CA5, vec4(0.5), 2.5);

    return v;
}

// GL_OES_texture_storage_multisample_2d_array

uniform highp sampler2DMSArray  samp2DMSA;
uniform highp isampler2DMSArray samp2DMSAi;
uniform highp usampler2DMSArray samp2DMSAu;

vec4 MSA()
{
    highp ivec3 iv = ivec3(0);
    iv += textureSize(samp2DMSA);
    iv += textureSize(samp2DMSAi);
    iv += textureSize(samp2DMSAu);
    
    vec4 v = vec4(iv, 1.0);
    v *= texelFetch(samp2DMSA, ivec3(5), 2);
    v *= vec4(texelFetch(samp2DMSAi, ivec3(5), 2));
    v *= vec4(texelFetch(samp2DMSAu, ivec3(5), 2));

    return v;
}

// GL_OES_shader_image_atomic

uniform layout(r32f)  highp  image2D im2Df;
uniform layout(r32ui) highp uimage2D im2Du;
uniform layout(r32i)  highp iimage2D im2Di;
uniform ivec2 P;

void goodImageAtom()
{
    float datf = 1.8;
    int dati = 4;
    uint datu = 7u;

    imageAtomicAdd(     im2Di, P, P.x);
    imageAtomicAdd(     im2Du, P, datu);
    imageAtomicMin(     im2Di, P, dati);
    imageAtomicMin(     im2Du, P, datu);
    imageAtomicMax(     im2Di, P, dati);
    imageAtomicMax(     im2Du, P, datu);
    imageAtomicAnd(     im2Di, P, dati);
    imageAtomicAnd(     im2Du, P, datu);
    imageAtomicOr(      im2Di, P, dati);
    imageAtomicOr(      im2Du, P, datu);
    imageAtomicXor(     im2Di, P, dati);
    imageAtomicXor(     im2Du, P, datu);
    imageAtomicExchange(im2Di, P, dati);
    imageAtomicExchange(im2Du, P, datu);
    imageAtomicExchange(im2Df, P, datf);
    imageAtomicCompSwap(im2Di, P,  3, dati);
    imageAtomicCompSwap(im2Du, P, 5u, datu);
}
