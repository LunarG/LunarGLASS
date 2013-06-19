#version 300 es

precision mediump float;

uniform int imax, imin;
uniform uint umax, umin;

in vec3 x, y;
uniform bvec3 bv;

flat in uint uy;
flat in uvec2 uv2c;
flat in uvec2 uv2y;
flat in uvec2 uv2x;
flat in uvec4 uv4y;

flat in ivec3 iv3a;
flat in ivec3 iv3b;

flat in ivec4 iv4a;
flat in ivec4 iv4b;

in float f;

in vec2 v2a, v2b;

in vec4 v4;

out vec4 result;

void main()
{
    uvec4 usum = uvec4(0);
    ivec4 isum = ivec4(0);
    vec4 sum = vec4(0);

    // 1.3 int
    sum += vec4(mix(x, y, bv), 0.0);

    isum = abs(iv4a);
    isum += sign(iv4a);

    isum += min(iv4a, iv4b);    
    isum += min(iv4a, imin);    
    uvec2 u = min(uv2x, uv2y);
    usum += uvec4(u, 0, 0);
    usum += min(uv4y, uy);

    ivec3 i17 = max(iv3a, iv3b);
    isum += ivec4(i17, 0);
    isum += max(iv4a, imax);
    uvec2 u10 = max(uv2x, uv2y);
    uvec2 u11 = max(uv2x, uy);
    usum += uvec4(u10, u11);
    
    isum += clamp(iv4a, iv4a, iv4b);
    isum += clamp(iv4a, imin, imax);
    uvec2 u12 = clamp(uv2x, uv2y, uv2c);
    usum += uvec4(u12, 0, 0);
    usum += clamp(uv4y, umin, umax);
    
    // 1.3 float
//    vec3 modfOut;
//??    vec3 v11 = modf(x, modfOut);
//    sum += vec4(v11, modfOut);

    sum += vec4(trunc(f));
    vec2 v12 = round(v2a);
    vec2 v13 = roundEven(v2a);
    sum += vec4(v12, v13);
    bvec2 b10 = isnan(v2a);
    bvec4 b11 = isinf(v4);
    isum += ivec4(b10, b11);

    // 3.3 float
    int i = floatBitsToInt(f);
    isum += ivec4(i);
    usum += floatBitsToUint(v4);
    sum += intBitsToFloat(iv4a);
    vec2 v15 = uintBitsToFloat(uv2c);
    sum += vec4(v15, 0, 0);

    // 4.0  pack
    uint u19 = packSnorm2x16(v2a);
    vec2 v20 = unpackSnorm2x16(uy);
    uint u15 = packUnorm2x16(v2a);
    vec2 v16 = unpackUnorm2x16(uy);
    uint u17 = packHalf2x16(v2b);
    vec2 v18 = unpackHalf2x16(uy);

    usum += uvec4(u19, u15, u17, 0);
    sum += vec4(v20, v16);
    sum += vec4(v18, 0, 0);

    result = sum + vec4(isum) + vec4(usum);

    result += gl_FragCoord + vec4(gl_PointCoord, 2.0 * gl_PointCoord);
    gl_FragDepth = f;
    gl_FragDepth += 0.1;
}
