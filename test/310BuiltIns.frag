#version 310 es

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

    // 8.3 int
    sum += vec4(mix(iv3a, iv3b, bv), 0.0);

    result = sum + vec4(isum) + vec4(usum);

    result += gl_FragCoord + vec4(gl_PointCoord, 2.0 * gl_PointCoord);
    gl_FragDepth = f;
    gl_FragDepth += 0.1;
}
