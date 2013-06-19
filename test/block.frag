#version 300 es

precision mediump float;

struct S {
    vec4 u;
    uvec4 v;
    lowp isampler2D sampler;
    vec3 w;
    mat3 m3a[5];
};

uniform S s;

uniform fooBlock {
    vec4 bu;
    vec2 bw;
    uvec4 bv;
    S bs;
    mat2 bm2;
    mat4 m4a[8];
};

uniform barBlock {
    vec4 nbu;
    mat3 nbm;
    uvec4 nbv;
    int ni;
} inst[4];

out vec4 color;

void main()
{
   color = vec4(texture(s.sampler, bw)) + inst[3].nbu + bu;
}
