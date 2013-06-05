#version 300 es

struct S {
    vec4 u;
    uvec4 v;
    vec3 w;
};

uniform S s;

uniform fooBlock {
   vec4 bu;
   vec2 bw;
   isampler3D sampler;
   uvec4 bv;
   mat2 bm2;
};

uniform barBlock {
   vec4 nbu;
   mat3 nbm;
   uvec4 nbv;
   int ni;
} inst;

out vec4 color;

flat in int i;

void main()
{
    color = s.u + vec4(s.v) + vec4(bv) + vec4(inst.nbu);
}
