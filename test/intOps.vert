#version 310 es

in uint u1;
in uvec2 u2;
in uvec3 u3;
in uvec4 u4;

in float v1;
in vec2 v2;
in vec3 v3;
in vec4 v4;

in int i1;
in ivec2 i2;
in ivec3 i3;
in ivec4 i4;

out uvec4 uout;
out ivec4 iout;
out vec4 fout;

void main()
{
    iout = ivec4(0);
    uout = uvec4(0);
    fout = vec4(0.0);

    uvec2 u2out;
    uout.xy += uaddCarry(u2, u2, u2out);
    uout.xy += u2out;

    uint u1out;
    uout.x += usubBorrow(u1, u1, u1out);
    uout.x += u1out;

    uvec4 u4out;
    umulExtended(u4, u4, u4out, u4out);
    uout += u4out;

    ivec4 i4out1, i4out2;
    imulExtended(i4, i4, i4out1, i4out2);
    iout += i4out1 + i4out2;

    iout.x += bitfieldExtract(i1, 4, 5);
    uout.xyz += bitfieldExtract(u3, 4, 5);
    iout.xyz += bitfieldInsert(i3, i3, 4, 5);
    uout.x += bitfieldInsert(u1, u1, 4, 5);
    iout.xy += bitfieldReverse(i2);
    uout += bitfieldReverse(u4);
    iout.x += bitCount(i1);
    iout.xyz += bitCount(u3);
    iout.xy += findLSB(i2);
    iout += findLSB(u4);
    iout.x += findMSB(i1);
    iout.xy += findMSB(u2);

    ivec3 i3out;
    fout.xyz += frexp(v3, i3out);
    iout.xyz += i3out;
    int i1out;
    fout.x += frexp(v1, i1out);
    iout.x += i1out;

    fout.xy += ldexp(v2, i2);
    fout.x += ldexp(v1, i1);

    uout.x += packUnorm4x8(v4);
    uout.x += packSnorm4x8(v4);
    fout += unpackUnorm4x8(u1);
    fout += unpackSnorm4x8(u1);
}
