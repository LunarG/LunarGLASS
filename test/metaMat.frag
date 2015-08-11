#version 300 es
precision highp float;

struct S1 {
    uint i;
    mat2x3 S1m;
};

struct S2 {
    uint i;
    mat2x3 S2m;
};

struct Si1 {
    float f;
    S1 s1;
};

struct Si2 {
    float g;
    S2 s2;
};

uniform BNA {
    mat3x4 BNAm;
    Si1 si1;
} BNAI;

layout (row_major) uniform BNB {
    mat3 BNBm3;
    Si1 si1;
    layout(column_major) Si2 si21;
    layout(row_major) Si2 si22;
    layout(column_major) mat4 BNBm41;
    mat4 BNBm42;
} BNBI;

void main()
{
}