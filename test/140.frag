#version 140

in vec4 i;
out vec4 o;

in float gl_ClipDistance[5];

layout(row_major) uniform;

uniform sampler2D samp2Da[3];

layout(std140) uniform bn {
    layout(row_major) mat4 matra[4];
    layout(column_major) mat4 matca[4];
    layout(row_major) mat4 matr;
    layout(column_major) mat4 matc;
    mat4 matrdef;
};

void main()
{
    o.y = gl_ClipDistance[2];
    o.z = gl_ClipDistance[int(i)];
}
