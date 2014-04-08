#version 140

in vec4 i;
out vec4 o;

in float gl_ClipDistance[5];

void main()
{
    o.y = gl_ClipDistance[2];
    o.z = gl_ClipDistance[int(i)];
}
