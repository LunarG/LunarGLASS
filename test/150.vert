#version 150 core

in vec4 iv4;

uniform float ps;

invariant gl_Position;

void main()
{
    gl_Position = iv4;
    gl_PointSize = ps;
    gl_ClipDistance[2] = iv4.x;
}

out float gl_ClipDistance[4];
