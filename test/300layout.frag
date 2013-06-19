#version 300 es

precision mediump float;

in vec4 pos;
in vec3 color;

layout(location = 7) out vec3 c;
layout(LocatioN = 3) out vec4 p[2];

void main()
{
    c = color;
    p[1] = pos;
}
