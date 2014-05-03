#version 150 core

in vec4 iv4;

uniform float ps;
uniform int ui;

invariant gl_Position;

struct s1 {
    int a;
    int a2;
    vec4 b[3];
};

struct s2 {
    int c;
    s1 d[4];
};

out s2 s2out;

void main()
{
    gl_Position = iv4;
    gl_PointSize = ps;
    gl_ClipDistance[2] = iv4.x;

    s2out.d[ui].b[2].w = ps;
}

out float gl_ClipDistance[4];
