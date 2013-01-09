#version 130

varying vec4 Color;
varying float Depth;

out vec4 foo;

void main()
{
    gl_FragDepth = Depth;
    gl_FragColor = Color;
    foo = Color;
}
