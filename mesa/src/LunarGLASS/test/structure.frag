#version 130

uniform sampler2D sampler;
varying vec2 coord;

struct s {
    int i;
    float f;
};

uniform s foo;

void main()
{
    gl_FragColor = foo.f * texture2D(sampler, coord);
}
