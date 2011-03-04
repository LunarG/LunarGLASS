#version 130

uniform sampler2D sampler;
varying vec2 coord;

void main()
{
    gl_FragColor = texture(sampler, coord);
}
