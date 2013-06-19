#version 120

uniform vec4 bigColor;
varying vec4 BaseColor;

uniform int Count;

void main()
{
    vec4 color = BaseColor;

    for (int i = 0; i < Count; ++i) {
        color += bigColor;
    }

    gl_FragColor = color;
}
