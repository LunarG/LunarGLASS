#version 130

uniform vec4 bigColor;
in vec4 BaseColor;

uniform int Count;
uniform uvec4 v4;

void main()
{
    vec4 color = BaseColor;

    for (int i = 0; i < Count; ++i) {
        color += bigColor;
    }

    gl_FragColor = color;

    float sum = 0.0;
    for (int i = 0; i < 4; ++i)
        sum += v4[i];

    vec4 tv4;

    for (int i = 0; i < 4; ++i)
        tv4[i] = v4[i] * 4u;

    gl_FragColor += vec4(sum) + tv4;
}
