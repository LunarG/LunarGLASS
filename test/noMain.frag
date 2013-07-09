#version 330

uniform bool B;
uniform float a;

float b = B ? sin(a) : tan(a);

float foo()
{
    return b;
}

float c = cos(b);
