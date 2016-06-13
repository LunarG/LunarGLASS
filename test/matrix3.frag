#version 300 es

uniform highp mat3 a;
uniform highp mat2x3 b;
uniform highp vec2 d;
out highp vec3 o;

void main (void)
{
  highp mat2x3 c = a * b;
  o = c * d;
}
