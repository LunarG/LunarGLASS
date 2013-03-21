#version 100

uniform mat4 mvp;

attribute vec4 v;
attribute mat3 am3;
//attribute mat4 arraym[3];

varying float f;

void main()
{
	gl_Position = mvp * v;
	f = am3[2][1]; // + arraym[1][2][3];
}
