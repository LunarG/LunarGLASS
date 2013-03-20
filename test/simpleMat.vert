#version 100

uniform mat4 mvp;

attribute vec4 v;

void main()
{
	gl_Position = mvp * v;
}