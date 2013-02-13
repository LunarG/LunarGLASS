#version 120

struct s {
    int count;
	mat4x3 sm[12];
};

uniform s us;
uniform mat4x3 am[8];
uniform int i;

varying vec3 v;

uniform mat4x3 m;

void main()
{
    s ls;
    ls.sm[3] = us.sm[i] + am[4];

	gl_FragColor = v * ls.sm[3];
}
