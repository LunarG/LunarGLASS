#version 130

uniform sampler2D sampler;
varying vec2 coord;

struct s1 {
    int i;
    float f;
};

struct s2 {
    int i;
    float f;
	s1 s1_1;
};

struct s3 {
	s2 s2_1;
    int i;
    float f;
	s1 s1_1;
};


uniform s1 foo;
uniform s2 foo2;
uniform s3 foo3;

void main()
{
	s2 locals2;

	if (foo3.s2_1.i > 0) {
		locals2.f = 1.0;
		locals2.s1_1 = s1(0, 0.0);
	} else {
		locals2.f = coord.x;
		locals2.s1_1 = s1(1, coord.y);
	}

	gl_FragColor = locals2.s1_1.f * texture2D(sampler, coord);
}
