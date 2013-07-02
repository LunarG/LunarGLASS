#version 100

uniform int low, high;

void main()
{
	int z = 3;

	if (2 * low + 1 < high)
		++z;

	gl_FragColor = vec4(z);
}
