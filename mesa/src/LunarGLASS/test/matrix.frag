uniform mat3 colorTransform;
varying vec3 Color;
uniform mat4 m, n;
varying vec4 v;

void main()
{
    gl_FragColor = vec4(Color * colorTransform, 1.0);

    if (m != n)
        gl_FragColor += m * v;
}
