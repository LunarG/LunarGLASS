uniform mat3 colorTransform;
varying vec3 Color;

void main()
{
    gl_FragColor = vec4(Color * colorTransform, 1.0);
}
