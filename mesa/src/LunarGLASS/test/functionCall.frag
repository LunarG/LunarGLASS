uniform vec4 bigColor;
varying vec4 BaseColor;
uniform float d;

float foo(vec4 bar)
{
    return bar.x + bar.y;
}

void main()
{
    vec4 color = vec4(foo(BaseColor));
    
    gl_FragColor = color;
}
