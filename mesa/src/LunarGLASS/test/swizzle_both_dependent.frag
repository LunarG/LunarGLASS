uniform float blend;
uniform vec4 u;
uniform vec4 u2;
uniform vec4 u3;

varying vec2 t;

void main()
{
    float blendscale = 1.789;

    vec4 w = u;
    vec4 w2 = u2;
    vec4 w3 = u3;
    vec4 w4 = u;
    vec4 w5 = u;

    w.x = blendscale;
    w.y = blendscale;
    w.z = blendscale;

    w2.wy = t;

    w3.zw = t;

    w4.xz = w.yz;

    w5.xy = w2.xy;
    w5.zw = w3.yz;

    mix(w2, w3, w4);
    gl_FragColor = mix(w * w2, w3, w4 * w5);
}
