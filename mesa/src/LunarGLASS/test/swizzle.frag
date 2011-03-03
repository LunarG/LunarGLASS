uniform float blend;
uniform vec4 u;

varying vec2 t;

void main()
{  
    float blendscale = 1.789;

    vec4 w = u;

    w.wy = t;
    
    gl_FragColor = mix(w, u, blend * blendscale);
}
