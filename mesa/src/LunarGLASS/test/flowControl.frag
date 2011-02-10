uniform float d;
uniform vec4 bigColor, smallColor;

varying float c;
varying vec4 BaseColor;

void main()
{
    vec4 color = BaseColor;

    if (c > d) {
		color += bigColor;
	} else {
	    color += smallColor;
	}

    gl_FragColor = color;
}
