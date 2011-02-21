uniform float d;
uniform vec4 bigColor, smallColor;
uniform vec4 otherColor;

varying float c;
varying vec4 BaseColor;

void main()
{
    vec4 color = BaseColor;
	vec4 color2;

	color2 = otherColor;
/*
	if (c > d*d) {
		if (c > d) {
			color += bigColor;
			color2 = smallColor;
		} else {
			color += smallColor;
		}
	} else {
		if (c > d) {
			color += bigColor;
			color2 = smallColor;
		} else {
			color += smallColor;
		}
    }
*/
    while (c > d) {
        color += bigColor;
    }

    gl_FragColor = color * color2;
}
