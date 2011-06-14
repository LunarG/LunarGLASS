#version 130

uniform sampler2D Image;
uniform vec4 DefaultColor;
uniform float Linear;
uniform vec4 Modulator;
uniform vec4 Ink1, Ink2;

in float Intensity;
in vec2 TexCoord;

void main()
{
    vec4 color;
	vec4 baseColor;

	baseColor = texture2D(Image, TexCoord);
	color = baseColor + (Ink1 + Ink2);

    if (Intensity > Linear)
	    color = Ink1 + Ink2 + color * baseColor;

    gl_FragColor = color * Modulator;
}
