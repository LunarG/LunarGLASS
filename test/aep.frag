#version 310 es

precision highp float;

#extension GL_ANDROID_extension_pack_es31a : enable

// - GL_KHR_blend_equation_advanced
// + GL_OES_sample_variables
// - GL_OES_shader_multisample_interpolation
// + GL_EXT_geometry_shader
// + GL_EXT_shader_io_blocks

in inName {
    vec4 color;
} inInst;

out vec4 color;

int foo_GS();
void goodSample();

void main()
{
    color = inInst.color;
    color *= float(foo_GS());
    goodSample();
}

// GL_OES_geometry_shader

highp int foo_GS()
{
    highp int l = gl_Layer;
    highp int p = gl_PrimitiveID;
    return l + p;
}


// GL_OES_sample_variables

void goodSample()
{
    lowp     int  a1 = gl_SampleID;
    mediump  vec2 a2 = gl_SamplePosition; 
    highp    int  a3 = gl_SampleMaskIn[0];
    gl_SampleMask[0] = a3;
    mediump int n1 = gl_MaxSamples;
    mediump int n2 = gl_NumSamples;
    gl_SampleMask[0] += n1 + n2 + a1;
    color.xy += a2;
}
