#version 310 es

precision highp float;

#extension GL_ANDROID_extension_pack_es31a : enable

// - GL_KHR_blend_equation_advanced
// - GL_OES_sample_variables
// - GL_OES_shader_multisample_interpolation
// - GL_EXT_geometry_shader
// + GL_EXT_shader_io_blocks

in inName {
    vec4 color;
} inInst;

//layout(location = 0) out vec4 color;  // - more than one out, if no location?

void foo_GS();
void goodSample();

void main()
{
// -    color = inInst.color;
    foo_GS();
    goodSample();
}

// - GL_OES_geometry_shader

void foo_GS()
{
    highp int l = gl_Layer;             // - consume both these ...
    highp int p = gl_PrimitiveID;
}


// - GL_OES_sample_variables

void goodSample()
{
    lowp     int  a1 = gl_SampleID;        // - consume all these built-ins...
    mediump  vec2 a2 = gl_SamplePosition; 
    highp    int  a3 = gl_SampleMaskIn[0];
    gl_SampleMask[0] = a3;
    mediump int n1 = gl_MaxSamples;
    mediump int n2 = gl_NumSamples;
}
