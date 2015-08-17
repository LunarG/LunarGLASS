#version 310 es

#extension GL_ANDROID_extension_pack_es31a : enable

// - GL_EXT_tessellation_shader
// + GL_EXT_tessellation_point_size

layout(quads, ccw) in;  // - no ccw

layout(fractional_odd_spacing) in;    

layout(point_mode) in;

patch in vec4 patchIn;

void pointSize();

void main()
{
    vec4 p = gl_in[1].gl_Position;

    int pvi = gl_PatchVerticesIn;
    int pid = gl_PrimitiveID;
    vec3 tc = gl_TessCoord;
    float tlo = gl_TessLevelOuter[3];
    float tli = gl_TessLevelInner[1];

    gl_Position = p;
    pointSize();
}

in vec2 inb[];
in vec2 ind[gl_MaxPatchVertices];

in testblb {
    int f;
} blb[];

in testbld {
    int f;
} bld[gl_MaxPatchVertices];

layout(location = 23) in vec4 ivla[];
layout(location = 24) in vec4 ivlb[];

layout(location = 23) out vec4 ovla[2];

patch in pinbn {
    int a;
} pinbi;

centroid out vec3 myColor2;
centroid in vec3 centr[];

#extension GL_OES_tessellation_point_size : enable

void pointSize()
{
    gl_PointSize = gl_in[1].gl_PointSize;
}
