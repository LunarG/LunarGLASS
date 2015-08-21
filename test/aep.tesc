#version 310 es

#extension GL_ANDROID_extension_pack_es31a : enable

// GL_EXT_tessellation_shader: patch
// GL_EXT_tessellation_point_size
// GL_OES_primitive_bounding_box

layout(vertices = 4) out;
int outa[gl_out.length()];

patch out vec4 patchOut;

void bb();
void pointSize();
float foop();

void main()
{
    barrier();

    vec4 p = gl_in[1].gl_Position;

    int pvi = gl_PatchVerticesIn;
    int pid = gl_PrimitiveID;
    int iid = gl_InvocationID;

    gl_out[1].gl_Position = p;

    gl_TessLevelOuter[3] = 3.2;
    gl_TessLevelInner[1] = 1.3;
    bb();
    pointSize();
    patchOut = vec4(0.5);
    patchOut *= foop();
}

in vec2 inb[];
in vec2 ind[gl_MaxPatchVertices];

#extension GL_ARB_separate_shader_objects : enable

layout(location = 3) in vec4 ivla[];
layout(location = 4) in vec4 ivlb[];

layout(location = 3) out vec4 ovla[];
layout(location = 4) out vec4 ovlb[];

patch out pinbn {
    int a;
} pinbi;

centroid out vec3 myColor2[];
centroid in vec3 centr[];
out float okaySize[4];

#extension GL_OES_tessellation_point_size : enable

// GL_EXT_tessellation_point_size

void pointSize()
{
    gl_out[1].gl_PointSize = gl_in[1].gl_PointSize;
}

// for testing with gpu_shader5
in vec3 inv[];
precise vec3 pv3;

float foop()
{
    precise float d;

    pv3 = inv[3];
    pv3 *= pv3;
    pv3 = fma(pv3, pv3, pv3);
    d = fma(pv3.x, pv3.y, pv3.z);

    return d;
}

// GL_OES_primitive_bounding_box

void bb()
{
    gl_BoundingBoxOES[0] = vec4(0.0);
    gl_BoundingBoxOES[1] = vec4(1.0);
}
