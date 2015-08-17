#version 310 es

// + GL_EXT_geometry_shader
// + GL_EXT_geometry_point_size

#extension GL_ANDROID_extension_pack_es31a : enable

precision mediump float;

in fromVertex {
    in vec3 color;
} fromV[];

in vec4 nonBlockUnsized[];

out toFragment {
    out vec3 color;
} toF;

out fromVertex {
    centroid vec3 color;
    int len;
    int inv;
};

layout(triangle_strip) out;
layout(max_vertices = 200) out;
layout(lines_adjacency) in;
layout(invocations = 4) in;

centroid in vec3 centr[];

void pointSize2();

void main()
{
    EmitVertex();
    EndPrimitive();

    color = fromV[0].color + centr[1];
    gl_Position = gl_in[0].gl_Position;

    gl_PrimitiveID = gl_PrimitiveIDIn;
    gl_Layer = 2;

    len = fromV.length();     // 4: lines_adjacency
    inv = gl_InvocationID;

    pointSize2();
}


#extension GL_OES_geometry_point_size : enable

void pointSize2()
{
    highp float ps = gl_in[3].gl_PointSize;
    gl_PointSize = ps;
}
