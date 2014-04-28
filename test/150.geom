#version 150 core

layout(triangles_adjacency) in;
layout(max_vertices = 30) out;
layout(stream = 3, triangle_strip) out;

in fromVertex {
    in vec3 color;
} fromV[];

out toFragment {
    out vec3 color;
} toF;

out fromVertex {
    vec3 color;
};

void main()
{
    EmitVertex();
    EndPrimitive();

    color = fromV[0].color;
    //?? gl_ClipDistance[3] = gl_in[1].gl_ClipDistance[2];
    gl_Position = gl_in[0].gl_Position;
    gl_PointSize = gl_in[3].gl_PointSize;
    gl_PrimitiveID = gl_PrimitiveIDIn;
    gl_Layer = 2;
}
