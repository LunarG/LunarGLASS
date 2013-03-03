#version 430 core

buffer vec4 buf;

invariant out vec4 outVi;

centroid in vec4 inVc;
smooth in vec4 inVs;
flat in vec4 inVf;
noperspective in vec4 inVn;
centroid noperspective in vec4 inVcn;

sample in vec4 inV;

coherent uniform image2D imageVc;
volatile uniform image2D imageVv;
restrict uniform image2D imageVr;
readonly uniform image2D imageVro;
writeonly uniform image2D imageVwo;

coherent volatile restrict readonly writeonly uniform image2D imageVall;

void main()
{
    outVi = inVc + inVs + inVf + inVn + inVcn;
}
