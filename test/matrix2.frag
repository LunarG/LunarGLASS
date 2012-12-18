#version 130

//#define TEST_POST_110

uniform mat3 colorTransform;
varying vec3 Color;
uniform mat4 m, n;

#ifdef TEST_POST_110
uniform mat4x3 um43;
uniform mat3x4 un34;
#else
uniform mat4 um43;
uniform mat4 un34;
#endif

varying vec4 v;

#ifdef TEST_POST_110
varying vec3 u;
#else
varying vec4 u;
#endif

void main()
{
    
#ifdef TEST_POST_110
    mat3x4 m34 = outerProduct(v, u);
#else
    mat4 m34 = mat4(v.x*u.x, v.x*u.y, v.x*u.z, v.x*u.w, 
                    v.y*u.x, v.y*u.y, v.y*u.z, v.y*u.w, 
                    v.z*u.x, v.z*u.y, v.z*u.z, v.z*u.w, 
                    v.w*u.x, v.w*u.y, v.w*u.z, v.w*u.w);
#endif

    m34 += mat4(4.3);

    gl_FragColor = vec4(Color, 1.0);
    gl_FragColor *= um43;

    m34 *= v.x;

#ifdef TEST_POST_110
    mat4 m44 = mat4(un34);
#else
    mat4 m44 = mat4(4.7);
#endif

    m44 *= um43;

    //m44 = matrixCompMult(m44, m44);

    gl_FragColor += (-m44) * u;

    gl_FragColor *= matrixCompMult(m44, m44);
}
