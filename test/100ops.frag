#version 100

lowp float foo();

uniform int low, high;

lowp float face1 = gl_FrontFacing ? -1.0 : 1.0;

void main()
{
    int z = 3;

    if (2 * low + 1 < high)
        ++z;

    gl_FragColor = face1 * vec4(z) + foo();
}

// TODO: this doesn't work yet
//lowp float face2 = gl_FrontFacing ? -1.0 : 1.0;
//
//lowp float foo()
//{
//    // just testing if face2 insert it's logic correctly in main
//    return face2;
//}
