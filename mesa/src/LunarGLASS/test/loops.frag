#version 130
uniform vec4 bigColor;
uniform vec4 bigColor1_1;
uniform vec4 bigColor1_2;
uniform vec4 bigColor1_3;
uniform vec4 bigColor2;
uniform vec4 bigColor3;
uniform vec4 bigColor4;
uniform vec4 bigColor5;
uniform vec4 bigColor6;
uniform vec4 bigColor7;
uniform vec4 bigColor8;

varying vec4 BaseColor;

uniform float d;
uniform float d2;
uniform float d3;
uniform float d4;
uniform float d5;
uniform float d6;
uniform float d7;
uniform float d8;
uniform float d9;
uniform float d10;
uniform float d11;

uniform int Count;

void main()
{
    vec4 color = BaseColor;

    // While
    while (color.x < d) {
        color += bigColor;
    }

    // While (latchy)
    while (color.z < d) {
        color += bigColor1_1;
        if (color.w < d)
            continue;

        color += bigColor1_1;
    }

    // While (constant)
    while (color.x < 42.0) {
        ++color;
    }

    // While (complicated-conditional)
    while (color.w < d2 && color.y < d3) {
        color += bigColor1_2;
    }

    // While (multi-exit)
    while (color.z < d3) {
        color += bigColor1_3;
        if (color.y < d4)
            break;
        color += bigColor1_3;
    }

    // For (dynamic)
    for (int i = 0; i < Count; ++i) {
        color += bigColor2;
    }

    // Do while
    do {
        color += bigColor3;
    } while (color.x < d2);

    // For (static)
    for (int i = 0; i < 42; ++i) {
        color.z += d3;
    }

    // For (static) flow-control
    for (int i = 0; i < 100; ++i) {
        if (color.z < 20.0)
            color.x++;
        else
            color.y++;
        if (color.w < 20.0)
            if (color.z > color.y)
                0;              // do nothing
    }

    // For (static) latchy
    for (int i = 0; i < 42; ++i) {
        color.z += d3;
        if (color.x < d4)
            continue;
        ++color.w;
    }

    // For (static) multi-exit
    for (int i = 0; i < 42; ++i) {
        color.z += d3;
        if (color.x < d4)
            break;
        ++color.w;
    }

    // Latchy
    do {
        color += bigColor4;
        if (color.x < d4)
            continue;
        if (color.y < d4)
            color.y += d4;
        else
            color.x += d4;
    } while (color.z < d4);

    // Latchy2
    do {
        color += bigColor4;
        if (color.x < d4) {
            color.z += 2.0;
            if (color.z < d4) {
                color.x++;
                continue;
            }
        }
        if (color.y < d4)
            color.y += d4;
        else
            color.x += d4;
    } while (color.z < d4);

    // Do while flow control
    do {
        color += bigColor5;
        if (color.y < d5)
            color.y += d5;
    } while (color.x < d5);

    // If then loop
    if (color.x < d6) {
        while (color.y < d6)
            color += bigColor6;
    } else {
        while (color.z < d6)
            color.z += bigColor6.z;
    }


    // Multi-exit
    do {
       if (d7 < 0.0)
           break;

       color += bigColor7;

       if (d7 < 1.0) {
           color.z++;
           break;
       }

       color += BaseColor;

    } while (true);


    // Multi-exit2
    do {
       if (d8 < 0.0)
           break;

       color += bigColor7;

       if (d8 < 1.0) {
           color.z++;
           if (d8 < 2.0) {
               color.y++;
           } else {
               color.x++;
           }
           break;
       }

       color += BaseColor;

    } while (color.z < d8);


    // // // Broken:
    // // // Multi-continue
    // // while (color.x < 10) {
    // //     color += bigColor8;

    // //     if (color.z < d8)
    // //         if (color.w < d6)
    // //             continue;

    // //     color.y += bigColor8.x;
    // // }


    gl_FragColor = color;
}
