#version 300 es

in lowp float lowfin;
in mediump float mediumfin;
in highp vec4 highfin;

uniform highp int global_high;
uniform mediump int global_medium;
uniform lowp int global_low;

out mediump vec4 mediumfout;

lowp vec2 foo(mediump vec3 mv3)
{
    return highfin.xy;
}

void main()
{
    lowp int sum = global_medium + global_high;

    sum += global_high;
    sum += global_low;
    
    // test maxing precisions of args to get precision of builtin
    lowp float arg1 = 3.2;
    mediump float arg2 = 1023908.2;
    lowp float d = distance(lowfin, mediumfin);

    mediumfout = vec4(sin(d));

    sum += 4 + ((ivec2(global_low) * ivec2(global_high) + ivec2((/* comma operator */global_low, global_high)))).x;

    mediumfout += vec4(sum);
}
