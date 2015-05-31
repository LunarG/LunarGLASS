#version 450

uniform int b;
uniform int c;

in float inf;

void foo(out vec3 out_v)
{
    float p = inf * (3.0F - inf);
    float q = inf * (3.0F - inf * 13.8);
    float t = inf * (3.0F - inf);

    switch (b)
    {
        case 0 : out_v = vec3 (inf, t, p); break;
        case 1 : out_v = vec3 (q, inf, p); break;
        case 2 : out_v = vec3 (p, inf, t); break;
        case 3 : out_v = vec3 (p, q, inf); break;
        case 4 : out_v = vec3 (t, p, inf); break;
        case 5 : out_v = vec3 (inf, p, q); break;
    }
}

void main()
{
    int a = b;

    vec3 out_v;
    foo(out_v);

    switch (c-8) {
    case 1:
        a = 2 * a;
        break;
    case 2:
        a += 5;
    case 3:
        a -= 15;
        break;
    case 4:
        a += a * b;
        break;
    case 9:
        --a;
    default:
        a -= 13;
        break;
    }

    gl_Position = vec4(out_v, a);
}

