#version 300 es
precision mediump float;
uniform int c, d;
in float x;
out float color;

void main()
{
    float f;
    int a[2];
    int local = c;

    switch(++local)
    {
    }

    switch (c) {
    case 1:
        f = sin(x);
        break;
    case 2:
        f = cos(x);
        break;
    default:
        f = tan(x);
    }

    switch (c) {
    case 1:
        f += sin(x);
    case 2:
        f += cos(x);
        break;
    default:
        f += tan(x);
    }

    switch (c) {
    case 1:
        f += sin(x);
        break;
    case 2:
        f += cos(x);
        break;
    }

    switch (c) {
    case 1:
        f += sin(x);
        break;
    case 2:
        switch (d) {
        case 1:
            f += x * x * x;
            break;
        case 2:
            f += x * x;
            break;
        }
        break;
    default:
        f += tan(x);
    }

    for (int i = 0; i < 10; ++i) {
        switch (c) {
        case 1:
            f += sin(x);
            for (int j = 20; j < 30; ++j) {
                ++f;
                if (f < 100.2)
                    break;
            }
            break;
        case 2:
            f += cos(x);
            break;
        default:
            f += tan(x);
        }

        if (f < 3.43)
            break;
    }

    color = f + float(local);
}
