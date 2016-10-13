#version 140

uniform int g_nDebugMode;
uniform int g_nTest;
uniform float g_flTest;

uniform vec4 result0;
uniform vec4 result1;
uniform vec4 result2;
uniform vec4 result3;
vec4 ApplyDebugMode( out bool bReturn )
{
    vec4 vResult;
    if ( g_nDebugMode == 0 )
    {
        bReturn = true;
        vResult = result0;
    }
    else
    {
        bReturn = false;
        vResult = result1;
    }
    return vResult;
}

vec4 foo()
{
    vec4 vOutColor;
    if ( g_nDebugMode != 1 )
    {
        bool bReturn = true;
        vOutColor = ApplyDebugMode( bReturn );
        if ( bReturn )
            return vOutColor;
    }

    if ( g_flTest > 0.0 )
    {
        if ( g_nTest > 0 )
        {
            vOutColor = result2;
        }
        else
        {
            vOutColor = result3;
        }

    }
    return vOutColor;
}

void main()
{
    gl_FragColor = foo();
}

