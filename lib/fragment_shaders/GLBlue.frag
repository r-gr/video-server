vec4 GLBlue(vec4 colour, float blue)
{
    float r = colour.r;
    float g = colour.g;
    float b = blue * colour.b;
    float a = colour.a;

    return vec4(r, g, b, a);
}
