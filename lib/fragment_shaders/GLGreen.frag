vec4 GLGreen(vec4 colour, float green)
{
    float r = colour.r;
    float g = green * colour.g;
    float b = colour.b;
    float a = colour.a;

    return vec4(r, g, b, a);
}
