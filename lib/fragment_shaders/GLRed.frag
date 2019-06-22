vec4 GLRed(vec4 colour, float red)
{
    float r = red * colour.r;
    float g = colour.g;
    float b = colour.b;
    float a = colour.a;

    return vec4(r, g, b, a);
}
