vec4 GLRGB(vec4 colour, float red, float green, float blue)
{
    float r = red   * colour.r;
    float g = green * colour.g;
    float b = blue  * colour.b;
    float a = colour.a;

    return vec4(r, g, b, a);
}
