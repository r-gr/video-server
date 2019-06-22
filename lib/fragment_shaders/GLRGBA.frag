vec4 GLRGBA(vec4 colour, float red, float green, float blue, float alpha)
{
    float r = red   * colour.r;
    float g = green * colour.g;
    float b = blue  * colour.b;
    float a = alpha * colour.a;

    return vec4(r, g, b, a);
}
