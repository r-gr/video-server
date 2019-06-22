vec4 GLAlpha(vec4 colour, float alpha)
{
    float r = colour.r;
    float g = colour.g;
    float b = colour.b;
    float a = alpha * colour.a;

    return vec4(r, g, b, a);
}
