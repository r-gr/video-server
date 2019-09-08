vec4 Translate(sampler2D in_tex, float x_offset, float y_offset)
{
    vec2 xy = TexCoord;
    float x = xy.x - x_offset;
    float y = xy.y + y_offset;
    return texture(in_tex, vec2(x, y));
}
