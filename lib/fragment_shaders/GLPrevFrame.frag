vec4 GLPrevFrame()
{
    return texture(sc_PrevFrame, vec2(TexCoord.x, 1.0 - TexCoord.y));
}
