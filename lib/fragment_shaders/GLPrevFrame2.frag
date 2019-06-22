vec4 GLPrevFrame2(vec2 coords)
{
    return texture(sc_PrevFrame, vec2(coords.x, 1 - coords.y));
}
