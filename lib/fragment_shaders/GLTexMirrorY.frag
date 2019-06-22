vec2 GLTexMirrorY(vec2 coords, float y_offset, float mirror_pos)
{
    float offset = y_offset / 2.0;    // [-1, 1] -> [-1/2, 1/2]
    float pos = mirror_pos/2.0 + 0.5; // [-1, 1] -> [0, 1]

    float y = (coords.y <= pos) ? coords.y - offset : 1.0 - coords.y - offset;
    return vec2(coords.x, y);
}
