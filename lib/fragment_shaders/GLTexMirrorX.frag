vec2 GLTexMirrorX(vec2 coords, float x_offset, float mirror_pos)
{
    float offset = x_offset / 2.0;    // [-1, 1] -> [-1/2, 1/2]
    float pos = mirror_pos/2.0 + 0.5; // [-1, 1] -> [0, 1]

    float x = (coords.x <= pos) ? coords.x - offset : 1.0 - coords.x - offset;
    return vec2(x, coords.y);
}
