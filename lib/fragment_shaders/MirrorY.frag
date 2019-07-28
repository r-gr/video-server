vec4 MirrorY(sampler2D in_tex, float y_offset, float mirror_pos)
{
    float offset = y_offset / 2.0;    // [-1, 1] -> [-1/2, 1/2]
    float pos = mirror_pos/2.0 + 0.5; // [-1, 1] -> [0, 1]

    vec2 xy = TexCoord;
    float y = (xy.y <= pos) ? xy.y - offset : 1.0 - xy.y - offset;
    xy = vec2(xy.x, y);

    return texture(in_tex, xy);
}
