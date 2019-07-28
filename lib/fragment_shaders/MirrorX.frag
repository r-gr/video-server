vec4 MirrorX(sampler2D in_tex, float x_offset, float mirror_pos)
{
    float offset = x_offset / 2.0;    // [-1, 1] -> [-1/2, 1/2]
    float pos = mirror_pos/2.0 + 0.5; // [-1, 1] -> [0, 1]

    vec2 xy = TexCoord;
    float x = (xy.x <= pos) ? xy.x - offset : 1.0 - xy.x - offset;
    xy = vec2(x, xy.y);

    return texture(in_tex, xy);
}
