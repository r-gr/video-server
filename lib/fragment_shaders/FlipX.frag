vec4 FlipX(sampler2D in_tex)
{
    vec2 coord = TexCoord;
    return texture(in_tex, vec2(1.0 - coord.x, coord.y));
}
