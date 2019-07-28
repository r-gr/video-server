vec4 FlipY(sampler2D in_tex)
{
	vec2 coord = TexCoord;
    return texture(in_tex, vec2(coord.x, 1 - coord.y));
}
