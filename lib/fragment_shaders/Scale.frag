vec4 Scale(sampler2D in_tex, float xy_scale)
{
	vec2 xy = TexCoord;
         xy = vec2(xy.x * xy_scale, xy.y * xy_scale);
    return texture(in_tex, xy);
}
