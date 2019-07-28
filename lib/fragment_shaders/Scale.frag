vec4 Scale(sampler2D in_tex, float x_scale, float y_scale)
{
	vec2 xy = TexCoord;
         xy = vec2(xy.x * x_scale, xy.y * y_scale);
    return texture(in_tex, xy);
}
