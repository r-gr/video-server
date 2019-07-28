vec4 Translate(sampler2D in_tex, float x_offset, float y_offset)
{
	vec2 xy = TexCoord;
    float x = xy.x + (x_offset/2.0 + 0.5); // offset: [-1, 1] -> [0, 1]
    float y = xy.y + (y_offset/2.0 + 0.5);
    return texture(in_tex, vec2(x, y));
}
