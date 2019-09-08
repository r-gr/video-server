vec4 Scale2(sampler2D in_tex, float xy_scale)
{
    vec2 xy = TexCoord;
         xy -= 0.5;
         xy = xy / xy_scale;
         xy += 0.5;
    return texture(in_tex, xy);
}
