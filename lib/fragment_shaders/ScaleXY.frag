vec4 ScaleXY(sampler2D in_tex, float x_scale, float y_scale)
{
    vec2 xy = TexCoord;
         xy -= 0.5;
         xy = vec2(xy.x / x_scale, xy.y / y_scale);
         xy += 0.5;
    return texture(in_tex, xy);
}
