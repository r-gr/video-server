vec4 Rotate(sampler2D in_tex, float angle)
{
    float x = TexCoord.x - 0.5;
    float y = TexCoord.y - 0.5;
    float cos_a = cos(angle);
    float sin_a = sin(angle);
    vec2 coords = vec2(
        cos_a*x - sin_a*y + 0.5,
        sin_a*x + cos_a*y + 0.5
    );
    return texture(in_tex, coords);
}
