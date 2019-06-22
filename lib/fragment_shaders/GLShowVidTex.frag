vec4 GLShowVidTex(sampler2D video_texture, vec2 coords)
{
    return texture(video_texture, coords);
}
