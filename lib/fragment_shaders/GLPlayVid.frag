vec4 GLPlayVid(sampler2D video_texture)
{
    return texture(video_texture, TexCoord);
}
