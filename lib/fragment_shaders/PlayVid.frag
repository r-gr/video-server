vec4 PlayVid(sampler2D video_texture, float _rate, bool _loop)
{
    return texture(video_texture, TexCoord);
}
