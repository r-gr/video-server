vec4 GLPlayImg(sampler2D image_texture)
{
    return texture(image_texture, TexCoord);
}
