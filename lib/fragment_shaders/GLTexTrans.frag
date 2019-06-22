vec2 GLTexTrans(vec2 coords, float x_offset, float y_offset)
{
    float x = coords.x + (x_offset/2.0 + 0.5); // offset: [-1, 1] -> [0, 1]
    float y = coords.y + (y_offset/2.0 + 0.5);
    return vec2(x, y);
}
