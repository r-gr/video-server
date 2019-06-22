vec2 GLRotate(vec2 coords, float angle)
{
    // mat2 R = mat2(
    //     vec2(cos(angle), -sin(angle)),
    //     vec2(sin(angle), cos(angle))
    // );
    // return coords * R;
    float x = coords.x - 0.5;
    float y = coords.y - 0.5;
    return vec2(cos(angle)*x - sin(angle)*y + 0.5, sin(angle)*x + cos(angle)*y + 0.5);
}
