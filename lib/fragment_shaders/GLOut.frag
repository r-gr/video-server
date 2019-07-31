vec4 GLOut(vec4 frag_color, vec4 wire)
{
    // Assume that frag_color will only ever have an opacity of 1.0
    vec3 out_color = wire.rgb * wire.a + frag_color.rgb * (1.0 - wire.a);
    // float out_opacity = clamp(frag_color.a + wire.a, 0.0, 1.0);
    // return vec4(out_color, out_opacity);
    return vec4(out_color, 1.0);
}
