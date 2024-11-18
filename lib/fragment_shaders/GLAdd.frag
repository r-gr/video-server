vec4 GLAdd(vec4 in_1, vec4 in_2)
{
    vec3  c_a     = in_2.a * in_2.rgb;
    float alpha_a = in_2.a;
    vec3  c_b     = in_1.a * in_1.rgb;
    float alpha_b = in_1.a;

    // c_o = c_a + c_b*(1 - alpha_a)
    // where   c_i = alpha_i * C_i
    // and e.g C_a = in_2.rgb
    vec3 c_o = vec3(c_a + c_b*(1.0 - alpha_a));
    // alpha_o = alpha_a + alpha_b*(1 - alpha_a)
    float alpha_o = alpha_a + alpha_b*(1.0 - alpha_a);

    return vec4(c_o, alpha_o);
}
