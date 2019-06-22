vec4 SHKWater(sampler2D u_texture, float u_time, float u_speed, float u_strength, float u_frequency) {
    // bring both speed and strength into the kinds of ranges we need for this effect
    float speed = u_time * u_speed * 0.05;
    float strength = u_strength / 100.0;

    // take a copy of the current texture coordinate so we can modify it
    vec2 coord = TexCoord;

    // offset the coordinate by a small amount in each direction, based on wave frequency and wave strength
    coord.x += sin((coord.x + speed) * u_frequency) * strength;
    coord.y += cos((coord.y + speed) * u_frequency) * strength;

    // use the color at the offset location for our new pixel color
    return texture(u_texture, coord);
}
