vec4 SHKCircleWave(vec4 current_color, float u_time, float u_speed, float u_brightness, float u_strength, float u_density, float u_center) {
    // find the current pixel color
    vec3 u_color = vec3(0.2, 0.6, 0.8); // arbitrary color
    vec4 output_color = vec4(0.0);

    // if it's not transparent
    if (current_color.a > 0.0) {
        // calculate how fast to make the waves move; this is negative so the waves move outwards
        float wave_speed = -(u_time * u_speed * 10.0);

        // create RGB colors from the provided brightness
        vec3 brightness = vec3(u_brightness);

        // how far our pixel is from the center of the circle
        float pixel_distance = distance(TexCoord, vec2(u_center));

        // create a gradient by combining our R color with G and B values calculated using our texture coordinate, then multiply the result by the provided brightness
        vec3 gradient_color = vec3(u_color.r, u_color.g, u_color.b) * brightness;

        // calculate how much color to apply to this pixel by cubing its distance from the center
        float color_strength = pow(1.0 - pixel_distance, 3.0);

        // multiply by the user's input strength
        color_strength *= u_strength;

        // calculate the size of our wave by multiplying provided density with our distance from the center
        float wave_density = u_density * pixel_distance;

        // decide how dark this pixel should be as a range from -1 to 1 by adding the speed of the overall wave by the density of the current pixel
        float cosine = cos(wave_speed + wave_density);

        // halve that cosine and add 0.5, which will give a range of 0 to 1
        // this is our wave fluctuation, which causes waves to vary between colored and dark
        float cosine_adjustment = (0.5 * cosine) + 0.5;

        // calculate the brightness for this pixel by multiplying its color strength with the sum of the user's requested strength and our cosine adjustment
        float luma = color_strength * (u_strength + cosine_adjustment);

        // force the brightness to decay rapidly so we don't hit the edges of our sprite
        luma *= 1.0 - (pixel_distance * 2.0);
        luma = max(0.0, luma);

        // multiply our gradient color by brightness for RGB, and the brightness itself for A
        vec4 final_color = vec4(gradient_color * luma, luma);

        // multiply the final color by the actual alpha of this node, so users can make it fade in/out as needed
        output_color = final_color * current_color.a;
    } else {
        // use the current (transparent) color
        output_color = current_color;
    }

    return output_color;
}


