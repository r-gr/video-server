vec4 SHKDesaturate(vec4 current_color, float u_strength) {
    float strength = clamp(u_strength, 0.0, 1.0);
    vec4 output_color = vec4(0.0);

    // these values correspond to how important each color is to the overall brightness
    vec3 gray_values = vec3(0.2125, 0.7154, 0.0721);

    // the dot() function multiples all the colors in our source color with all the values in our
    // gray_values conversion then sums them; this then gets put into a new vec3 color as its RGB values
    vec3 desaturated = vec3(dot(current_color.rgb, gray_values));

    // if the user requested full desaturation
    if (strength == 1.0) {
        // just show the desaturated version
        output_color = vec4(desaturated, current_color.a);
    } else {
        // blend the original and desaturated by whatever 1 - strength was passed in
        output_color = vec4(mix(current_color.rgb, desaturated, strength), current_color.a);
    }

    return output_color;
}


