vec4 SHKColorInvert(vec4 current_color) {
    // find the current pixel color
    // vec4 current_color = texture2D(u_texture, v_tex_coord);
    vec4 output_color = vec4(0.0);

    // if it's not transparent
    if (current_color.a > 0.0) {
        // subtract its current RGB values from 1 and use its current alpha; multiply by the node alpha so we can fade in or out
        output_color = vec4(1.0 - current_color.rgb, current_color.a) * current_color.a;
    } else {
        // use the current (transparent) color
        output_color = current_color;
    }

    return output_color;
}
