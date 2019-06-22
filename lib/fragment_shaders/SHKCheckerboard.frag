vec4 SHKCheckerboard(vec4 current_color, float u_rows, float u_cols) { // , vec3 u_first_color, vec3 u_second_color) {
    // find the current pixel color
    vec4 output_color = vec4(0.0);
    vec4 u_first_color = vec4(1.0);
    vec4 u_second_color = vec4(0.0, 0.0, 0.0, 1.0);

    if (current_color.a > 0.0) {
        // figure out whether we are an even column
        bool x = mod(u_cols * TexCoord.x, 2.0) < 1.0;

        // figure out whether we are an even row
        bool y = mod(u_rows * TexCoord.y, 2.0) < 1.0;

        // iff one of these is true
        if ((x == true && y == false) || (x == false && y == true)) {
            // use the first color
            output_color = u_first_color * current_color.a;
        } else {
            // use the second color
            output_color = u_second_color * current_color.a;
        }
    } else {
        // use the current (transparent) color
        output_color = current_color;
    }

    return output_color;
}
