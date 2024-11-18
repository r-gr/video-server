vec4 GLBlend(vec4 backdrop, vec4 source, float blendModeF, float mixVal)
{
    float alpha = max(backdrop.a, source.a);
    vec3 cb = backdrop.rgb;
    vec3 cs = source.rgb;
    vec4 result;

    int blendMode = int(floor(blendModeF));

    switch (blendMode) {
        case 0: // normal
            result = vec4(cs, alpha);
            break;
        case 1: // multiply
            result = vec4(cb * cs, alpha);
            break;
        case 2: // screen
            result = vec4(cb + cs - (cb * cs), alpha);
            break;
        case 3: // darken
            result = vec4(min(cb, cs), alpha);
            break;
        case 4: // lighten
            result = vec4(max(cb, cs), alpha);
            break;
        case 5: // colour dodge
            result = vec4(
                cs.r < 1.0 ? min(1.0, cb.r / (1.0 - cs.r)) : 1.0,
                cs.g < 1.0 ? min(1.0, cb.g / (1.0 - cs.g)) : 1.0,
                cs.b < 1.0 ? min(1.0, cb.b / (1.0 - cs.b)) : 1.0,
                alpha
            );
            break;
        case 6: // colour burn
            result = vec4(
                cs.r > 0 ? 1.0 - min(1.0, (1.0 - cb.r) / cs.r) : 1.0,
                cs.g > 0 ? 1.0 - min(1.0, (1.0 - cb.g) / cs.g) : 1.0,
                cs.b > 0 ? 1.0 - min(1.0, (1.0 - cb.b) / cs.b) : 1.0,
                alpha
            );
            break;
        case 7: // hard light
            result = vec4(
                cs.r <= 0.5 ? cb.r * (2.0 * cs.r) : cb.r + (2.0 * cs.r - 1.0) - (cb.r * (2.0 * cs.r - 1.0)),
                cs.g <= 0.5 ? cb.g * (2.0 * cs.g) : cb.g + (2.0 * cs.g - 1.0) - (cb.g * (2.0 * cs.g - 1.0)),
                cs.b <= 0.5 ? cb.b * (2.0 * cs.b) : cb.b + (2.0 * cs.b - 1.0) - (cb.b * (2.0 * cs.b - 1.0)),
                alpha
            );
            break;
        case 8: // overlay
            result = vec4(
                cb.r <= 0.5 ? cs.r * (2.0 * cb.r) : cs.r + (2.0 * cb.r - 1.0) - (cs.r * (2.0 * cb.r - 1.0)),
                cb.g <= 0.5 ? cs.g * (2.0 * cb.g) : cs.g + (2.0 * cb.g - 1.0) - (cs.g * (2.0 * cb.g - 1.0)),
                cb.b <= 0.5 ? cs.b * (2.0 * cb.b) : cs.b + (2.0 * cb.b - 1.0) - (cs.b * (2.0 * cb.b - 1.0)),
                alpha
            );
            break;
        case 9: // difference
            result = vec4(
                abs(cb.r - cs.r),
                abs(cb.g - cs.g),
                abs(cb.b - cs.b),
                alpha
            );
            break;
        case 10: // exclusion
            result = vec4(cb + cs - (2.0 * cb * cs), alpha);
            break;
        default:
            mix(backdrop, result, 0.5);
            break;
    }

    return mix(backdrop, result, mixVal);
}
