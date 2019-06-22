#version 430 core

in  vec2 TexCoord;
out vec4 FragColor;

uniform sampler2D sc_PrevFrame;

void main()
{
    FragColor = vec4(0.0, 0.0, 0.0, 1.0);
}
