#version 330 core

uniform sampler2D materialTex;

in vec2 fragTexCoord;
out vec4 finalColor;

void
main()
{
  finalColor = texture(materialTex, fragTexCoord);
  if (finalColor.w == 0.0) discard;
}
