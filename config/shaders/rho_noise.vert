#version 330 core
uniform mat4 camera;
uniform mat4 model;

in vec3 vert;
in vec3 vertNormal;

out vec4 fragColor;
out vec3 fragNormal;
out vec3 fragVert;

void
main(void)
{
  vec4 vOutPosition = camera * model * vec4(vert, 1);
  gl_Position = vOutPosition;
  fragColor = vec4(1.0);
  fragNormal = vertNormal;
  fragVert = vert;
}
