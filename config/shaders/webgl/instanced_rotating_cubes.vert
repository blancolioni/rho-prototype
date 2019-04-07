#version 430 core

layout(location = 0) in vec4 vPosition;

uniform mat4 ModelViewMatrix;
uniform mat4 ProjectionMatrix;

void
main(void)
{
  vec4 v = vPosition;
  if (gl_InstanceID == 0) {
     v.x = v.x - 1.5;
  } else {
     v.x = v.x + 1.5;
  }
  gl_Position = ProjectionMatrix * ModelViewMatrix * v;
}
