#version 430 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec3 vOffset;

uniform mat4 ModelViewMatrix;
uniform mat4 ProjectionMatrix;

out vec4 instance_colour;

void
main(void)
{
  vec4 v = vPosition + vec4(vOffset, 0.0);
  
  switch(gl_InstanceID){
     case 0:
        instance_colour = vec4(1.0, 0.0, 0.0, 1.0);
        break;
     case 1:
        instance_colour = vec4(0.0, 1.0, 0.0, 1.0);
        break;
     case 2:
        instance_colour = vec4(0.0, 0.0, 1.0, 1.0);
        break;
     case 3:
        instance_colour = vec4(1.0, 1.0, 0.0, 1.0);
        break;
     case 4:
        instance_colour = vec4(1.0, 0.0, 1.0, 1.0);
        break;
     case 5:
        instance_colour = vec4(0.0, 1.0, 1.0, 1.0);
        break;
  }
  
  gl_Position = ProjectionMatrix * ModelViewMatrix * v;
}
