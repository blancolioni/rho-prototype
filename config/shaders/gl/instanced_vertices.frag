#version 430 core

in vec4 instance_colour;

layout (location = 0) out vec4 colour;

void
main()
{
  colour = instance_colour; // vec4(1.0, 1.0, 1.0, 1.0);
}
