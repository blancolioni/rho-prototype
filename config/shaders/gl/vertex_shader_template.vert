#version 330 core
uniform mat4 camera;
uniform mat4 model;

@$parameters

in vec3 vert;
[texture]in vec2 vertTexCoord;
[light]in vec3 vertNormal;
[no-texture]in vec4 vertColor;

[light]out vec3 fragVert;
[texture]out vec2 fragTexCoord;
[light]out vec3 fragNormal;
[no-texture]out vec4 fragColor;

[texture mirror]float normTexAddr(float t)
[texture mirror]{
[texture mirror]  float u = abs(t);
[texture mirror]  if (u / 2.0 >= 0.5) {
[texture mirror]    return 1.0 - (u - floor (u));
[texture mirror]  } else {
[texture mirror]    return u - floor (u);
[texture mirror]  }  
[texture mirror]}

void main() {
    // Pass some variables to the fragment shader
[texture mirror]    fragTexCoord = vec2(normTexAddr(vertTexCoord.x), normTexAddr(vertTexCoord.y));
[texture no-mirror]    fragTexCoord = vertTexCoord;
[no-texture]    fragColor = vertColor + $ambient-color;
[light]    fragNormal = vertNormal;
[light]    fragVert = vert;
    
    // Apply all matrix transformations to vert
    gl_Position = camera * model * vec4(vert, 1);
}