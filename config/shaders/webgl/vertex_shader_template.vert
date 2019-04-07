precision mediump float;

uniform mat4 camera;
uniform mat4 model;
@$parameters

attribute vec3 vertexPosition;
[texture]attribute vec2 vertexTexCoord;
[light]attribute vec3 vertexNormal;
[no-texture]attribute vec4 vertexColor;

[light]varying vec3 fragVert;
[texture]varying vec2 fragTexCoord;
[light]varying vec3 fragNormal;
[no-texture]varying vec4 fragColor;

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
[texture mirror]    fragTexCoord = vec2(normTexAddr(vertexTexCoord.x), normTexAddr(vertexTexCoord.y));
[texture no-mirror]    fragTexCoord = vertexTexCoord;
[no-texture]    fragColor = vertexColor + $ambient-color;
[light]    fragNormal = vertexNormal;
[light]    fragVert = vertexPosition;
    
    // Apply all matrix transformations to vertexPosition
    gl_Position = camera * model * vec4(vertexPosition, 1);
}