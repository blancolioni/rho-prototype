#version 150

uniform mat4 model;
uniform vec3 cameraPosition;

// material settings
[texture]uniform sampler2D materialTex;
[light]uniform float materialShininess;
[light]uniform vec3 materialSpecularColor;

[light]uniform struct Light {
[light]   vec3 position;
[light]   vec3 intensities; //a.k.a the color of the light
[light]   float attenuation;
[light]   float ambientCoefficient;
[light]} light;

[texture]in vec2 fragTexCoord;
[no-texture]in vec4 fragColor;
[light]in vec3 fragNormal;
[light]in vec3 fragVert;

out vec4 finalColor;

void main() {
[light]    vec3 normal = normalize(transpose(inverse(mat3(model))) * fragNormal);
[light]    vec3 surfacePos = vec3(model * vec4(fragVert, 1));
[texture]    vec4 surfaceColor = texture(materialTex, fragTexCoord);
[no-texture]    vec4 surfaceColor = fragColor;
[light]    vec3 surfaceToLight = normalize(light.position - surfacePos);
[light]    vec3 surfaceToCamera = normalize(cameraPosition - surfacePos);
    
[light]    //ambient
[light]    vec3 ambient = light.ambientCoefficient * surfaceColor.rgb * light.intensities;

[light]    //diffuse
[light]    float diffuseCoefficient = max(0.0, dot(normal, surfaceToLight));
[light]    vec3 diffuse = diffuseCoefficient * surfaceColor.rgb * light.intensities;
    
[light]    //specular
[light]    float specularCoefficient = 0.0;
[light]    if(diffuseCoefficient > 0.0)
[light]        specularCoefficient = pow(max(0.0, dot(surfaceToCamera, reflect(-surfaceToLight, normal))), materialShininess);
[light]    vec3 specular = specularCoefficient * materialSpecularColor * light.intensities;
    
[light]    //attenuation
[light]    float distanceToLight = length(light.position - surfacePos);
[light]    float attenuation = 1.0 / (1.0 + light.attenuation * pow(distanceToLight, 2));

[light]    //linear color (color before gamma correction)
[light]    vec3 linearColor = ambient + attenuation*(diffuse + specular);
    
[light]    //final color (after gamma correction)
[light]    vec3 gamma = vec3(1.0/2.2);
[light]    finalColor = vec4(pow(linearColor, gamma), surfaceColor.a);
[no-light]    finalColor = surfaceColor;
    $alpha-discard
    $color-discard
}