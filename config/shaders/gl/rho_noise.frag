#version 330 core

#define map_length 256

in vec4 fragColor;
in vec3 fragNormal;
in vec3 fragVert;

out vec4 finalColor;

uniform float random_seed;
uniform float octave_count;
uniform float roughness;
uniform float lacunarity;
uniform sampler2D materialTex;
uniform int map_index[256];
uniform vec3 random_buffer[256];

uniform mat4 model;
uniform vec3 cameraPosition;

uniform float materialShininess;
uniform vec3 materialSpecularColor;

uniform struct Light {
   vec3 position;
   vec3 intensities; //a.k.a the color of the light
   float attenuation;
   float ambientCoefficient;
} light;

float
lattice(ivec3 index, vec3 rem)
{
   int acc = 0;
   acc = map_index[(acc + index.x) % map_length];
   acc = map_index[(acc + index.y) % map_length];
   acc = map_index[(acc + index.z) % map_length];
   
   float result = 0.0;
   result += random_buffer[acc].x * rem.x;
   result += random_buffer[acc].y * rem.y;
   result += random_buffer[acc].z * rem.z;
   
   return result;
}

float 
get3(vec3 coordinate)
{
   ivec3 index;
   vec3 base, rem, cubic;
   base = floor(coordinate);
   index = ivec3(int(base.x),int(base.y),int(base.z));
   rem = coordinate - base;
   cubic = rem * rem * (vec3(3.0) - 2.0 * rem);

   float a11 = lattice(index + ivec3(0,0,0), rem - vec3(0,0,0));
   float a12 = lattice(index + ivec3(1,0,0), rem - vec3(1,0,0));
   float a21 = lattice(index + ivec3(0,1,0), rem - vec3(0,1,0));
   float a22 = lattice(index + ivec3(1,1,0), rem - vec3(1,1,0));
   float b11 = lattice(index + ivec3(0,0,1), rem - vec3(0,0,1));
   float b12 = lattice(index + ivec3(1,0,1), rem - vec3(1,0,1));
   float b21 = lattice(index + ivec3(0,1,1), rem - vec3(0,1,1));
   float b22 = lattice(index + ivec3(1,1,1), rem - vec3(1,1,1));
   
   float a1 = mix(a11, a12, cubic.x);
   float a2 = mix(a21, a22, cubic.x);
   float b1 = mix(b11, b12, cubic.x);
   float b2 = mix(b21, b22, cubic.x);
   float a = mix(a1,a2,cubic.y);
   float b = mix(b1,b2,cubic.y);
   
   return clamp(mix(a,b,cubic.z), -1, 1);
}

float
brownian3(vec3 coordinate)
{
   float result = 0;
   float f = 1;
   int last_octave = int(octave_count);
   vec3 pos = coordinate;
   for(int i=0;i<last_octave;++i) {
     result += get3(pos) * pow(f,-roughness);
     pos *= lacunarity;
     f *= lacunarity;
   }
   
   float rem = octave_count - floor(octave_count);
   if (rem > 0) {
      result += rem * get3(pos) * pow(f,-roughness);
   }
   
   return clamp(result,-1.0,1.0);
}

void
main()
{
    vec3 normal = normalize(transpose(inverse(mat3(model))) * fragNormal);
    vec3 surfacePos = vec3(model * vec4(fragVert, 1));
    vec3 surfaceToLight = normalize(light.position - surfacePos);
    vec3 surfaceToCamera = normalize(cameraPosition - surfacePos);

  vec3 stu = fragNormal.xyz + (1.0, 1.0, 1.0);
  float height = brownian3(stu);
  vec4 surfaceColor = texture(materialTex, vec2((height + 1.0) / 2.0,0.5));

    vec3 ambient = light.ambientCoefficient * surfaceColor.rgb * light.intensities;
    float diffuseCoefficient = max(0.0, dot(normal, surfaceToLight));
    vec3 diffuse = diffuseCoefficient * surfaceColor.rgb * light.intensities;
    float specularCoefficient = 0.0;
    if(diffuseCoefficient > 0.0)
        specularCoefficient = pow(max(0.0, dot(surfaceToCamera, reflect(-surfaceToLight, normal))), materialShininess);
    vec3 specular = specularCoefficient * materialSpecularColor * light.intensities;
    float distanceToLight = length(light.position - surfacePos);
    float attenuation = 1.0 / (1.0 + light.attenuation * pow(distanceToLight, 2));
    vec3 linearColor = ambient + attenuation*(diffuse + specular);

    vec3 gamma = vec3(1.0/2.2);
    finalColor = vec4(pow(linearColor, gamma), surfaceColor.a);
}
