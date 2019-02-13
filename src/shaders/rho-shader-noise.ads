with Rho.Color;
with Rho.Materials.Material;
with Rho.Texture;

package Rho.Shader.Noise is

   type Rho_Noise_Shader_Record is
     new Rho_Shader_Record with private;

   procedure Set_Octaves
     (Shader  : in out Rho_Noise_Shader_Record'Class;
      Octaves : Non_Negative_Float);

   procedure Set_Palette
     (Shader  : in out Rho_Noise_Shader_Record'Class;
      Palette : Rho.Color.Rho_Color_1D_Array);

   procedure Reset
     (Shader    : not null access Rho_Noise_Shader_Record'Class;
      Initiator : Integer);

   function Material
     (Shader : Rho_Noise_Shader_Record'Class)
      return Rho.Materials.Material.Rho_Material;

   type Rho_Noise_Shader is access all Rho_Noise_Shader_Record'Class;

   function Create_Noise_Shader
     (Initiator  : Integer;
      Octaves    : Non_Negative_Float;
      Roughness  : Unit_Float;
      Lacunarity : Non_Negative_Float;
      Palette    : Rho.Color.Rho_Color_1D_Array)
      return Rho_Noise_Shader;

private

   Map_Length : constant := 256;

   type Uniform_Vector_3_Array_Access is access Uniform_Vector_3_Array;
   type Uniform_Integer_Array_Access is access Uniform_Integer_Array;

   type Rho_Noise_Shader_Record is
     new Rho_Shader_Record with
      record
         Octave_Uniform     : Rho_Uniform_Value;
         Roughness_Uniform  : Rho_Uniform_Value;
         Lacunarity_Uniform : Rho_Uniform_Value;
         Buffer_Uniform     : Rho_Uniform_Value;
         Index_Map_Uniform  : Rho_Uniform_Value;
         Palette            : Rho.Color.Rho_Color_1D_Array_Access;
         Buffer             : Uniform_Vector_3_Array_Access;
         Index_Map          : Uniform_Integer_Array_Access;
         Texture            : Rho.Texture.Rho_Texture;
         Material           : Rho.Materials.Material.Rho_Material;
      end record;

   procedure Create_Material
     (Shader    : not null access Rho_Noise_Shader_Record'Class;
      Initiator : Integer);

   function Material
     (Shader : Rho_Noise_Shader_Record'Class)
      return Rho.Materials.Material.Rho_Material
   is (Shader.Material);

end Rho.Shader.Noise;
