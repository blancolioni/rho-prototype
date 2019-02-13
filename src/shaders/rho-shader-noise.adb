with Ada.Numerics.Float_Random;

with Rho.Shader.Load;

package body Rho.Shader.Noise is

   ---------------------
   -- Create_Material --
   ---------------------

   procedure Create_Material
     (Shader    : not null access Rho_Noise_Shader_Record'Class;
      Initiator : Integer)
   is
      use Ada.Numerics.Float_Random;
      Gen : Generator;

   begin

      Reset (Gen, Initiator);

      for Index in Shader.Buffer'Range loop
         Shader.Index_Map (Index) := Index;
         for V of Shader.Buffer (Index) loop
            V := Unit_Float (Random (Gen)) - 0.5;
         end loop;

         Shader.Buffer (Index) :=
           Rho.Matrices.Normalise (Shader.Buffer (Index));

      end loop;

      for Index in reverse Shader.Index_Map'Range loop
         declare
            Target_Index : constant Positive :=
                             Positive (Natural (Random (Gen) * 65535.0)
                                             / 256 + 1);
            Temp         : constant Positive :=
                             Shader.Index_Map (Index);
         begin
            Shader.Index_Map (Index) := Shader.Index_Map (Target_Index);
            Shader.Index_Map (Target_Index) := Temp;
         end;
      end loop;

      declare
         use type Rho.Materials.Material.Rho_Material;
      begin
         if Shader.Material = null then
            Shader.Texture :=
              Rho.Texture.Create_From_Data
                ("noise-texture", Shader.Palette.all);

            Shader.Material :=
              Rho.Materials.Material.Rho_New_With_Texture
                ("noise-material", Shader.Texture, Lighting => True);
            Shader.Material.Technique (1).Pass (1).Set_Shader
              (Rho_Shader (Shader));

         end if;
      end;

   end Create_Material;

   -------------------------
   -- Create_Noise_Shader --
   -------------------------

   function Create_Noise_Shader
     (Initiator  : Integer;
      Octaves    : Non_Negative_Float;
      Roughness  : Unit_Float;
      Lacunarity : Non_Negative_Float;
      Palette    : Rho.Color.Rho_Color_1D_Array)
      return Rho_Noise_Shader
   is
      Shader : constant Rho_Noise_Shader := new Rho_Noise_Shader_Record;
      Vertex_Shader   : constant Rho_Vertex_Shader :=
                          Rho.Shader.Load.Load ("Rho_noise.vert");
      Fragment_Shader : constant Rho_Fragment_Shader :=
                          Rho.Shader.Load.Load ("Rho_noise.frag");
   begin
      Rho_Initialize (Shader.all);
      Shader.Add (Vertex_Shader);
      Shader.Add (Fragment_Shader);
      Shader.Compile;

      Shader.Activate;

      Shader.Octave_Uniform := Shader.Uniform_Value ("octave_count");
      Shader.Octave_Uniform.Set_Value (Octaves);

      Shader.Roughness_Uniform := Shader.Uniform_Value ("roughness");
      Shader.Roughness_Uniform.Set_Value (Roughness);

      Shader.Lacunarity_Uniform := Shader.Uniform_Value ("lacunarity");
      Shader.Lacunarity_Uniform.Set_Value (Lacunarity);

      Shader.Buffer_Uniform := Shader.Uniform_Value ("random_buffer");
      Shader.Index_Map_Uniform := Shader.Uniform_Value ("map_index");

      Shader.Palette := new Rho.Color.Rho_Color_1D_Array'
        (Rho.Color.Scale (Palette, Map_Length, Rho.Color.Linear));

      Shader.Buffer := new Uniform_Vector_3_Array (1 .. Map_Length);
      Shader.Index_Map := new Uniform_Integer_Array (1 .. Map_Length);

      Shader.Create_Material (Initiator);

      Shader.Activate;
      Shader.Buffer_Uniform.Set_Array (Shader.Buffer.all);
      Shader.Index_Map_Uniform.Set_Array (Shader.Index_Map.all);

      return Shader;
   end Create_Noise_Shader;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Shader    : not null access Rho_Noise_Shader_Record'Class;
      Initiator : Integer)
   is
   begin
      Shader.Create_Material (Initiator);
      Shader.Activate;
      Shader.Buffer_Uniform.Set_Array (Shader.Buffer.all);
      Shader.Index_Map_Uniform.Set_Array (Shader.Index_Map.all);
   end Reset;

   -----------------
   -- Set_Octaves --
   -----------------

   procedure Set_Octaves
     (Shader  : in out Rho_Noise_Shader_Record'Class;
      Octaves : Non_Negative_Float)
   is
   begin
      Shader.Activate;
      Shader.Octave_Uniform.Set_Value (Octaves);
   end Set_Octaves;

   -----------------
   -- Set_Palette --
   -----------------

   procedure Set_Palette
     (Shader  : in out Rho_Noise_Shader_Record'Class;
      Palette : Rho.Color.Rho_Color_1D_Array)
   is
   begin
      Shader.Palette.all :=
        Rho.Color.Scale (Palette, Map_Length, Rho.Color.Linear);
   end Set_Palette;

end Rho.Shader.Noise;
