with Cairo;

with WL.Images;

with Rho.Color;
with Rho.Object;
with Rho.Rectangle;
with Rho.Shader;

package Rho.Texture is

   Rho_Texture_Class_Name : constant String := "texture";

   type Texture_Id is new Natural;

   type Texture_Address_Mode is (Border, Clamp, Mirror, Wrap);
   type Texture_Filter_Type is (Nearest, Linear);

   type Rho_Texture_Record is
     new Rho.Object.Rho_Resource_Record with private;

   overriding function Class_Name (Texture : Rho_Texture_Record) return String
   is (Rho_Texture_Class_Name);

   procedure Load (Texture : in out Rho_Texture_Record);
   procedure Unload (Texture : in out Rho_Texture_Record);

   function Loaded (Texture : Rho_Texture_Record) return Boolean;
   function Id (Texture : Rho_Texture_Record) return Texture_Id;

   procedure Set_Surface
     (Texture : in out Rho_Texture_Record;
      Surface : Cairo.Cairo_Surface;
      Region  : Rho.Rectangle.Rho_Rectangle);

   procedure Set_Uniform
     (Texture : in out Rho_Texture_Record;
      Uniform : Rho.Shader.Rho_Uniform_Value);

   function Has_Uniform
     (Texture : Rho_Texture_Record)
      return Boolean;

   function Uniform
     (Texture : Rho_Texture_Record)
      return Rho.Shader.Rho_Uniform_Value
     with Pre => Texture.Has_Uniform;

   type Rho_Texture is access all Rho_Texture_Record'Class;

   function Create
     (Name : String)
     return Rho_Texture;

   function Create_From_Png
     (Name : String;
      Path : String)
      return Rho_Texture;

   function Create_From_Surface
     (Surface : Cairo.Cairo_Surface)
      return Rho_Texture;

   function Create_From_Data
     (Name : String;
      Data : Rho.Color.Rho_Color_1D_Array)
      return Rho_Texture;

   function Create_From_Data
     (Name : String;
      Data : Rho.Color.Rho_Color_2D_Array)
      return Rho_Texture;

   function Create_From_Image
     (Name  : String;
      Image : WL.Images.Image_Type)
      return Rho_Texture;

   type Array_Of_Textures is
     array (Positive range <>) of Rho_Texture;

private

   type Rho_Texture_Record is
     new Rho.Object.Rho_Resource_Record with
      record
         Id           : Texture_Id;
         Surface      : Cairo.Cairo_Surface;
         Data         : access Rho.Color.Rho_Color_2D_Array;
         Region       : Rho.Rectangle.Rho_Rectangle;
         S_Wrap       : Rho.Texture.Texture_Address_Mode;
         T_Wrap       : Rho.Texture.Texture_Address_Mode;
         Mag_Filter   : Rho.Texture.Texture_Filter_Type;
         Uniform      : Rho.Shader.Rho_Uniform_Value;
         Has_Uniform  : Boolean := False;
      end record;

end Rho.Texture;
