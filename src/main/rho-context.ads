limited with Rho.Rendering;

with Rho.Materials.Material;
with Rho.Texture;

package Rho.Context is

   type Rho_Context_Record is abstract tagged limited private;

   type Rho_Context is access all Rho_Context_Record'Class;

   function Image_Path
     (Context         : Rho_Context_Record;
      Image_File_Name : String)
      return String
      is abstract;

   function Renderer
     (Context : Rho_Context_Record)
      return access Rho.Rendering.Rho_Renderer_Record'Class
      is abstract;

   function Material
     (Context : not null access Rho_Context_Record;
      Name    : String)
      return Rho.Materials.Material.Rho_Material
      is abstract;

   function Texture
     (Context : not null access Rho_Context_Record;
      Name    : String)
      return Rho.Texture.Rho_Texture
      is abstract;

private

   type Rho_Context_Record is abstract tagged limited
      record
         null;
      end record;

end Rho.Context;
