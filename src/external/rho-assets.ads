with Rho.Materials.Material;
with Rho.Mesh;
with Rho.Shader;
with Rho.Texture;

package Rho.Assets is

   function Material
     (Name : String)
      return Rho.Materials.Material.Rho_Material;

   function Mesh
     (Name : String)
      return Rho.Mesh.Rho_Mesh;

   function Shader
     (Name : String)
      return Rho.Shader.Rho_Shader;

   function Texture
     (Name : String)
      return Rho.Texture.Rho_Texture;

   function Image_Path
     (Relative_Path : String)
      return String;

   procedure Add_Search_Path
     (Path : String);

   procedure Add_Image_Path
     (Path : String);

   procedure Add_Folder_Name
     (Class_Name  : String;
      Folder_Name : String);

end Rho.Assets;
