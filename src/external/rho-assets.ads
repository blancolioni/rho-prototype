limited with Rho.Context;

with Rho.Materials.Material;
with Rho.Mesh;
with Rho.Shaders;
with Rho.Texture;

private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Rho.String_Maps;
private with Rho.Object;

package Rho.Assets is

   type Rho_Asset_Handle is tagged limited private;

   procedure Create_Handle
     (Handle   : in out Rho_Asset_Handle;
      Context  : not null access Rho.Context.Rho_Context_Record'Class);

   function Material
     (Handle : in out Rho_Asset_Handle;
      Name   : String)
      return Rho.Materials.Material.Rho_Material;

   function Mesh
     (Handle : in out Rho_Asset_Handle;
      Name   : String)
      return Rho.Mesh.Rho_Mesh;

   function Shader
     (Handle : in out Rho_Asset_Handle;
      Name   : String)
      return Rho.Shaders.Rho_Shader;

   function Texture
     (Handle : in out Rho_Asset_Handle;
      Name   : String)
      return Rho.Texture.Rho_Texture;

   function Image_Path
     (Handle        : Rho_Asset_Handle;
      Relative_Path : String)
      return String;

   procedure Add_Search_Path
     (Handle : in out Rho_Asset_Handle;
      Path   : String);

   procedure Add_Image_Path
     (Handle : in out Rho_Asset_Handle;
      Path   : String);

   procedure Add_Folder_Name
     (Handle      : in out Rho_Asset_Handle;
      Class_Name  : String;
      Folder_Name : String);

private

   package Search_Path_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package Asset_Map is
     new Rho.String_Maps (Rho.Object.Rho_Object,
                          Rho.Object."=");

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package Resource_Folder_Map is
     new Rho.String_Maps (String_Lists.List, String_Lists."=");

   type Rho_Asset_Handle is tagged limited
      record
         Context      : access Rho.Context.Rho_Context_Record'Class;
         Search_Paths : Search_Path_Lists.List;
         Image_Paths  : Search_Path_Lists.List;
         Assets       : Asset_Map.Map;
         Folders      : Resource_Folder_Map.Map;
      end record;

end Rho.Assets;
