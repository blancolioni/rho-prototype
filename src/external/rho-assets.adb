with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Directories;
with Ada.Text_IO;

with Rho.Logging;

with Rho.Object;
with Rho.String_Maps;

with Rho.Materials.Loader;
with Rho.Mesh.Reader;
with Rho.Shader.Load;
with Rho.Texture.Loader;

package body Rho.Assets is

   package Search_Path_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package Asset_Map is
     new Rho.String_Maps (Rho.Object.Rho_Object,
                         Rho.Object."=");

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package Resource_Folder_Map is
      new Rho.String_Maps (String_Lists.List, String_Lists."=");

   Search_Paths : Search_Path_Lists.List;
   Image_Paths  : Search_Path_Lists.List;
   Assets       : Asset_Map.Map;
   Folders      : Resource_Folder_Map.Map;

   function Get_Asset
     (Class_Name : String;
      Name       : String;
      Loader     : not null access
        function (Path : String) return Rho.Object.Rho_Object)
      return Rho.Object.Rho_Object;

   function To_File_Name
     (Class_Name : String;
      Asset_Name : String)
      return String;

   ---------------------
   -- Add_Folder_Name --
   ---------------------

   procedure Add_Folder_Name
     (Class_Name  : String;
      Folder_Name : String)
   is
   begin
      if not Folders.Contains (Class_Name) then
         Folders.Insert (Class_Name, String_Lists.Empty_List);
      end if;
      Folders (Class_Name).Append (Folder_Name);
   end Add_Folder_Name;

   --------------------
   -- Add_Image_Path --
   --------------------

   procedure Add_Image_Path
     (Path : String)
   is
   begin
      Image_Paths.Append (Path);
   end Add_Image_Path;

   ---------------------
   -- Add_Search_Path --
   ---------------------

   procedure Add_Search_Path
     (Path : String)
   is
   begin
      Search_Paths.Append (Path);
   end Add_Search_Path;

   ---------------
   -- Get_Asset --
   ---------------

   function Get_Asset
     (Class_Name : String;
      Name       : String;
      Loader     : not null access
        function (Path : String) return Rho.Object.Rho_Object)
      return Rho.Object.Rho_Object
   is
      Key : constant String := "[" & Class_Name & "] " & Name;

      function EMaassting_File_Path
        (Base_Path : String;
         File_Name : String)
         return String;

      ------------------------
      -- EMaassting_File_Path --
      ------------------------

      function EMaassting_File_Path
        (Base_Path : String;
         File_Name : String)
         return String
      is
         use Ada.Directories;
      begin
         if Folders.Contains (Class_Name) then
            for Folder_Name of Folders (Class_Name) loop
               declare
                  Path : constant String :=
                           Compose
                             (Compose (Base_Path, Folder_Name),
                              File_Name);
               begin
                  if Exists (Path) then
                     return Path;
                  end if;
               end;
            end loop;
         end if;
         return "";
      end EMaassting_File_Path;

   begin

      if Assets.Contains (Key) then
         return Assets.Element (Key);
      end if;

      declare
         File_Name : constant String := To_File_Name (Class_Name, Name);
      begin
         for Path of Search_Paths loop
            declare
               File_Path : constant String :=
                             EMaassting_File_Path (Path, File_Name);
            begin
               if File_Path /= "" then
                  declare
                     use type Rho.Object.Rho_Object;
                     Result : constant Rho.Object.Rho_Object :=
                                Loader (File_Path);
                  begin
                     if Result /= null then
                        Assets.Insert (Key, Result);
                        return Result;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end;

      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "Warning: " & Class_Name & " asset '" & Name & "' not defined");

      if Name = "default" then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Warning: no default " & Class_Name & " asset found");
         Assets.Insert ("[" & Class_Name & "] default", null);
         return null;
      else
         return Get_Asset (Class_Name, "default", Loader);
      end if;

   end Get_Asset;

   ----------------
   -- Image_Path --
   ----------------

   function Image_Path
     (Relative_Path : String)
      return String
   is
   begin
      for Path of Image_Paths loop
         declare
            Full_Path : constant String :=
                          Path & "/" & Relative_Path;
         begin
            if Ada.Directories.Exists (Full_Path) then
               return Full_Path;
            end if;
         end;
      end loop;
      Rho.Logging.Put_Line
         ("Cannot find image at " & Relative_Path);
      return "";
   end Image_Path;

   --------------
   -- Material --
   --------------

   function Material
     (Name : String)
      return Rho.Materials.Material.Rho_Material
   is
      function Load (Path : String) return Rho.Object.Rho_Object
      is (Rho.Object.Rho_Object (Rho.Materials.Loader.Load (Path)));

   begin
      return Rho.Materials.Material.Rho_Material
        (Get_Asset (Rho.Materials.Material.Rho_Material_Class_Name,
         Name, Load'Access));
   end Material;

   ----------
   -- Mesh --
   ----------

   function Mesh
     (Name : String)
      return Rho.Mesh.Rho_Mesh
   is
      function Load (Path : String) return Rho.Object.Rho_Object
      is (Rho.Object.Rho_Object (Rho.Mesh.Reader.Load (Path)));

   begin
      return Rho.Mesh.Rho_Mesh
        (Get_Asset (Rho.Mesh.Rho_Mesh_Class_Name, Name, Load'Access));
   end Mesh;

   ------------
   -- Shader --
   ------------

   function Shader
     (Name : String)
      return Rho.Shader.Rho_Shader
   is

      function Load (Path : String) return Rho.Object.Rho_Object;

      ----------
      -- Load --
      ----------

      function Load (Path : String) return Rho.Object.Rho_Object is
         Base_Name : constant String :=
                       Ada.Directories.Simple_Name (Path);
         Shader    : constant Rho.Shader.Rho_Shader :=
                       Rho.Shader.Load.Load (Base_Name, Base_Name);
      begin
         return Rho.Object.Rho_Object (Shader);
      end Load;

   begin
      return Rho.Shader.Rho_Shader
        (Get_Asset (Rho.Shader.Rho_Shader_Class_Name, Name, Load'Access));
   end Shader;

   -------------
   -- Texture --
   -------------

   function Texture
     (Name : String)
      return Rho.Texture.Rho_Texture
   is
      function Load (Path : String) return Rho.Object.Rho_Object
      is (Rho.Object.Rho_Object (Rho.Texture.Loader.Load_Texture (Path)));

   begin
      return Rho.Texture.Rho_Texture
        (Get_Asset (Rho.Texture.Rho_Texture_Class_Name, Name, Load'Access));
   end Texture;

   ------------------
   -- To_File_Name --
   ------------------

   function To_File_Name
     (Class_Name : String;
      Asset_Name : String)
      return String
   is
      use Ada.Characters.Handling;
      Result : String := To_Lower (Asset_Name);
   begin
      for I in Result'Range loop
         if Result (I) = '.' or else Result (I) = '/' then
            Result (I) := '-';
         end if;
      end loop;
      return Result & "." & To_Lower (Class_Name);
   end To_File_Name;

end Rho.Assets;
