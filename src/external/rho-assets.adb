with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Text_IO;

with Rho.Logging;

with Rho.Materials.Loader;
with Rho.Mesh.Reader;
with Rho.Shaders.Loader;
with Rho.Shaders.Program;
with Rho.Texture.Loader;

with Rho.Context;

package body Rho.Assets is

   function Get_Asset
     (Handle     : in out Rho_Asset_Handle;
      Class_Name : String;
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
     (Handle      : in out Rho_Asset_Handle;
      Class_Name  : String;
      Folder_Name : String)
   is
   begin
      if not Handle.Folders.Contains (Class_Name) then
         Handle.Folders.Insert (Class_Name, String_Lists.Empty_List);
      end if;
      Handle.Folders (Class_Name).Append (Folder_Name);
   end Add_Folder_Name;

   --------------------
   -- Add_Image_Path --
   --------------------

   procedure Add_Image_Path
     (Handle : in out Rho_Asset_Handle;
      Path   : String)
   is
   begin
      Handle.Image_Paths.Append (Path);
   end Add_Image_Path;

   ---------------------
   -- Add_Search_Path --
   ---------------------

   procedure Add_Search_Path
     (Handle : in out Rho_Asset_Handle;
      Path   : String)
   is
   begin
      Handle.Search_Paths.Append (Path);
   end Add_Search_Path;

   procedure Create_Handle
     (Handle   : in out Rho_Asset_Handle;
      Context  : not null access Rho.Context.Rho_Context_Record'Class)
   is
   begin
      Handle.Context := Context;
   end Create_Handle;

   ---------------
   -- Get_Asset --
   ---------------

   function Get_Asset
     (Handle     : in out Rho_Asset_Handle;
      Class_Name : String;
      Name       : String;
      Loader     : not null access
        function (Path : String) return Rho.Object.Rho_Object)
      return Rho.Object.Rho_Object
   is
      Key : constant String := "[" & Class_Name & "] " & Name;

      function Existing_File_Path
        (Base_Path : String;
         File_Name : String)
         return String;

      ------------------------
      -- Existing_File_Path --
      ------------------------

      function Existing_File_Path
        (Base_Path : String;
         File_Name : String)
         return String
      is
         use Ada.Directories;
      begin
         if Handle.Folders.Contains (Class_Name) then
            for Folder_Name of Handle.Folders (Class_Name) loop
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
      end Existing_File_Path;

   begin

      if Handle.Assets.Contains (Key) then
         return Handle.Assets.Element (Key);
      end if;

      declare
         File_Name : constant String := To_File_Name (Class_Name, Name);
      begin
         for Path of Handle.Search_Paths loop
            declare
               File_Path : constant String :=
                             Existing_File_Path (Path, File_Name);
            begin
               if File_Path /= "" then
                  declare
                     use type Rho.Object.Rho_Object;
                     Result : constant Rho.Object.Rho_Object :=
                                Loader (File_Path);
                  begin
                     if Result /= null then
                        Handle.Assets.Insert (Key, Result);
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
         Handle.Assets.Insert ("[" & Class_Name & "] default", null);
         return null;
      else
         return Handle.Get_Asset (Class_Name, "default", Loader);
      end if;

   end Get_Asset;

   ----------------
   -- Image_Path --
   ----------------

   function Image_Path
     (Handle        : Rho_Asset_Handle;
      Relative_Path : String)
      return String
   is
   begin
      for Path of Handle.Image_Paths loop
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
     (Handle : in out Rho_Asset_Handle;
      Name   : String)
      return Rho.Materials.Material.Rho_Material
   is
      function Load (Path : String) return Rho.Object.Rho_Object
      is (Rho.Object.Rho_Object
          (Rho.Materials.Loader.Load
           (Handle.Context, Path)));

   begin
      return Rho.Materials.Material.Rho_Material
        (Handle.Get_Asset (Rho.Materials.Material.Rho_Material_Class_Name,
         Name, Load'Access));
   end Material;

   ----------
   -- Mesh --
   ----------

   function Mesh
     (Handle : in out Rho_Asset_Handle;
      Name   : String)
      return Rho.Mesh.Rho_Mesh
   is
      function Load (Path : String) return Rho.Object.Rho_Object
      is (Rho.Object.Rho_Object
          (Rho.Mesh.Reader.Load (Handle.Context, Path)));

   begin
      return Rho.Mesh.Rho_Mesh
        (Handle.Get_Asset (Rho.Mesh.Rho_Mesh_Class_Name, Name, Load'Access));
   end Mesh;

   ------------
   -- Shader --
   ------------

   function Shader
     (Handle : in out Rho_Asset_Handle;
      Name   : String)
      return Rho.Shaders.Rho_Shader
   is

      function Load (Path : String) return Rho.Object.Rho_Object;

      ----------
      -- Load --
      ----------

      function Load (Path : String) return Rho.Object.Rho_Object is
         Base_Name : constant String :=
                       Ada.Directories.Simple_Name (Path);
         Program   : constant Rho.Shaders.Rho_Shader :=
                       Rho.Shaders.Loader.Load
                         (Handle.Context, Base_Name, Base_Name);
      begin
         return Rho.Object.Rho_Object (Program);
      end Load;

   begin
      return Rho.Shaders.Rho_Shader
        (Handle.Get_Asset
           (Rho.Shaders.Program.Rho_Program_Class_Name,
            Name, Load'Access));
   end Shader;

   -------------
   -- Texture --
   -------------

   function Texture
     (Handle : in out Rho_Asset_Handle;
      Name   : String)
      return Rho.Texture.Rho_Texture
   is
      function Load (Path : String) return Rho.Object.Rho_Object
      is (Rho.Object.Rho_Object
          (Rho.Texture.Loader.Load_Texture
           (Handle.Context, Path)));

   begin
      return Rho.Texture.Rho_Texture
        (Handle.Get_Asset
           (Rho.Texture.Rho_Texture_Class_Name,
            Name, Load'Access));
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
