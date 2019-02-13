with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Text_IO;

with WL.Bitmap_IO;

with Tropos.Reader;

with Rho.Assets;

package body Rho.Texture.Loader is

   function Load_Texture_File
     (Path : String)
      return Rho_Texture;

   function Load_From_Image_File
     (Name : String;
      Path : String)
      return Rho_Texture;

   function Load_From_Bitmap
     (Name : String;
      Path : String)
      return Rho_Texture;

   function Load_From_Png
     (Name : String;
      Path : String)
      return Rho_Texture;

   ----------------------
   -- Load_From_Bitmap --
   ----------------------

   function Load_From_Bitmap
     (Name : String;
      Path : String)
      return Rho_Texture
   is
      use WL.Bitmap_IO;
      Bitmap : Bitmap_Type;
      Data    : access Rho.Color.Rho_Color_2D_Array;
   begin
      Read (Bitmap, Path);
      Data := new Rho.Color.Rho_Color_2D_Array (1 .. Width (Bitmap),
                                              1 .. Height (Bitmap));
      for Y in Data'Range (2) loop
         for X in Data'Range (1) loop
            declare
               Pixel : constant Color_Type :=
                         WL.Bitmap_IO.Color (Bitmap, X - 1, Y - 1);
            begin
               Data (X, Y) :=
                 (Rho_Float (Pixel.R) / 255.0,
                  Rho_Float (Pixel.G) / 255.0,
                  Rho_Float (Pixel.B) / 255.0,
                  Rho_Float (Pixel.Alpha) / 255.0);
            end;
         end loop;
      end loop;

      return Create_From_Data (Name, Data.all);

   end Load_From_Bitmap;

   ------------------
   -- Load_Texture --
   ------------------

   function Load_From_Image_File
     (Name : String;
      Path : String)
      return Rho_Texture
   is
      Extension : constant String :=
                    Ada.Characters.Handling.To_Lower
                      (Ada.Directories.Extension (Path));
   begin
      if Extension = "png" then
         return Load_From_Png
           (Name, Path);
      elsif Extension = "bmp" then
         return Load_From_Bitmap
              (Name, Path);
      else
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "unknown texture file extension: " & Extension);
         return null;
      end if;
   end Load_From_Image_File;

   -------------------
   -- Load_From_Png --
   -------------------

   function Load_From_Png
     (Name : String;
      Path : String)
      return Rho_Texture
   is
   begin
      return Create_From_Png (Name, Path);
   end Load_From_Png;

   ------------------
   -- Load_Texture --
   ------------------

   function Load_Texture
     (Path : String)
      return Rho_Texture
   is
      Extension : constant String :=
                    Ada.Characters.Handling.To_Lower
                      (Ada.Directories.Extension (Path));
   begin
      if Extension = "texture" then
         return Load_Texture_File (Path);
      else
         return Load_From_Image_File
           (Ada.Directories.Base_Name (Path), Path);
      end if;
   end Load_Texture;

   -----------------------
   -- Load_Texture_File --
   -----------------------

   function Load_Texture_File
     (Path : String)
      return Rho_Texture
   is
      Texture_Config : constant Tropos.Configuration :=
                         Tropos.Reader.Read_Config (Path);
      Name           : constant String :=
                         Texture_Config.Get
                           ("name",
                            Ada.Directories.Base_Name (Path));
      Image_Raw_Path : constant String :=
                         Texture_Config.Get ("image", "");
      Image_Path     : constant String :=
                         Rho.Assets.Image_Path (Image_Raw_Path);
   begin
      if Image_Path = "" then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Image_Raw_Path & ": cannot find image");
         return null;
      else
         return Load_From_Image_File (Name, Image_Path);
      end if;
   end Load_Texture_File;

end Rho.Texture.Loader;
