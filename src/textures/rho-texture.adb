with Ada.Text_IO;

with Cairo.Image_Surface;
with Cairo.Png;
with Cairo.Surface;

with Rho.Context;
with Rho.Rendering;

package body Rho.Texture is

   ------------
   -- Create --
   ------------

   function Create
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Name    : String)
      return Rho_Texture
   is
   begin
      return Result : constant Rho_Texture := new Rho_Texture_Record do
         Result.Context := Context;
         Result.Set_Name (Name);
         Result.Id := 0;
         Result.S_Wrap := Wrap;
         Result.T_Wrap := Wrap;
         Result.Mag_Filter := Linear;
         Result.Surface := Cairo.Null_Surface;
      end return;
   end Create;

   ----------------------
   -- Create_From_Data --
   ----------------------

   function Create_From_Data
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Name    : String;
      Data    : Rho.Color.Rho_Color_1D_Array)
      return Rho_Texture
   is
      Data_2D : Rho.Color.Rho_Color_2D_Array (Data'Range, 1 .. 1);
   begin
      for I in Data'Range loop
         Data_2D (I, 1) := Data (I);
      end loop;
      return Create_From_Data (Context, Name, Data_2D);
   end Create_From_Data;

   ----------------------
   -- Create_From_Data --
   ----------------------

   function Create_From_Data
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Name    : String;
      Data : Rho.Color.Rho_Color_2D_Array)
      return Rho_Texture
   is
   begin
      return Result : constant Rho_Texture := new Rho_Texture_Record do
         Result.Context := Context;
         Result.Set_Name (Name);
         Result.Id := 0;
         Result.S_Wrap := Wrap;
         Result.T_Wrap := Wrap;
         Result.Mag_Filter := Nearest;
         Result.Data := new Rho.Color.Rho_Color_2D_Array'(Data);
      end return;
   end Create_From_Data;

   -----------------------
   -- Create_From_Image --
   -----------------------

   function Create_From_Image
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Name    : String;
      Image : WL.Images.Image_Type)
      return Rho_Texture
   is
   begin
      return Result : constant Rho_Texture := new Rho_Texture_Record do
         Result.Context := Context;
         Result.Set_Name (Name);
         Result.Id := 0;
         Result.S_Wrap := Wrap;
         Result.T_Wrap := Wrap;
         Result.Mag_Filter := Nearest;
         Result.Data :=
           new Rho.Color.Rho_Color_2D_Array (1 .. Positive (Image.Width),
                                           1 .. Positive (Image.Height));
         for Y in 1 .. Image.Height loop
            for X in 1 .. Image.Width loop
               declare
                  Img_Color : constant WL.Images.Image_Color :=
                                Image.Color (X, Y);
               begin
                  Result.Data (Positive (X), Positive (Y)) :=
                    Rho.Color.Rho_Color'
                      (Red   => Rho_Float (Img_Color.Red) / 255.0,
                       Green => Rho_Float (Img_Color.Green) / 255.0,
                       Blue  => Rho_Float (Img_Color.Blue) / 255.0,
                       Alpha => Rho_Float (Img_Color.Alpha) / 255.0);
               end;
            end loop;
         end loop;
      end return;
   end Create_From_Image;

   ---------------------
   -- Create_From_Png --
   ---------------------

   function Create_From_Png
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Name    : String;
      Path : String)
      return Rho_Texture
   is
   begin
      Ada.Text_IO.Put_Line ("Texture: " & Path);
      return Result : constant Rho_Texture := new Rho_Texture_Record do
         Result.Context := Context;
         Result.Set_Name (Name);
         Result.Id := 0;
         Result.S_Wrap := Wrap;
         Result.T_Wrap := Wrap;
         Result.Mag_Filter := Nearest;
         Result.Surface :=
           Cairo.Png.Create_From_Png
             (Path);
         case Cairo.Surface.Status (Result.Surface) is
            when Cairo.Cairo_Status_Success =>
               Result.Region :=
                 (0.0, 0.0,
                  Rho_Float (Cairo.Image_Surface.Get_Width (Result.Surface)),
                  Rho_Float (Cairo.Image_Surface.Get_Height (Result.Surface)));

            when Cairo.Cairo_Status_File_Not_Found =>
               Ada.Text_IO.Put_Line ("Texture: " & Path);
               Ada.Text_IO.Put_Line ("file not found");
            when others =>
               Ada.Text_IO.Put_Line ("Texture: " & Path);
               Ada.Text_IO.Put_Line
                 ("unknown error "
                  & Cairo.Cairo_Status'Image
                 (Cairo.Surface.Status (Result.Surface)));
         end case;
      end return;
   end Create_From_Png;

   -------------------------
   -- Create_From_Surface --
   -------------------------

   function Create_From_Surface
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Surface : Cairo.Cairo_Surface)
      return Rho_Texture
   is
   begin
      return Result : constant Rho_Texture := new Rho_Texture_Record do
         Result.Context := Context;
         Result.Set_Name ("Surface");
         Result.Id := 0;
         Result.S_Wrap := Wrap;
         Result.T_Wrap := Wrap;
         Result.Mag_Filter := Nearest;
         Result.Surface := Surface;
         Result.Region :=
           (0.0, 0.0,
            Rho_Float (Cairo.Image_Surface.Get_Width (Result.Surface)),
            Rho_Float (Cairo.Image_Surface.Get_Height (Result.Surface)));
      end return;
   end Create_From_Surface;

   -----------------
   -- Has_Uniform --
   -----------------

   function Has_Uniform
     (Texture : Rho_Texture_Record)
      return Boolean
   is
   begin
      return Texture.Has_Uniform;
   end Has_Uniform;

   --------
   -- Id --
   --------

   function Id (Texture : Rho_Texture_Record) return Texture_Id is
   begin
      return Texture.Id;
   end Id;

   ----------
   -- Load --
   ----------

   procedure Load (Texture : in out Rho_Texture_Record) is
      use Cairo;
   begin
      if Texture.Id = 0 then
         Texture.Id :=
           Texture.Context.Renderer.Load_Texture
             (Texture.S_Wrap, Texture.T_Wrap,
              Texture.Mag_Filter);
      end if;

      if Texture.Data /= null then
         Texture.Context.Renderer.Load_Texture_Data
           (Texture.Id, Texture.Data.all);
      elsif Texture.Surface /= Null_Surface then
         Texture.Context.Renderer.Load_Texture_Data
           (Texture.Id, Texture.Surface, Texture.Region);
      end if;
   end Load;

   ------------
   -- Loaded --
   ------------

   function Loaded (Texture : Rho_Texture_Record) return Boolean is
   begin
      return Texture.Id /= 0;
   end Loaded;

   -----------------
   -- Set_Surface --
   -----------------

   procedure Set_Surface
     (Texture : in out Rho_Texture_Record;
      Surface : Cairo.Cairo_Surface;
      Region  : Rho.Rectangle.Rho_Rectangle)
   is
   begin
      Texture.Surface := Surface;
      Texture.Region := Region;
      if Texture.Id /= 0 then
         Texture.Context.Renderer.Load_Texture_Data
           (Texture.Id, Texture.Surface, Texture.Region);
      end if;
   end Set_Surface;

   -----------------
   -- Set_Uniform --
   -----------------

   procedure Set_Uniform
     (Texture : in out Rho_Texture_Record;
      Uniform : Rho.Shaders.Values.Rho_Uniform_Value)
   is
   begin
      Texture.Uniform := Uniform;
      Texture.Has_Uniform := True;
   end Set_Uniform;

   -------------
   -- Uniform --
   -------------

   function Uniform
     (Texture : Rho_Texture_Record)
      return Rho.Shaders.Values.Rho_Uniform_Value
   is
   begin
      return Texture.Uniform;
   end Uniform;

   ------------
   -- Unload --
   ------------

   procedure Unload (Texture : in out Rho_Texture_Record) is
      pragma Unreferenced (Texture);
   begin
      null;
   end Unload;

end Rho.Texture;
