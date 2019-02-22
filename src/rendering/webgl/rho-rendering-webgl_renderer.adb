with Ada.Unchecked_Deallocation;

with Rho.Rendering.WebGL_Renderer.WebGL_Window;

with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas.Context_WebGL;

with Interfaces.C;

with System;

with Glib;
with Cairo.Image_Surface;

package body Rho.Rendering.WebGL_Renderer is

   use Gnoga.Gui.Element.Canvas.Context_WebGL;

   task type Render_Task_Type is
      entry Start (Renderer : Rho_Renderer);
      entry Stop;
   end Render_Task_Type;

   type Render_Task_Access is access Render_Task_Type;

   type Rho_WebGL_Renderer_Record is
     new Rho_Renderer_Record with
      record
         Parent_Element : Gnoga.Gui.Element.Common.DIV_Type;
         Top_Window     : WebGL_Window.Rho_WebGL_Window;
         Context        : Context_WebGL_Access;
         Active_Texture : Rho.Texture.Texture_Id;
         Render_Task    : Render_Task_Access;
      end record;

   type Rho_GL_Renderer is access all Rho_WebGL_Renderer_Record'Class;

   overriding function Current_Render_Target
     (Renderer : Rho_WebGL_Renderer_Record)
      return Rho.Render_Target.Rho_Render_Target
   is (Rho.Render_Target.Rho_Render_Target (Renderer.Top_Window));

   overriding procedure Render_Loop
     (Renderer : in out Rho_WebGL_Renderer_Record);

   overriding procedure Exit_Render_Loop
     (Renderer : in out Rho_WebGL_Renderer_Record);

   overriding function Create_Top_Level_Window
     (Renderer : in out Rho_WebGL_Renderer_Record)
      return Rho.Render_Window.Rho_Render_Window;

   overriding procedure Render_One_Frame
     (Renderer : in out Rho_WebGL_Renderer_Record);

   overriding function Create_Surface
     (Renderer      : in out Rho_WebGL_Renderer_Record;
      Width, Height : Rho_Float)
      return Cairo.Cairo_Surface;

   overriding function Load_Texture
     (Renderer   : in out Rho_WebGL_Renderer_Record;
      S_Wrap     : Rho.Texture.Texture_Address_Mode;
      T_Wrap     : Rho.Texture.Texture_Address_Mode;
      Mag_Filter : Rho.Texture.Texture_Filter_Type)
      return Rho.Texture.Texture_Id;

   overriding procedure Load_Texture_Data
     (Renderer     : in out Rho_WebGL_Renderer_Record;
      Texture_Id   : Rho.Texture.Texture_Id;
      From_Surface : Cairo.Cairo_Surface;
      Region       : Rho.Rectangle.Rho_Rectangle);

   overriding procedure Load_Texture_Data
     (Renderer     : in out Rho_WebGL_Renderer_Record;
      Texture_Id   : Rho.Texture.Texture_Id;
      From_Data    : Rho.Color.Rho_Color_2D_Array);

   overriding function Load_Buffer
     (Renderer : in out Rho_WebGL_Renderer_Record;
      Buffer   : Rho.Float_Buffer.Rho_Float_Buffer_Record'Class)
      return Rho.Float_Buffer.Rho_Float_Buffer_Id;

   --------------------
   -- Create_Surface --
   --------------------

   overriding function Create_Surface
     (Renderer      : in out Rho_WebGL_Renderer_Record;
      Width, Height : Rho_Float)
      return Cairo.Cairo_Surface
   is
      pragma Unreferenced (Renderer);
   begin
      return Cairo.Image_Surface.Create
        (Format => Cairo.Image_Surface.Cairo_Format_ARGB32,
         Width  => Glib.Gint (Width),
         Height => Glib.Gint (Height));
   end Create_Surface;

   -----------------------------
   -- Create_Top_Level_Window --
   -----------------------------

   overriding function Create_Top_Level_Window
     (Renderer : in out Rho_WebGL_Renderer_Record)
      return Rho.Render_Window.Rho_Render_Window
   is
   begin

      Renderer.Top_Window :=
        WebGL_Window.Create_Top_Level_Window;

      Renderer.Top_Window.Set_Back_Face_Removal (True);
      Renderer.Top_Window.Set_Wireframe (False);
      Renderer.Top_Window.Set_Depth_Test (True);

      return Rho.Render_Window.Rho_Render_Window (Renderer.Top_Window);
   end Create_Top_Level_Window;

   -------------------------
   -- Create_GL_Renderer --
   -------------------------

   function Create_WebGL_Renderer
     (Parent : in out Gnoga.Gui.Element.Element_Type'Class)
      return Rho_Renderer
   is
      Result : constant Rho_GL_Renderer :=
                 new Rho_WebGL_Renderer_Record;
   begin
      Result.Parent_Element.Create
        (Parent  => Parent);
      Result.Parent_Element.Fill_Parent;
      return Rho_Renderer (Result);
   end Create_WebGL_Renderer;

   ----------------------
   -- Exit_Render_Loop --
   ----------------------

   overriding procedure Exit_Render_Loop
     (Renderer : in out Rho_WebGL_Renderer_Record)
   is
   begin
      Renderer.Render_Task.Stop;
   end Exit_Render_Loop;

   -----------------
   -- Load_Buffer --
   -----------------

   overriding function Load_Buffer
     (Renderer : in out Rho_WebGL_Renderer_Record;
      Buffer   : Rho.Float_Buffer.Rho_Float_Buffer_Record'Class)
      return Rho.Float_Buffer.Rho_Float_Buffer_Id
   is
      type Float_Array_Access is access Float_Array;
      procedure Free is
        new Ada.Unchecked_Deallocation (Float_Array, Float_Array_Access);

      Values    : Float_Array_Access := new Float_Array (1 .. Buffer.Count);
      Buffer_Id : GLuint;

   begin
      for I in Values'Range loop
         Values (I) := GLfloat (Buffer.Value (I));
      end loop;

      Buffer_Id := Renderer.Context.Create_Buffer;

      Renderer.Context.Bind_Buffer (GL_Array_Buffer, Buffer_Id);
      Renderer.Context.Buffer_Data
        (Target => GL_Array_Buffer,
         Data   => Values.all,
         Usage  => GL_Static_Draw);

      Free (Values);

--        GL.Vertex_Attribute_Pointer
--          (Index        => 0,
--           Size         => 3,
--           Element_Type => GL_FLOAT,
--           Normalized   => GL_FALSE,
--           Stride       => 0,
--           Pointer      => Buffer.To_Offset (Vertex_Start));
--        GL.Enable_Vertex_Attribute_Array (0);
--
--        if Texture_Start > 0 then
--           GL.Vertex_Attribute_Pointer
--             (Index        => 1,
--              Size         => 2,
--              Element_Type => GL_FLOAT,
--              Normalized   => GL_FALSE,
--              Stride       => 0,
--              Pointer      => Buffer.To_Offset (Texture_Start));
--           GL.Enable_Vertex_Attribute_Array (1);
--        end if;

      return Rho.Float_Buffer.Rho_Float_Buffer_Id (Buffer_Id);
   end Load_Buffer;

   ------------------
   -- Load_Program --
   ------------------

--     function Load_Program
--       (Vertex_Shader_Path   : String;
--        Fragment_Shader_Path : String)
--        return Rho.Shader.Rho_Program
--     is
--
--        Vertex_Shader   : constant Rho.Shader.Rho_Shader :=
--                            Rho.Shader.Load (Rho.Paths.Config_Path
--                                            & "/shaders/"
--                                            & Vertex_Shader_Path,
--                                            Rho.Shader.Vertex);
--        Fragment_Shader : constant Rho.Shader.Rho_Shader :=
--                            Rho.Shader.Load (Rho.Paths.Config_Path
--                                            & "/shaders/"
--                                            & Fragment_Shader_Path,
--                                            Rho.Shader.Fragment);
--        Result          : constant Rho.Shader.Rho_Program :=
--                            Rho.Shader.Create;
--     begin
--        Result.Add (Vertex_Shader);
--        Result.Add (Fragment_Shader);
--
--        Result.Compile;
--
--        return Result;
--
--     end Load_Program;

   ------------------
   -- Load_Texture --
   ------------------

   overriding function Load_Texture
     (Renderer     : in out Rho_WebGL_Renderer_Record;
      S_Wrap       : Rho.Texture.Texture_Address_Mode;
      T_Wrap       : Rho.Texture.Texture_Address_Mode;
      Mag_Filter   : Rho.Texture.Texture_Filter_Type)
      return Rho.Texture.Texture_Id
   is
      use Rho.Texture;

      GL : constant Context_WebGL_Access := Renderer.Context;

      Id           : GLuint;
      To_GL_Wrap        : constant array (Texture_Address_Mode) of
        Texture_Wrap_Mode :=
          (Border => GL_Repeat, Clamp => GL_Clamp_To_Edge,
           Mirror => GL_Clamp_To_Edge, Wrap => GL_Repeat);
      To_GL_Filter          : constant array (Texture_Filter_Type)
        of Texture_Mag_Filter :=
          (Nearest => GL_Nearest, Linear => GL_Linear);

      GL_S_Wrap     : constant Texture_Wrap_Mode := To_GL_Wrap (S_Wrap);
      GL_T_Wrap     : constant Texture_Wrap_Mode := To_GL_Wrap (T_Wrap);
      GL_Mag_Filter : constant Texture_Mag_Filter :=
                        To_GL_Filter (Mag_Filter);

      --        function Tex_Arg is
      --          new Ada.Unchecked_Conversion (GL.Texture_Wrap_Mode, GL.Int);
      --
      --        function Tex_Arg is
      --          new Ada.Unchecked_Conversion (GL.Texture_Mag_Filter, GL.Int);
      --
      --        function Tex_Arg is
      --          new Ada.Unchecked_Conversion (GL.Texture_Min_Filter, GL.Int);
      --
      --        Width : constant Gint :=
      --                  Cairo.Image_Surface.Get_Width (From_Surface);
      --        Height : constant Gint :=
      --                   Cairo.Image_Surface.Get_Height (From_Surface);

   begin
      Id := GL.Create_Texture;
      GL.Bind_Texture (GL_Texture_2d, Id);

      GL.Texture_Wrap_S (GL_Texture_2d, GL_S_Wrap);
      GL.Texture_Wrap_T (GL_Texture_2d, GL_T_Wrap);
--        GL.te (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_Mag_Filter);
--     GL.Tex_Parameter (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_Mag_Filter);

      Renderer.Active_Texture := Rho.Texture.Texture_Id (Id);
      return Renderer.Active_Texture;
   end Load_Texture;

   -----------------------
   -- Load_Texture_Data --
   -----------------------

   overriding procedure Load_Texture_Data
     (Renderer     : in out Rho_WebGL_Renderer_Record;
      Texture_Id   : Rho.Texture.Texture_Id;
      From_Data    : Rho.Color.Rho_Color_2D_Array)
   is
      Width : constant GLint := GLint (From_Data'Length (1));
      Height : constant GLint := GLint (From_Data'Length (2));

      Dest_Data   : Aliased_Ubyte_Array_Access;
   begin
      if Height = 0 or else Width = 0 then
         return;
      end if;

      Dest_Data :=
        new Aliased_Ubyte_Array (0 .. 4 * Width * Height - 1);

      for Y in 0 .. Height - 1 loop
         for X in 0 .. Width - 1 loop
            declare
               Src_X        : constant Positive := Positive (X + 1);
               Src_Y        : constant Positive := Positive (Y + 1);
               Dest_Index   : constant Int :=
                                4 * X + (Height - Y - 1) * Width * 4;

               function To_Ubyte (X : Unit_Float) return Ubyte
               is (Ubyte (X * 255.0));

            begin
               Dest_Data (Dest_Index) :=
                 To_Ubyte (From_Data (Src_X, Src_Y).Red);
               Dest_Data (Dest_Index + 1) :=
                 To_Ubyte (From_Data (Src_X, Src_Y).Green);
               Dest_Data (Dest_Index + 2) :=
                 To_Ubyte (From_Data (Src_X, Src_Y).Blue);
               Dest_Data (Dest_Index + 3) :=
                 To_Ubyte (From_Data (Src_X, Src_Y).Alpha);
            end;
         end loop;
      end loop;

      declare
         use type Rho.Texture.Texture_Id;
      begin
         if Renderer.Active_Texture /= Texture_Id then
            GL.Bind_Texture (GL_TEXTURE_2D, Uint (Texture_Id));
            Renderer.Active_Texture := Texture_Id;
         end if;
      end;

      GL.Tex_Image_2D
        (Target          => GL_TEXTURE_2D,
         Level           => 0,
         Internal_Format => GL_RGBA,
         Width           => Sizei (Width),
         Height          => Sizei (Height),
         Border          => 0,
         Format          => GL_RGBA,
         Ptype           => GL_UNSIGNED_BYTE,
         Pixels          => Dest_Data (0)'Address);

      Free (Dest_Data);

   end Load_Texture_Data;

   -----------------------
   -- Load_Texture_Data --
   -----------------------

   overriding procedure Load_Texture_Data
     (Renderer     : in out Rho_WebGL_Renderer_Record;
      Texture_Id   : Rho.Texture.Texture_Id;
      From_Surface : Cairo.Cairo_Surface;
      Region       : Rho.Rectangle.Rho_Rectangle)
   is
      use Glib;
      Data       : constant System.Address :=
                     Cairo.Image_Surface.Get_Data_Generic (From_Surface);
      Stride     : constant Glib.Gint :=
                     Cairo.Image_Surface.Get_Stride (From_Surface);
      Src_Width  : constant Gint :=
                     Cairo.Image_Surface.Get_Width (From_Surface);
      Src_Height : constant Gint :=
                     Cairo.Image_Surface.Get_Height (From_Surface);
      Start_X    : constant Gint := Gint (Region.X);
      Start_Y    : constant Gint := Gint (Region.Y);
      Width      : constant Natural := Natural (Region.Width);
      Height     : constant Natural := Natural (Region.Height);

      type Source_Data_Array is array (Gint range <>) of aliased Ubyte;
      Source_Data : Source_Data_Array (0 .. Stride * Src_Height - 1);
      for Source_Data'Address use Data;
      Dest_Data : Rho.Color.Rho_Color_2D_Array_Access;

      procedure Set_Dest (Source_Index : Gint;
                          X, Y         : Positive);

      --------------
      -- Set_Dest --
      --------------

      procedure Set_Dest (Source_Index : Gint;
                          X, Y         : Positive)
      is
         Red   : constant Ubyte := Source_Data (Source_Index + 2);
         Green : constant Ubyte := Source_Data (Source_Index + 1);
         Blue  : constant Ubyte := Source_Data (Source_Index + 0);
         Alpha : constant Ubyte := Source_Data (Source_Index + 3);
      begin
         Dest_Data (X, Y) :=
           (Red   => Rho_Float (Red) / 255.0,
            Green => Rho_Float (Green) / 255.0,
            Blue  => Rho_Float (Blue) / 255.0,
            Alpha => Rho_Float (Alpha) / 255.0);
      end Set_Dest;

   begin
      Cairo.Surface.Flush (From_Surface);

      if Height = 0 or else Width = 0 then
         return;
      end if;

      Dest_Data := new Rho.Color.Rho_Color_2D_Array (1 .. Width, 1 .. Height);

      for Y in 0 .. Height - 1 loop
         for X in 0 .. Width - 1 loop
            declare
               Src_X        : constant Gint := Gint (X) + Start_X;
               Src_Y        : constant Gint := Gint (Y) + Start_Y;
               Source_Index : constant Gint :=
                                (if Src_X < Src_Width
                                 and then Src_Y < Src_Height
                                 then 4 * Src_X + Src_Y * Stride
                                 else 0);
            begin
               if Src_X < Src_Width and then Src_Y < Src_Height then
                  Set_Dest (Source_Index, X + 1, Y + 1);
               else
                  Dest_Data (X + 1, Y + 1) := (0.0, 0.0, 0.0, 0.0);
               end if;
            end;
         end loop;
      end loop;

      Renderer.Load_Texture_Data
        (Texture_Id, Dest_Data.all);

      Rho.Color.Free (Dest_Data);

   end Load_Texture_Data;

   -----------------
   -- Render_Loop --
   -----------------

   overriding procedure Render_Loop
     (Renderer : in out Rho_WebGL_Renderer_Record)
   is
      pragma Unreferenced (Renderer);
   begin
      GLUT.Main_Loop;
   end Render_Loop;

   ----------------------
   -- Render_One_Frame --
   ----------------------

   overriding procedure Render_One_Frame
     (Renderer : in out Rho_WebGL_Renderer_Record)
   is
   begin
      Renderer.Top_Window.Render;
   end Render_One_Frame;

end Rho.Rendering.WebGL_Renderer;
