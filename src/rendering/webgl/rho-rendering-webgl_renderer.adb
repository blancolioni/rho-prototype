with Ada.Unchecked_Deallocation;

with System.Storage_Elements;

with Rho.Rendering.WebGL_Renderer.WebGL_Window;

with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas.Context_WebGL;

with System;

with Glib;
with Cairo.Surface;
with Cairo.Image_Surface;

package body Rho.Rendering.WebGL_Renderer is

   use Gnoga.Gui.Element.Canvas.Context_WebGL;

   type Rho_WebGL_Renderer_Record is
     new Rho_Renderer_Record with
      record
         Parent_Element : Gnoga.Gui.Element.Common.DIV_Type;
         Top_Window     : WebGL_Window.Rho_WebGL_Window;
         Context        : Context_WebGL_Access;
         Active_Texture : Rho.Texture.Texture_Id;
      end record;

   type Rho_GL_Renderer is access all Rho_WebGL_Renderer_Record'Class;

   overriding function Current_Render_Target
     (Renderer : Rho_WebGL_Renderer_Record)
      return Rho.Render_Target.Rho_Render_Target
   is (Rho.Render_Target.Rho_Render_Target (Renderer.Top_Window));

   overriding procedure Render_Loop
     (Renderer : not null access Rho_WebGL_Renderer_Record);

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

   overriding function Load_Shader
     (Renderer : in out Rho_WebGL_Renderer_Record;
      Shader   : Rho.Shaders.Rho_Shader_Type;
      Source   : String)
      return Rho_Shader_Id;

   overriding procedure Use_Shader
     (Renderer : in out Rho_WebGL_Renderer_Record;
      Shader   : Rho_Program_Id);

   overriding function Create_Program
     (Renderer : in out Rho_WebGL_Renderer_Record;
      Shaders  : Rho.Shaders.Shader_Array)
      return Rho_Program_Id;

   overriding procedure Bind_Vertex_Buffer
     (Renderer       : in out Rho_WebGL_Renderer_Record;
      Attribute      : Rho_Attribute_Id;
      Buffer         : Rho.Float_Buffer.Rho_Float_Buffer;
      Start          : Positive;
      Component_Size : Positive);

   overriding function Get_Attribute_Location
     (Renderer : Rho_WebGL_Renderer_Record;
      Program  : Rho_Program_Id;
      Name     : String)
      return Rho_Attribute_Id;

   overriding function Get_Uniform_Location
     (Renderer : Rho_WebGL_Renderer_Record;
      Program  : Rho_Program_Id;
      Name     : String)
      return Rho_Uniform_Id;

   overriding procedure Set_Uniform_Value
     (Renderer   : in out Rho_WebGL_Renderer_Record;
      Id         : Rho_Uniform_Id;
      Value      : Integer);

   overriding procedure Set_Uniform_Value
     (Renderer   : in out Rho_WebGL_Renderer_Record;
      Id         : Rho_Uniform_Id;
      Value      : Rho_Float);

   overriding procedure Set_Uniform_Value
     (Renderer   : in out Rho_WebGL_Renderer_Record;
      Id         : Rho_Uniform_Id;
      Value      : Rho.Float_Arrays.Real_Vector);

   overriding procedure Set_Uniform_Value
     (Renderer   : in out Rho_WebGL_Renderer_Record;
      Id         : Rho_Uniform_Id;
      Value      : Rho.Float_Arrays.Real_Matrix);

   overriding procedure Set_Uniform_Value
     (Renderer   : in out Rho_WebGL_Renderer_Record;
      Id         : Rho_Uniform_Id;
      Value      : Integer_Array);

   overriding procedure Set_Uniform_Vector_Array
     (Renderer     : in out Rho_WebGL_Renderer_Record;
      Id           : Rho_Uniform_Id;
      Element_Size : Positive;
      Value        : Rho.Float_Arrays.Real_Vector);

   ------------------------
   -- Bind_Vertex_Buffer --
   ------------------------

   overriding procedure Bind_Vertex_Buffer
     (Renderer       : in out Rho_WebGL_Renderer_Record;
      Attribute      : Rho_Attribute_Id;
      Buffer         : Rho.Float_Buffer.Rho_Float_Buffer;
      Start          : Positive;
      Component_Size : Positive)
   is
   begin
      Renderer.Context.Bind_Buffer (GL_Array_Buffer, GLuint (Buffer.Id));
      Renderer.Context.Vertex_Attrib_Pointer
        (Index        => GLuint (Attribute),
         Size         => GLuint (Component_Size),
         Element_Type => GL_Float,
         Normalized   => False,
         Stride       => 0,
         Pointer      => System.Storage_Elements.Storage_Offset (Start));
      Renderer.Context.Enable_Vertex_Attrib_Array
        (GLuint (Attribute));
   end Bind_Vertex_Buffer;

   --------------------
   -- Create_Program --
   --------------------

   overriding function Create_Program
     (Renderer : in out Rho_WebGL_Renderer_Record;
      Shaders  : Rho.Shaders.Shader_Array)
      return Rho_Program_Id
   is
      GL : constant Context_WebGL_Access := Renderer.Context;
      Id : constant GLuint := GL.Create_Program;
   begin
      for Shader_Id of Shaders loop
         GL.Attach_Shader (Id, GLuint (Shader_Id));
      end loop;
      GL.Link_Program (Id);
      return Rho_Program_Id (Id);
   end Create_Program;

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
        WebGL_Window.Create_Top_Level_Window (Renderer.Parent_Element);

      Renderer.Top_Window.Set_Back_Face_Removal (True);
      Renderer.Top_Window.Set_Wireframe (False);
      Renderer.Top_Window.Set_Depth_Test (True);

      Renderer.Context := Renderer.Top_Window.Context;

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
   is null;

   ----------------------------
   -- Get_Attribute_Location --
   ----------------------------

   overriding function Get_Attribute_Location
     (Renderer : Rho_WebGL_Renderer_Record;
      Program  : Rho_Program_Id;
      Name     : String)
      return Rho_Attribute_Id
   is
   begin
      return Rho_Attribute_Id
        (Renderer.Context.Get_Attrib_Location
           (GLuint (Program), Name));
   end Get_Attribute_Location;

   --------------------------
   -- Get_Uniform_Location --
   --------------------------

   overriding function Get_Uniform_Location
     (Renderer : Rho_WebGL_Renderer_Record;
      Program  : Rho_Program_Id;
      Name     : String)
      return Rho_Uniform_Id
   is
   begin
      return Rho_Uniform_Id
        (Renderer.Context.Get_Uniform_Location
           (GLuint (Program), Name));
   end Get_Uniform_Location;

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
--        return Rho.Shaders.Rho_Program
--     is
--
--        Vertex_Shader   : constant Rho.Shaders.Rho_Shader :=
--                            Rho.Shaders.Load (Rho.Paths.Config_Path
--                                            & "/shaders/"
--                                            & Vertex_Shader_Path,
--                                            Rho.Shaders.Vertex);
--        Fragment_Shader : constant Rho.Shaders.Rho_Shader :=
--                            Rho.Shaders.Load (Rho.Paths.Config_Path
--                                            & "/shaders/"
--                                            & Fragment_Shader_Path,
--                                            Rho.Shaders.Fragment);
--        Result          : constant Rho.Shaders.Rho_Program :=
--                            Rho.Shaders.Create;
--     begin
--        Result.Add (Vertex_Shader);
--        Result.Add (Fragment_Shader);
--
--        Result.Compile;
--
--        return Result;
--
--     end Load_Program;

   -----------------
   -- Load_Shader --
   -----------------

   overriding function Load_Shader
     (Renderer : in out Rho_WebGL_Renderer_Record;
      Shader   : Rho.Shaders.Rho_Shader_Type;
      Source   : String)
      return Rho_Shader_Id
   is
      GL : constant Context_WebGL_Access := Renderer.Context;
      Id : constant GLuint :=
             GL.Create_Shader
               (case Shader is
                   when Rho.Shaders.Vertex_Shader   => GL_Vertex_Shader,
                   when Rho.Shaders.Fragment_Shader => GL_Fragment_Shader);
   begin

      GL.Shader_Source
        (Shader => Id,
         Source => Source);

      GL.Compile_Shader (Id);

      return Rho_Shader_Id (Id);
   end Load_Shader;

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
      pragma Unreferenced (Mag_Filter);
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
      pragma Unreferenced (To_GL_Filter);

      GL_S_Wrap     : constant Texture_Wrap_Mode := To_GL_Wrap (S_Wrap);
      GL_T_Wrap     : constant Texture_Wrap_Mode := To_GL_Wrap (T_Wrap);
--        GL_Mag_Filter : constant Texture_Mag_Filter :=
--                          To_GL_Filter (Mag_Filter);

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
      Width : constant Natural := From_Data'Length (1);
      Height : constant Natural := From_Data'Length (2);

      Dest_Data   : Uint8_Array (1 .. 4 * Width * Height);
   begin
      if Height = 0 or else Width = 0 then
         return;
      end if;

      for Y in 0 .. Height - 1 loop
         for X in 0 .. Width - 1 loop
            declare
               Src_X        : constant Positive := Positive (X + 1);
               Src_Y        : constant Positive := Positive (Y + 1);
               Dest_Index   : constant Positive :=
                                1 + 4 * X + (Height - Y - 1) * Width * 4;

               function To_Uint8 (X : Unit_Float) return GLuint8
               is (GLuint8 (X * 255.0));

            begin
               Dest_Data (Dest_Index) :=
                 To_Uint8 (From_Data (Src_X, Src_Y).Red);
               Dest_Data (Dest_Index + 1) :=
                 To_Uint8 (From_Data (Src_X, Src_Y).Green);
               Dest_Data (Dest_Index + 2) :=
                 To_Uint8 (From_Data (Src_X, Src_Y).Blue);
               Dest_Data (Dest_Index + 3) :=
                 To_Uint8 (From_Data (Src_X, Src_Y).Alpha);
            end;
         end loop;
      end loop;

      declare
         use type Rho.Texture.Texture_Id;
      begin
         if Renderer.Active_Texture /= Texture_Id then
            Renderer.Context.Bind_Texture (GL_Texture_2d, GLuint (Texture_Id));
            Renderer.Active_Texture := Texture_Id;
         end if;
      end;

      Renderer.Context.Tex_Image_2D
        (Target         => GL_Texture_2d,
         Level          => 0,
         Internalformat => GL_Rgba,
         Width          => GLsizei (Width),
         Height         => GLsizei (Height),
         Border         => 0,
         Format         => GL_Rgba,
         Item_Type      => GL_Unsigned_Byte,
         Pixels         => Dest_Data);

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

      Source_Data : Uint8_Array (1 .. Natural (Stride * Src_Height));
      for Source_Data'Address use Data;
      Dest_Data : Rho.Color.Rho_Color_2D_Array_Access;

      procedure Set_Dest (Source_Index : Positive;
                          X, Y         : Positive);

      --------------
      -- Set_Dest --
      --------------

      procedure Set_Dest (Source_Index : Positive;
                          X, Y         : Positive)
      is
         Red   : constant GLuint8 := Source_Data (Source_Index + 2);
         Green : constant GLuint8 := Source_Data (Source_Index + 1);
         Blue  : constant GLuint8 := Source_Data (Source_Index + 0);
         Alpha : constant GLuint8 := Source_Data (Source_Index + 3);
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
                  Set_Dest (Natural (Source_Index) + 1, X + 1, Y + 1);
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
     (Renderer : not null access Rho_WebGL_Renderer_Record)
   is null;

   ----------------------
   -- Render_One_Frame --
   ----------------------

   overriding procedure Render_One_Frame
     (Renderer : in out Rho_WebGL_Renderer_Record)
   is
   begin
      Renderer.Top_Window.Render;
   end Render_One_Frame;

   -----------------
   -- Render_Task --
   -----------------

   task body Render_Task_Type is
      Rho_Handle : Rho.Handles.Rho_Handle;
   begin
      accept Run (Handle : in Rho.Handles.Rho_Handle) do
         Rho_Handle := Handle;
      end Run;

      loop
         select
            accept Stop;
            exit;
         else
            delay 0.02;
            Rho_Handle.Render_One_Frame;
         end select;
      end loop;
   end Render_Task_Type;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   overriding procedure Set_Uniform_Value
     (Renderer   : in out Rho_WebGL_Renderer_Record;
      Id         : Rho_Uniform_Id;
      Value      : Integer)
   is
   begin
      Renderer.Context.Uniform (GLint (Id), GLint (Value));
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   overriding procedure Set_Uniform_Value
     (Renderer   : in out Rho_WebGL_Renderer_Record;
      Id         : Rho_Uniform_Id;
      Value      : Rho_Float)
   is
   begin
      Renderer.Context.Uniform (GLint (Id), GLfloat (Value));
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   overriding procedure Set_Uniform_Value
     (Renderer   : in out Rho_WebGL_Renderer_Record;
      Id         : Rho_Uniform_Id;
      Value      : Rho.Float_Arrays.Real_Vector)
   is
   begin
      Renderer.Set_Uniform_Vector_Array (Id, 1, Value);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   overriding procedure Set_Uniform_Value
     (Renderer   : in out Rho_WebGL_Renderer_Record;
      Id         : Rho_Uniform_Id;
      Value      : Integer_Array)
   is
      Vector : Int_Array (Value'Range);
   begin
      for I in Vector'Range loop
         Vector (I) := GLint (Value (I));
      end loop;
      Renderer.Context.Uniform (GLint (Id), Vector);
   end Set_Uniform_Value;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   overriding procedure Set_Uniform_Value
     (Renderer   : in out Rho_WebGL_Renderer_Record;
      Id         : Rho_Uniform_Id;
      Value      : Rho.Float_Arrays.Real_Matrix)
   is
      Matrix : Matrix_4;
   begin
      for I in Value'Range (1) loop
         for J in Value'Range (2) loop
            Matrix (I, J) := GLfloat (Value (I, J));
         end loop;
      end loop;

      Renderer.Context.Uniform_Matrix (GLint (Id), Matrix);
   end Set_Uniform_Value;

   ------------------------------
   -- Set_Uniform_Vector_Array --
   ------------------------------

   overriding procedure Set_Uniform_Vector_Array
     (Renderer     : in out Rho_WebGL_Renderer_Record;
      Id           : Rho_Uniform_Id;
      Element_Size : Positive;
      Value        : Rho.Float_Arrays.Real_Vector)
   is
      Vector : Float_Array (Value'Range);
      Loc    : constant GLint := GLint (Id);
   begin
      for I in Vector'Range loop
         Vector (I) := GLfloat (Value (I));
      end loop;
      case Element_Size is
         when 1 =>
            Renderer.Context.Uniform (Loc, Vector);
         when 2 =>
            Renderer.Context.Uniform_2v (Loc, Vector);
         when 3 =>
            Renderer.Context.Uniform_3v (Loc, Vector);
         when 4 =>
            Renderer.Context.Uniform_4v (Loc, Vector);
         when others =>
            raise Constraint_Error with
              "Set_Uniform_Vector_Array: invalid element size:"
              & Element_Size'Image;
      end case;

   end Set_Uniform_Vector_Array;

   ----------------
   -- Use_Shader --
   ----------------

   overriding procedure Use_Shader
     (Renderer : in out Rho_WebGL_Renderer_Record;
      Shader   : Rho_Program_Id)
   is
   begin
      Renderer.Context.Use_Program (GLuint (Shader));
   end Use_Shader;

   ------------------------
   -- WebGL_Event_Source --
   ------------------------

   function WebGL_Event_Source return Rho.Handles.Render_Event_Access is
   begin
      return new Render_Task_Type;
   end WebGL_Event_Source;

end Rho.Rendering.WebGL_Renderer;
