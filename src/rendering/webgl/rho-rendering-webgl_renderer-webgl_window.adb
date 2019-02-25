with Rho.Logging;
with Rho.Shaders.Program;

package body Rho.Rendering.WebGL_Renderer.WebGL_Window is

   Log_Operations : constant Boolean := False;

   ------------------
   -- After_Render --
   ------------------

   overriding procedure After_Render
     (Window : not null access Rho_WebGL_Window_Record)
   is
      pragma Unreferenced (Window);
   begin
      null;
   end After_Render;

   -------------------
   -- Before_Render --
   -------------------

   overriding procedure Before_Render
     (Window : not null access Rho_WebGL_Window_Record)
   is
   begin
      Window.Context.Clear ((GL_Color_Buffer_Bit, GL_Depth_Buffer_Bit));
   end Before_Render;

   ---------------------------
   -- Bind_Vertex_Attribute --
   ---------------------------

   overriding procedure Bind_Vertex_Attribute
     (Window          : not null access Rho_WebGL_Window_Record;
      Buffer          : Rho.Float_Buffer.Rho_Float_Buffer;
      Attribute       : Rho.Shaders.Values.Rho_Attribute_Value;
      Start           : Positive;
      Component_Size  : Positive;
      Is_Array        : Boolean)
   is
      pragma Unreferenced (Is_Array);
      use type Rho.Float_Buffer.Rho_Float_Buffer;
   begin
      if Window.Current_Buffer /= Buffer then
         Window.Current_Buffer := Buffer;
         Window.Context.Bind_Buffer (GL_Array_Buffer, GLuint (Buffer.Id));
         Window.Current_Vertex_Start := 0;
         Window.Current_Texture_Start := 0;
         Window.Current_Instance_Start := 0;
      end if;

      Window.Context.Vertex_Attrib_Pointer
        (Index        => GLuint (Attribute.Location),
         Size         => GLuint (Component_Size),
         Element_Type => GL_Float,
         Normalized   => False,
         Stride       => 0,
         Pointer      => Buffer.To_Offset (Start));
      Window.Context.Enable_Vertex_Attrib_Array
        (GLuint (Attribute.Location));

   end Bind_Vertex_Attribute;

   -----------
   -- Blend --
   -----------

   overriding procedure Blend
     (Window               : in out Rho_WebGL_Window_Record;
      Enabled              : Boolean;
      Source_Function      : Rho.Render_Target.Rho_Blend_Function :=
        Rho.Render_Target.One;
      Destination_Function : Rho.Render_Target.Rho_Blend_Function :=
        Rho.Render_Target.One)
   is
      use all type Rho.Render_Target.Rho_Blend_Function;

      function GL_Blend_Function
        (Fn : Rho.Render_Target.Rho_Blend_Function)
         return Blending_Factor
      is ((case Fn is
              when Zero => GL_Zero,
              when One  => GL_One,
              when Source_Alpha => GL_Src_Alpha,
              when Destination_Alpha => GL_Dst_Alpha,
              when One_Minus_Source_Alpha => GL_One_Minus_Src_Alpha,
              when One_Minus_Destination_Alpha =>
                 GL_One_Minus_Dst_Alpha));
   begin
      if Enabled then
         Window.Context.Enable (GL_Blend);
         Window.Context.Blend_Func (GL_Blend_Function (Source_Function),
                        GL_Blend_Function (Destination_Function));
      else
         Window.Context.Disable (GL_Blend);
      end if;
   end Blend;

   -----------------------------
   -- Create_Top_Level_Window --
   -----------------------------

   function Create_Top_Level_Window
     (Parent : in out Gnoga.Gui.Element.Element_Type'Class)
      return Rho_WebGL_Window
   is
      Result : constant Rho_WebGL_Window :=
                 new Rho_WebGL_Window_Record;
      Width  : constant Natural := Parent.Width;
      Height : constant Natural := Parent.Height;
   begin

      Result.Canvas.Create (Parent, Parent.Width, Parent.Height);
      Result.Context := new Context_WebGL_Type;
      Result.Context.Get_Drawing_Context_WebGL (Result.Canvas);
      Result.Rectangle := (0.0, 0.0, Rho_Float (Width), Rho_Float (Height));
      return Result;

   end Create_Top_Level_Window;

   -----------------
   -- Draw_Buffer --
   -----------------

   overriding procedure Draw_Buffer
     (Window          : not null access Rho_WebGL_Window_Record;
      Buffer          : Rho.Float_Buffer.Rho_Float_Buffer;
      Operation       : Rho.Render_Operation.Operation_Type;
      Count           : Natural;
      Instance_Count  : Positive := 1)
   is

      pragma Unreferenced (Instance_Count);

      use type Rho.Float_Buffer.Rho_Float_Buffer;

      use Rho.Render_Operation;

      To_GL_Operation : constant array (Operation_Type) of Primitive_Type :=
                          (Point_List     => GL_Points,
                           Line_List      => GL_Lines,
                           Line_Strip     => GL_Line_Strip,
                           Triangle_List  => GL_Triangles,
                           Triangle_Strip => GL_Triangle_Strip,
                           Triangle_Fan   => GL_Triangle_Fan);
   begin

      if Buffer /= Window.Current_Buffer then
         Window.Current_Buffer := Buffer;
      end if;

      Window.Context.Draw_Arrays
        (Mode  => To_GL_Operation (Operation),
         First => 0,
         Count => GLsizei (Count));

--        if Instance_Count = 1 then
--           Window.Context.Draw_Arrays
--             (Mode  => To_GL_Operation (Operation),
--              First => 0,
--              Count => GLsizei (Count));
--        else
--           Window.Context.Draw_Arrays_Instanced
--             (Mode            => To_GL_Operation (Operation),
--              First           => 0,
--              Count           => Sizei (Count),
--              Primitive_Count => Sizei (Instance_Count));
--        end if;

   end Draw_Buffer;

   -----------------------
   -- Enable_Point_Size --
   -----------------------

   overriding procedure Enable_Point_Size
     (Window  : in out Rho_WebGL_Window_Record;
      Enabled : Boolean)
   is null;

--     begin
--        if Enabled then
--           Window.Context.Enable (GL_VERTEX_PROGRAM_POINT_SIZE_ARB);
--        else
--           Window.Context.Disable (GL_VERTEX_PROGRAM_POINT_SIZE_ARB);
--        end if;
--     end Enable_Point_Size;

   -------------------
   -- Get_Rectangle --
   -------------------

   overriding function Get_Rectangle
     (Item : Rho_WebGL_Window_Record)
      return Rho.Rectangle.Rho_Rectangle
   is
   begin
      return Item.Rectangle;
   end Get_Rectangle;

   -----------------
   -- Save_Matrix --
   -----------------

   overriding procedure Save_Matrix
     (Window : not null access Rho_WebGL_Window_Record;
      Matrix : in Rho.Matrices.Matrix_Mode_Type)
   is
   begin
      Window.Activate_Shader;

      declare
         Shader : constant Rho.Shaders.Program.Rho_Program :=
                    Rho.Shaders.Program.Rho_Program
                      (Window.Current_Shader);
      begin
         Shader.Uniform_Matrix_Value (Matrix).Set_Value
           (Window.Current (Matrix));
      end;

      Window.Set_Matrix_Saved (Matrix);

   end Save_Matrix;

   ---------------------------
   -- Set_Back_Face_Removal --
   ---------------------------

   overriding procedure Set_Back_Face_Removal
     (Window : in out Rho_WebGL_Window_Record;
      Enabled : Boolean)
   is
   begin
      if Enabled then
         Window.Context.Enable (GL_Cull_Face);
      else
         Window.Context.Disable (GL_Cull_Face);
      end if;
   end Set_Back_Face_Removal;

   ---------------------
   -- Set_Clear_Color --
   ---------------------

   overriding procedure Set_Clear_Color
     (Window : in out Rho_WebGL_Window_Record;
      Color  : Rho.Color.Rho_Color)
   is
   begin
      Rho.Render_Window.Rho_Render_Window_Record (Window)
        .Set_Clear_Color (Color);
      Window.Context.Clear_Color
        (GLfloat (Color.Red),
         GLfloat (Color.Green),
         GLfloat (Color.Blue),
         GLfloat (Color.Alpha));
   end Set_Clear_Color;

   --------------------
   -- Set_Depth_Test --
   --------------------

   overriding procedure Set_Depth_Test
     (Window  : in out Rho_WebGL_Window_Record;
      Enabled : Boolean)
   is
   begin
      Rho.Render_Window.Rho_Render_Window_Record (Window).Set_Depth_Test
        (Enabled);
      if Enabled then
         Window.Context.Enable (GL_Depth_Test);
         Window.Context.Depth_Func (GL_Greater);
         Window.Context.Clear_Depth (0.0);
      else
         Window.Context.Disable (GL_Depth_Test);
      end if;
   end Set_Depth_Test;

   ---------------------
   -- Set_Full_Screen --
   ---------------------

   overriding procedure Set_Full_Screen
     (Window      : in out Rho_WebGL_Window_Record;
      Full_Screen : Boolean)
   is null;

   -------------------------
   -- Set_Output_Position --
   -------------------------

   overriding procedure Set_Output_Position
     (Window             : not null access Rho_WebGL_Window_Record;
      X, Y               : Rho_Float)
   is null;

   -------------------------
   -- Set_Raster_Position --
   -------------------------

   overriding procedure Set_Raster_Position
     (Window             : not null access Rho_WebGL_Window_Record;
      Position           : Rho.Matrices.Vector_3)
   is null;

   -------------------
   -- Set_Rectangle --
   -------------------

   overriding procedure Set_Rectangle
     (Item  : in out Rho_WebGL_Window_Record;
      Rectangle : Rho.Rectangle.Rho_Rectangle)
   is
   begin
      Item.Rectangle := Rectangle;
--        Item.Top_Window.Set_Default_Size
--          (Glib.Gint (Rectangle.Width), Glib.Gint (Rectangle.Height));
      Item.Full_Viewport.Set_Size (Rectangle.Width, Rectangle.Height);
   end Set_Rectangle;

   -----------------
   -- Set_Texture --
   -----------------

   overriding procedure Set_Texture
     (Window  : not null access Rho_WebGL_Window_Record;
      Texture : Rho.Texture.Rho_Texture)
   is
      use type Rho.Texture.Rho_Texture;
   begin
      if Texture /= Window.Texture then
         Window.Texture := Texture;
         if Texture = null then
            if Window.Texture_Enabled then
               Window.Context.Disable (GL_Texture_2d);
               Window.Texture_Enabled := False;
            end if;
         else
            if not Window.Texture_Enabled then
               Window.Context.Enable (GL_Texture_2d);
               Window.Texture_Enabled := True;
            end if;

            if not Texture.Loaded then
               Texture.Load;
            else
               Window.Context.Bind_Texture
                 (GL_Texture_2d, GLuint (Texture.Id));
            end if;

            if Texture.Has_Uniform then
               Texture.Uniform.Set_Value (0);
            end if;

         end if;
      end if;
   end Set_Texture;

   ----------------------
   -- Set_Vertex_Array --
   ----------------------

--     overriding procedure Set_Vertex_Array
--       (Window       : not null access Rho_WebGL_Window_Record;
--        Vertex_Array : Rho.Vertex_Array.Rho_Vertex_Array;
--        Operation    : Rho.Render_Operation.Operation_Type :=
--          Rho.Render_Operation.Triangle_List)
--     is
--        use Rho.Render_Operation;
--        pragma Unreferenced (Window);
--
--        To_GL_Operation : constant array (Operation_Type) of GLenum :=
--                            (Point_List     => GL_POINTS,
--                             Line_List      => GL_LINES,
--                             Line_Strip     => GL_LINE_STRIP,
--                             Triangle_List  => GL_TRIANGLES,
--                             Triangle_Strip => GL_TRIANGLE_STRIP,
--                             Triangle_Fan   => GL_TRIANGLE_FAN,
--                             Quad_List      => GL_QUADS);
--     begin
--        Window.Context.Vertex_Attribute_Pointer
--          (Index        => 0,
--           Size         => 3,
--           Element_Type => GL_FLOAT,
--           Normalized   => GL_FALSE,
--           Stride       => 0,
--           Pointer      => 0);
--
--        Window.Context.Enable_Vertex_Attribute_Array (0);
--
--        Window.Context.Bind_Vertex_Array (GLuint (Vertex_Array.Id));
--
--        Window.Context.Draw_Arrays
--          (Mode  => To_GL_Operation (Operation),
--           First => 0,
--           Count => Sizei (Vertex_Array.Count));
--
--     end Set_Vertex_Array;

   ------------------
   -- Set_Viewport --
   ------------------

   overriding procedure Set_Viewport
     (Window   : in out Rho_WebGL_Window_Record;
      Viewport : in Rho.Viewport.Rho_Viewport)
   is
   begin
      if Log_Operations then
         Rho.Logging.Put ("Window.Context.Viewport ");
         Rho.Logging.Put (Rho.Matrices.Vector_4'
                           (Viewport.X, Viewport.Y,
                            Viewport.Width, Viewport.Height));
         Rho.Logging.New_Line;
      end if;

      Rho.Render_Window.Rho_Render_Window_Record (Window).Set_Viewport
        (Viewport);
      Window.Context.Viewport (GLint (Viewport.X), GLint (Viewport.Y),
                   GLsizei (Viewport.Width), GLsizei (Viewport.Height));
   end Set_Viewport;

   -------------------
   -- Set_Wireframe --
   -------------------

   overriding procedure Set_Wireframe
     (Window : in out Rho_WebGL_Window_Record;
      Enabled : Boolean)
   is null;

   -------------------------
   -- Uniform_Float_Array --
   -------------------------

   overriding procedure Uniform_Float_Array
     (Window   : not null access Rho_WebGL_Window_Record;
      Uniform  : Rho.Shaders.Values.Rho_Uniform_Value;
      Value    : Rho.Float_Arrays.Real_Vector)
   is
   begin
      Window.Activate_Shader;
      Uniform.Set_Value (Value);
   end Uniform_Float_Array;

end Rho.Rendering.WebGL_Renderer.WebGL_Window;
