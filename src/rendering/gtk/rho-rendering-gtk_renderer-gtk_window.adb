with Cairo.Image_Surface;

with Glib;

with Gdk.Cairo;
with Gdk.Event;

with Gtk.Handlers;
with Gtk.Widget;

with Xi.Main;
with Xi.Render_Operation;

with Xi.Logging;

package body Xi.Rendering.Gtk_Renderer.Gtk_Window is

   Log_Vertices : constant Boolean := False;

   type Xi_Gtk_Render_Operator is
     new Xi.Render_Operation.Xi_Render_Operation_Record with
      record
         Window       : Xi_Gtk_Window;
         Operation    : Xi.Render_Operation.Operation_Type;
         Current_Face : Face_Type;
      end record;

   overriding procedure Begin_Operation
     (Item      : in out Xi_Gtk_Render_Operator;
      Operation : Xi.Render_Operation.Operation_Type);

   overriding procedure End_Operation
     (Item : in out Xi_Gtk_Render_Operator);

   overriding procedure Color
     (Item    : in out Xi_Gtk_Render_Operator;
      Color   : Xi.Color.Xi_Color)
   is null;

   overriding procedure Normal
     (Item    : in out Xi_Gtk_Render_Operator;
      Vector  : Xi.Matrices.Vector_3);

   overriding procedure Vertex
     (Item    : in out Xi_Gtk_Render_Operator;
      Vector  : Xi.Matrices.Vector_3);

   overriding procedure Texture_Coordinate
     (Item    : in out Xi_Gtk_Render_Operator;
      S, T    : Xi_Float);

   package Top_Window_Callback is
     new Gtk.Handlers.Callback (Gtk.Window.Gtk_Window_Record);

   package Drawing_Area_Callback is
     new Gtk.Handlers.User_Return_Callback
       (Widget_Type => Gtk.Drawing_Area.Gtk_Drawing_Area_Record,
        Return_Type => Boolean,
        User_Type   => Xi_Gtk_Window);

   procedure Destroy_Handler (W : access Gtk.Window.Gtk_Window_Record'Class);

   function Configure_Handler
     (W : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Window : Xi_Gtk_Window)
      return Boolean;

   function Expose_Handler
     (W : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Window : Xi_Gtk_Window)
      return Boolean;

   function Mouse_Button_Press_Handler
     (W      : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Window : Xi_Gtk_Window)
      return Boolean;

   function Mouse_Button_Release_Handler
     (W      : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Window : Xi_Gtk_Window)
      return Boolean;

   function Mouse_Move_Handler
     (W      : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Window : Xi_Gtk_Window)
      return Boolean;

   function Key_Press_Handler
     (W      : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Window : Xi_Gtk_Window)
      return Boolean
   is (False);

   function Key_Release_Handler
     (W      : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Window : Xi_Gtk_Window)
      return Boolean
   is (False);

   type Buffer_Point_Type is
      record
         X, Y : Xi_Float;
      end record;

   type Buffer_Points is array (Positive range <>) of Buffer_Point_Type;

   type Z_Buffered_Surface is
      record
         Z      : Xi_Float;
         Pts    : Buffer_Points (1 .. 50);
         Count  : Natural;
         Colour : Xi.Color.Xi_Color;
         Filled : Boolean;
      end record;

   package Z_Buffer_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Z_Buffered_Surface);

   Z_Buffer : Z_Buffer_Lists.List;

   procedure Clear_Z_Buffer;
   procedure Add_To_Z_Buffer
     (Z      : Xi_Float;
      Pts    : Buffer_Points;
      Colour : Xi.Color.Xi_Color;
      Filled : Boolean);

   procedure Draw_Z_Buffer
     (Context : Cairo.Cairo_Context;
      Active  : Boolean);

   procedure Draw_Polygon
     (Context  : Cairo.Cairo_Context;
      Vertices : Buffer_Points;
      Colour   : Xi.Color.Xi_Color;
      Filled   : Boolean);

   function Find_Centre
     (Face : Face_Type)
      return Xi.Matrices.Vector_4;

   ---------------------
   -- Add_To_Z_Buffer --
   ---------------------

   procedure Add_To_Z_Buffer
     (Z      : Xi_Float;
      Pts    : Buffer_Points;
      Colour : Xi.Color.Xi_Color;
      Filled : Boolean)
   is
      use Z_Buffer_Lists;
      Position : Cursor := Z_Buffer.First;
      S : Z_Buffered_Surface;
   begin
      S.Z := Z;
      S.Pts (Pts'Range) := Pts;
      S.Count := Pts'Length;
      S.Colour := Colour;
      S.Filled := Filled;

      while Has_Element (Position)
        and then Element (Position).Z < Z
      loop
         Next (Position);
      end loop;
      if Has_Element (Position) then
         Z_Buffer.Insert (Position, S);
      else
         Z_Buffer.Append (S);
      end if;
   end Add_To_Z_Buffer;

   ------------------
   -- After_Render --
   ------------------

   overriding procedure After_Render
     (Window : not null access Xi_Gtk_Window_Record)
   is
      Projection_Matrix : constant Xi.Matrices.Matrix_4 :=
                            Window.Current (Xi.Matrices.Projection);
   begin
      Clear_Z_Buffer;

      if Log_Vertices then
         Xi.Logging.Put_Line ("Projection:");
         Xi.Logging.Put_Line (Projection_Matrix);
      end if;

      for Face of Window.Faces loop
         declare
            use Matrices;
            use Xi.Float_Arrays;
            Pts : Buffer_Points (1 .. Natural (Face.Vs.Length));
            Vs  : array (Pts'Range) of Vector_3;
            Z_Coord : Xi_Float := 0.0;
            Count   : Natural := 0;
            Filled  : Boolean;
            Colour  : Xi.Color.Xi_Color := Face.Color;
         begin
            for Light of Window.Lights loop
               if Light.Enabled then
                  declare
                     Light_Vector_4 : constant Vector_4 :=
                                        Light.Position - Find_Centre (Face);
                     Light_Vector_3 : constant Vector_3 :=
                                        Light_Vector_4 (1 .. 3);
                     Norm_Light     : constant Vector_3 :=
                                        Light_Vector_3 / abs Light_Vector_3;
                     Light_Factor : constant Xi_Float :=
                                        (Face.Normal * Norm_Light + 1.0)
                                        / 2.0;
                  begin
                     Colour.Red := Colour.Red * Light_Factor;
                     Colour.Green := Colour.Green * Light_Factor;
                     Colour.Blue := Colour.Blue * Light_Factor;
                  end;
               end if;
            end loop;

            for Object_Vertex of Face.Vs loop
               Count := Count + 1;

               if Log_Vertices then
                  Xi.Logging.Put ("Vertex" & Positive'Image (Count) & ": ");
                  Xi.Logging.Put (Object_Vertex);
               end if;

               declare
                  Eye_Vertex : constant Vector_4 := Object_Vertex;
                  Clip_Vertex : constant Vector_4 :=
                                  Projection_Matrix * Eye_Vertex;
                  Norm_Vertex : constant Vector_3 :=
                                  Clip_Vertex (1 .. 3) / Clip_Vertex (4);
               begin

                  if Log_Vertices then
                     Xi.Logging.Put (" --> ");
                     Xi.Logging.Put (Eye_Vertex);
                     Xi.Logging.Put (" --> ");
                     Xi.Logging.Put (Clip_Vertex);
                     Xi.Logging.Put (" --> ");
                     Xi.Logging.Put (Norm_Vertex);
                     Xi.Logging.New_Line;
                  end if;

                  Vs (Count) := Norm_Vertex;

                  Pts (Count).X :=
                    Norm_Vertex (1) * Window.Viewport.Width / 2.0
                    + Window.Viewport.X + Window.Viewport.Width / 2.0;
                  Pts (Count).Y :=
                    Window.Viewport.Height -
                      (Norm_Vertex (2) * Window.Viewport.Height / 2.0
                       + Window.Viewport.Y + Window.Viewport.Height / 2.0);

                  Z_Coord := Z_Coord + Norm_Vertex (3);

               end;

            end loop;

            Z_Coord := Z_Coord / Xi_Float (Pts'Length);

            Filled := not Window.Wireframe and then Face.Filled;

            if not Window.Back_Face_Removal
              or else Vs'Length < 3
            then
               Add_To_Z_Buffer
                 (Z_Coord, Pts, Colour, Filled);
            else
               declare
                  A : constant Vector_3 := Vs (2) - Vs (1);
                  B : constant Vector_3 := Vs (3) - Vs (2);
                  Z : constant Xi_Float :=
                        A (1) * B (2) - A (2) * B (1);
               begin
                  if Z >= 0.0 then
                     Add_To_Z_Buffer
                       (Z_Coord, Pts, Colour, Filled);
                  end if;
               end;
            end if;
         end;
      end loop;

      declare
         use type Glib.Gdouble;
         Surface : constant Cairo.Cairo_Surface :=
                     Cairo.Image_Surface.Create
                       (Cairo.Image_Surface.Cairo_Format_ARGB32,
                        Glib.Gint (Window.Viewport.Width),
                        Glib.Gint (Window.Viewport.Height));
         Context : constant Cairo.Cairo_Context :=
                     (if Window.Double_Buffered
                      then Cairo.Create (Surface)
                      else Gdk.Cairo.Create
                        (Window.Draw_Widget.Get_Window));

      begin
         Cairo.Set_Line_Width (Context, 1.0);
--           Cairo.Set_Source_Rgb
--             (Context,
--              Red     => Cairo.Color_Range (Window.Color.Red),
--              Green   => Cairo.Color_Range (Window.Color.Green),
--              Blue    => Cairo.Color_Range (Window.Color.Blue));
         Cairo.Rectangle
           (Context, 0.0, 0.0,
            Glib.Gdouble (Window.Viewport.Width),
            Glib.Gdouble (Window.Viewport.Height));
         Cairo.Fill (Context);

         Cairo.Set_Source_Rgb (Context, 1.0, 1.0, 1.0);

         Draw_Z_Buffer (Context, Window.Z_Buffer);

         for Surface of Window.Surfaces loop
            declare
               X : constant Xi_Float := Surface.Window_X;
               Y : constant Xi_Float := Surface.Window_Y;
            begin
               Cairo.Save (Context);
               Cairo.Translate (Context, Glib.Gdouble (X), Glib.Gdouble (Y));
               Cairo.Set_Source_Surface (Context, Surface.Surface,
                                         0.0, 0.0);
               Cairo.Paint (Context);
               Cairo.Restore (Context);
            end;
         end loop;

         if Window.Double_Buffered then
            declare
               Window_Context : constant Cairo.Cairo_Context :=
                                  Gdk.Cairo.Create
                                    (Window.Draw_Widget.Get_Window);
            begin
               Cairo.Rectangle (Window_Context,
                                Glib.Gdouble (Window.Viewport.X),
                                Glib.Gdouble (Window.Viewport.Y),
                                Glib.Gdouble (Window.Viewport.Width),
                                Glib.Gdouble (Window.Viewport.Height));
               Cairo.Set_Source_Surface
                 (Window_Context, Surface, 0.0, 0.0);
               Cairo.Fill (Window_Context);
               Cairo.Destroy (Window_Context);
            end;
         end if;

         Cairo.Destroy (Context);
         Cairo.Surface_Destroy (Surface);

      end;

   end After_Render;

   -------------------
   -- Before_Render --
   -------------------

   overriding procedure Before_Render
     (Window : not null access Xi_Gtk_Window_Record)
   is
   begin
      Window.Faces.Clear;
      Window.Surfaces.Clear;
   end Before_Render;

   ---------------------
   -- Begin_Operation --
   ---------------------

   overriding procedure Begin_Operation
     (Item      : in out Xi_Gtk_Render_Operator;
      Operation : Xi.Render_Operation.Operation_Type)
   is
   begin
      Item.Operation := Operation;
   end Begin_Operation;

   --------------------
   -- Clear_Z_Buffer --
   --------------------

   procedure Clear_Z_Buffer is
   begin
      Z_Buffer.Clear;
   end Clear_Z_Buffer;

   -----------------------
   -- Configure_Handler --
   -----------------------

   function Configure_Handler
     (W : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Window : Xi_Gtk_Window)
      return Boolean
   is
      Allocation : Gtk.Widget.Gtk_Allocation;
   begin
      W.Get_Allocation (Allocation);
      Window.Full_Viewport.Set_Size (Xi_Float (Allocation.Width),
                                     Xi_Float (Allocation.Height));
      return True;
   end Configure_Handler;

   -----------------------------
   -- Create_Top_Level_Window --
   -----------------------------

   function Create_Top_Level_Window return Xi_Gtk_Window is
      Result : constant Xi_Gtk_Window :=
                 new Xi_Gtk_Window_Record;
   begin

      Gtk.Window.Gtk_New (Result.Top_Window);

      Result.Top_Window.Set_Title ("Xi Gtk");

      Gtk.Drawing_Area.Gtk_New
        (Result.Draw_Widget);
      Result.Top_Window.Add (Result.Draw_Widget);

      declare
         use Gdk.Event;
      begin
         Result.Draw_Widget.Set_Events
           (Exposure_Mask
            or Pointer_Motion_Mask
            or Pointer_Motion_Hint_Mask
            or Button_Motion_Mask
            or Button_Press_Mask
            or Button_Release_Mask
            or Key_Press_Mask
            or Key_Release_Mask
            or Structure_Mask
            or Scroll_Mask);
      end;

      Top_Window_Callback.Connect
        (Result.Top_Window,
         Gtk.Widget.Signal_Destroy,
         Top_Window_Callback.To_Marshaller (Destroy_Handler'Access));

      Drawing_Area_Callback.Connect
        (Result.Draw_Widget,
         Gtk.Widget.Signal_Configure_Event,
         Drawing_Area_Callback.To_Marshaller (Configure_Handler'Access),
         Result);

      Drawing_Area_Callback.Connect
        (Result.Draw_Widget,
         Gtk.Widget.Signal_Draw,
         Drawing_Area_Callback.To_Marshaller (Expose_Handler'Access),
         Result);

      Drawing_Area_Callback.Connect
        (Result.Draw_Widget, Gtk.Widget.Signal_Button_Press_Event,
         Drawing_Area_Callback.To_Marshaller
           (Mouse_Button_Press_Handler'Access),
         Result);

      Drawing_Area_Callback.Connect
        (Result.Draw_Widget, Gtk.Widget.Signal_Button_Release_Event,
         Drawing_Area_Callback.To_Marshaller
           (Mouse_Button_Release_Handler'Access),
         Result);

      Drawing_Area_Callback.Connect
        (Result.Draw_Widget, Gtk.Widget.Signal_Motion_Notify_Event,
         Drawing_Area_Callback.To_Marshaller
           (Mouse_Move_Handler'Access),
         Result);

      Drawing_Area_Callback.Connect
        (Result.Draw_Widget,
         Gtk.Widget.Signal_Key_Press_Event,
         Drawing_Area_Callback.To_Marshaller
           (Key_Press_Handler'Access),
         Result);

      Drawing_Area_Callback.Connect
        (Result.Draw_Widget,
         Gtk.Widget.Signal_Key_Release_Event,
         Drawing_Area_Callback.To_Marshaller
           (Key_Release_Handler'Access),
         Result);

      Result.Top_Window.Show_All;

      Result.Lights.Set_Length (3);

      return Result;

   end Create_Top_Level_Window;

   ---------------------
   -- Destroy_Handler --
   ---------------------

   procedure Destroy_Handler
     (W : access Gtk.Window.Gtk_Window_Record'Class)
   is
      pragma Unreferenced (W);
   begin
      Xi.Main.Current_Renderer.Exit_Render_Loop;
   end Destroy_Handler;

   ------------------
   -- Draw_Polygon --
   ------------------

   procedure Draw_Polygon
     (Context  : Cairo.Cairo_Context;
      Vertices : Buffer_Points;
      Colour   : Xi.Color.Xi_Color;
      Filled   : Boolean)
   is
      use Glib;
      First : Boolean := True;
   begin

      Cairo.Set_Source_Rgba
        (Context,
         Red     => Cairo.Color_Range (Colour.Red),
         Green   => Cairo.Color_Range (Colour.Green),
         Blue    => Cairo.Color_Range (Colour.Blue),
         Alpha   => Cairo.Color_Range (Colour.Alpha));

      for V of Vertices loop
         declare
            X : constant Gdouble := Gdouble (V.X);
            Y : constant Gdouble := Gdouble (V.Y);
         begin
            if First then
               Cairo.Move_To (Context, X, Y);
               First := False;
            else
               Cairo.Line_To (Context, X, Y);
            end if;
         end;
      end loop;

      Cairo.Line_To (Context,
                     Gdouble (Vertices (Vertices'First).X),
                     Gdouble (Vertices (Vertices'First).Y));

      if Filled then
         Cairo.Fill_Preserve (Context);
      end if;

      Cairo.Stroke (Context);

   end Draw_Polygon;

   -------------------
   -- Draw_Z_Buffer --
   -------------------

   procedure Draw_Z_Buffer
     (Context : Cairo.Cairo_Context;
      Active  : Boolean)
   is
      pragma Unreferenced (Active);
   begin
      for S of Z_Buffer loop
         Draw_Polygon
           (Context  => Context,
            Vertices => S.Pts (1 .. S.Count),
            Colour   => S.Colour,
            Filled   => S.Filled);
      end loop;
   end Draw_Z_Buffer;

   ------------------
   -- Enable_Light --
   ------------------

   overriding procedure Enable_Light
     (Target  : not null access Xi_Gtk_Window_Record;
      Index   : Positive;
      Enabled : Boolean)
   is
   begin
      if Index <= Target.Lights.Last_Index then
         Target.Lights (Index).Enabled := Enabled;
      end if;
   end Enable_Light;

   -------------------
   -- End_Operation --
   -------------------

   overriding procedure End_Operation
     (Item : in out Xi_Gtk_Render_Operator)
   is
      pragma Unreferenced (Item);
   begin
      null;
   end End_Operation;

   --------------------
   -- Expose_Handler --
   --------------------

   function Expose_Handler
     (W : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Window : Xi_Gtk_Window)
      return Boolean
   is
      pragma Unreferenced (W);
      pragma Unreferenced (Window);
   begin
      return True;
   end Expose_Handler;

   -----------------
   -- Find_Centre --
   -----------------

   function Find_Centre
     (Face : Face_Type)
      return Xi.Matrices.Vector_4
   is
      use Xi.Matrices, Xi.Float_Arrays;
      Result : Vector_4 := (0.0, 0.0, 0.0, 0.0);
   begin
      for V of Face.Vs loop
         Result := Result + V;
      end loop;
      Result := Result / Xi_Float (Face.Vs.Length);
      Result (4) := 1.0;
      return Result;
   end Find_Centre;

   -----------------------
   -- Get_Pango_Context --
   -----------------------

   function Get_Pango_Context
     (Window : Xi_Gtk_Window_Record'Class)
      return Pango.Context.Pango_Context
   is
   begin
      return Window.Top_Window.Get_Pango_Context;
   end Get_Pango_Context;

   -------------------
   -- Get_Rectangle --
   -------------------

   overriding function Get_Rectangle
     (Item : Xi_Gtk_Window_Record)
      return Xi.Rectangle.Xi_Rectangle
   is
   begin
      return Item.Rectangle;
   end Get_Rectangle;

   --------------------------------
   -- Mouse_Button_Press_Handler --
   --------------------------------

   function Mouse_Button_Press_Handler
     (W      : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Window : Xi_Gtk_Window)
      return Boolean
   is
      pragma Unreferenced (W);
      pragma Unreferenced (Window);
      use type Glib.Guint;
   begin
      Gtk_Mouse.State.Button
        (Xi.Mouse.Mouse_Button'Val (Event.Button.Button - 1)) :=
        Xi.Mouse.Down;
      return True;
   end Mouse_Button_Press_Handler;

   ----------------------------------
   -- Mouse_Button_Release_Handler --
   ----------------------------------

   function Mouse_Button_Release_Handler
     (W      : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Window : Xi_Gtk_Window)
      return Boolean
   is
      pragma Unreferenced (W);
      pragma Unreferenced (Window);
      use type Glib.Guint;
   begin
      Gtk_Mouse.State.Button
        (Xi.Mouse.Mouse_Button'Val (Event.Button.Button - 1)) :=
        Xi.Mouse.Up;
      return True;
   end Mouse_Button_Release_Handler;

   ------------------------
   -- Mouse_Move_Handler --
   ------------------------

   function Mouse_Move_Handler
     (W      : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Window : Xi_Gtk_Window)
      return Boolean
   is
      pragma Unreferenced (Window);
      use type Glib.Guint;
   begin

      Gtk_Mouse.State.X := Xi_Float (Event.Motion.X);
      Gtk_Mouse.State.Reverse_Y :=
        Xi_Float (Event.Motion.Y);
      Gtk_Mouse.State.Y :=
        Xi_Float (W.Get_Allocated_Height) - Gtk_Mouse.State.Reverse_Y;
      return True;
   end Mouse_Move_Handler;

   ------------
   -- Normal --
   ------------

   overriding procedure Normal
     (Item    : in out Xi_Gtk_Render_Operator;
      Vector  : Xi.Matrices.Vector_3)
   is
      use Xi.Float_Arrays;
      View_Model_Matrix : constant Xi.Matrices.Matrix_4 :=
                            Item.Window.Current (Xi.Matrices.Model_View);
      Inverse_Matrix    : constant Xi.Matrices.Matrix_4 :=
                            Inverse (View_Model_Matrix);
      Transposed_Matrix : constant Xi.Matrices.Matrix_4 :=
                            Transpose (Inverse_Matrix);
      New_Normal : constant Xi.Matrices.Vector_4 :=
                            Transposed_Matrix * (Vector & 1.0);
   begin
      Item.Current_Face.Normal := New_Normal (1 .. 3);
   end Normal;

   ---------------------
   -- Set_Full_Screen --
   ---------------------

   overriding procedure Set_Full_Screen
     (Window      : in out Xi_Gtk_Window_Record;
      Full_Screen : Boolean)
   is
   begin
      if Full_Screen then
         Window.Top_Window.Fullscreen;
      end if;
   end Set_Full_Screen;

   ---------------
   -- Set_Light --
   ---------------

   overriding procedure Set_Light
     (Target             : not null access Xi_Gtk_Window_Record;
      Index              : Positive;
      Ambient_Intensity  : Xi.Color.Xi_Color := (0.0, 0.0, 0.0, 1.0);
      Diffuse_Intensity  : Xi.Color.Xi_Color := (1.0, 1.0, 1.0, 1.0);
      Specular_Intensity : Xi.Color.Xi_Color := (1.0, 1.0, 1.0, 1.0);
      Position           : Xi.Matrices.Vector_4 := (0.0, 0.0, 1.0, 0.0);
      Spot_Direction     : Xi.Matrices.Vector_3 := (0.0, 0.0, -1.0);
      Spot_Exponent      : Xi_Float := 0.0;
      Spot_Cutoff        : Xi_Float := 180.0)
   is
   begin
      if Index <= Target.Lights.Last_Index then
         Target.Lights (Index) :=
           (True, Ambient_Intensity, Diffuse_Intensity, Specular_Intensity,
            Position, Spot_Direction, Spot_Exponent, Spot_Cutoff);
      end if;
   end Set_Light;

   -------------------------
   -- Set_Output_Position --
   -------------------------

   overriding procedure Set_Output_Position
     (Target             : not null access Xi_Gtk_Window_Record;
      X, Y               : Xi_Float)
   is
   begin
      Target.Window_X := X;
      Target.Window_Y := Y;
   end Set_Output_Position;

   -------------------
   -- Set_Rectangle --
   -------------------

   overriding procedure Set_Rectangle
     (Item  : in out Xi_Gtk_Window_Record;
      Rectangle : Xi.Rectangle.Xi_Rectangle)
   is
   begin
      Item.Rectangle := Rectangle;
      Item.Top_Window.Set_Default_Size
        (Glib.Gint (Rectangle.Width), Glib.Gint (Rectangle.Height));
   end Set_Rectangle;

   ------------------------
   -- Texture_Coordinate --
   ------------------------

   overriding procedure Texture_Coordinate
     (Item    : in out Xi_Gtk_Render_Operator;
      S, T    : Xi_Float)
   is
      pragma Unreferenced (Item);
      pragma Unreferenced (S);
      pragma Unreferenced (T);
   begin
      null;
   end Texture_Coordinate;

   ------------
   -- Vertex --
   ------------

   overriding procedure Vertex
     (Item    : in out Xi_Gtk_Render_Operator;
      Vector  : Xi.Matrices.Vector_3)
   is
      use Ada.Containers;
      use Xi.Render_Operation;
      use Xi.Float_Arrays;
      View_Model_Matrix : constant Xi.Matrices.Matrix_4 :=
                            Item.Window.Current (Xi.Matrices.Model_View);
   begin

      Item.Current_Face.Vs.Append
        (View_Model_Matrix * (Vector & 1.0));

      case Item.Operation is
         when Point_List =>
            null;
         when Line_List =>
            if Item.Current_Face.Vs.Length = 2 then
               Item.Window.Faces.Append (Item.Current_Face);
               Item.Current_Face.Vs.Clear;
            end if;
         when Line_Strip =>
            if Item.Current_Face.Vs.Length = 2 then
               Item.Window.Faces.Append (Item.Current_Face);
               Item.Current_Face.Vs.Delete_First;
            end if;
         when Triangle_List =>
            if Item.Current_Face.Vs.Length = 3 then
               Item.Window.Faces.Append (Item.Current_Face);
               Item.Current_Face.Vs.Clear;
            end if;
         when Triangle_Strip =>
            if Item.Current_Face.Vs.Length = 3 then
               Item.Window.Faces.Append (Item.Current_Face);
               Item.Current_Face.Vs.Delete_First;
            end if;
         when Triangle_Fan =>
            if Item.Current_Face.Vs.Length = 3 then
               Item.Window.Faces.Append (Item.Current_Face);
               declare
                  use Vertex_Lists;
                  Position : Cursor := Item.Current_Face.Vs.First;
               begin
                  Next (Position);
                  Item.Current_Face.Vs.Delete (Position);
               end;
            end if;
         when Quad_List =>
            if Item.Current_Face.Vs.Length = 4 then
               Item.Window.Faces.Append (Item.Current_Face);
               Item.Current_Face.Vs.Clear;
            end if;
      end case;
   end Vertex;

end Xi.Rendering.Gtk_Renderer.Gtk_Window;
