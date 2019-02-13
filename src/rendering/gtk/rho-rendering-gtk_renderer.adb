with Glib.Main;

with Gtk.Main;

with Cairo.Image_Surface;

with Xi.Main;
with Xi.Rendering.Gtk_Renderer.Gtk_Window;

package body Xi.Rendering.Gtk_Renderer is

   type Xi_Gtk_Renderer_Record is
     new Xi_Renderer_Record with
      record
         Top_Window : Gtk_Window.Xi_Gtk_Window;
         Timeout_Id : Glib.Main.G_Source_Id;
      end record;

   type Xi_Gtk_Renderer is access all Xi_Gtk_Renderer_Record'Class;

   overriding procedure Render_Loop
     (Renderer : in out Xi_Gtk_Renderer_Record);

   overriding procedure Exit_Render_Loop
     (Renderer : in out Xi_Gtk_Renderer_Record);

   overriding function Create_Top_Level_Window
     (Renderer : in out Xi_Gtk_Renderer_Record)
      return Xi.Render_Window.Xi_Render_Window;

   overriding procedure Render_One_Frame
     (Renderer : in out Xi_Gtk_Renderer_Record);

   overriding function Create_Surface
     (Renderer : in out Xi_Gtk_Renderer_Record;
      Width, Height : Xi_Float)
      return Cairo.Cairo_Surface;

   overriding function Load_Texture
     (Renderer : in out Xi_Gtk_Renderer_Record;
      S_Wrap       : Xi.Texture.Texture_Wrap_Type;
      T_Wrap       : Xi.Texture.Texture_Wrap_Type;
      Mag_Filter   : Xi.Texture.Texture_Filter_Type)
      return Xi.Texture.Texture_Id
   is (0);

   overriding procedure Load_Texture_Data
     (Renderer     : in out Xi_Gtk_Renderer_Record;
      Texture_Id   : Xi.Texture.Texture_Id;
      From_Surface : Cairo.Cairo_Surface;
      Region       : Xi.Rectangle.Xi_Rectangle)
   is null;

--     overriding function Load_Vertex_Array
--       (Renderer : in out Xi_Gtk_Renderer_Record;
--        Vertex_Array : Xi.Vertex_Array.Xi_Vertex_Array_Record'Class)
--        return Xi.Vertex_Array.Xi_Vertex_Array_Id
--     is (0);

   overriding function Load_Buffer
     (Renderer : in out Xi_Gtk_Renderer_Record;
      Buffer   : Xi.Float_Buffer.Xi_Float_Buffer_Record'Class)
      return Xi.Float_Buffer.Xi_Float_Buffer_Id
   is (0);

   function Idle_Handler return Boolean;

   -------------------------
   -- Create_Gtk_Renderer --
   -------------------------

   function Create_Gtk_Renderer
      return Xi_Renderer
   is
      Result : constant Xi_Gtk_Renderer :=
                 new Xi_Gtk_Renderer_Record;
   begin
      Gtk.Main.Init;
      Xi.Mouse.Set_Current_Mouse (Gtk_Mouse'Access);
      return Xi_Renderer (Result);
   end Create_Gtk_Renderer;

   --------------------
   -- Create_Surface --
   --------------------

   overriding function Create_Surface
     (Renderer : in out Xi_Gtk_Renderer_Record;
      Width, Height : Xi_Float)
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
     (Renderer : in out Xi_Gtk_Renderer_Record)
      return Xi.Render_Window.Xi_Render_Window
   is
   begin

      Renderer.Top_Window :=
        Gtk_Window.Create_Top_Level_Window;

      Renderer.Timeout_Id :=
        Glib.Main.Timeout_Add (20, Idle_Handler'Access);
--          Glib.Main.Idle_Add
--            (Idle_Handler'Access);

      return Xi.Render_Window.Xi_Render_Window (Renderer.Top_Window);
   end Create_Top_Level_Window;

   ----------------------
   -- Exit_Render_Loop --
   ----------------------

   overriding procedure Exit_Render_Loop
     (Renderer : in out Xi_Gtk_Renderer_Record)
   is
   begin
      Glib.Main.Remove (Renderer.Timeout_Id);
      Gtk.Main.Main_Quit;
   end Exit_Render_Loop;

   ------------------
   -- Idle_Handler --
   ------------------

   function Idle_Handler return Boolean is
   begin
      Xi.Main.Render_One_Frame;
      return True;
   end Idle_Handler;

   -----------------
   -- Render_Loop --
   -----------------

   overriding procedure Render_Loop
     (Renderer : in out Xi_Gtk_Renderer_Record)
   is
      pragma Unreferenced (Renderer);
   begin
      Gtk.Main.Main;
   end Render_Loop;

   ----------------------
   -- Render_One_Frame --
   ----------------------

   overriding procedure Render_One_Frame
     (Renderer : in out Xi_Gtk_Renderer_Record)
   is
   begin
      Renderer.Top_Window.Render;
   end Render_One_Frame;

end Xi.Rendering.Gtk_Renderer;
