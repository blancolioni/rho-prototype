private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

private with Gtk.Drawing_Area;
private with Gtk.Window;

private with Cairo;

with Pango.Context;

with Xi.Color;
with Xi.Float_Arrays;
with Xi.Matrices;
with Xi.Rectangle;
with Xi.Render_Window;

package Xi.Rendering.Gtk_Renderer.Gtk_Window is

   type Xi_Gtk_Window_Record is
     new Xi.Render_Window.Xi_Render_Window_Record with private;

   function Get_Pango_Context
     (Window : Xi_Gtk_Window_Record'Class)
      return Pango.Context.Pango_Context;

   type Xi_Gtk_Window is access all Xi_Gtk_Window_Record'Class;

   function Create_Top_Level_Window return Xi_Gtk_Window;

private

   package Vertex_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Xi.Matrices.Vector_4, Xi.Float_Arrays."=");

   type Face_Type is
      record
         Color  : Xi.Color.Xi_Color;
         Normal : Xi.Matrices.Vector_3;
         Filled : Boolean := True;
         Vs     : Vertex_Lists.List;
      end record;

   package Face_Vectors is
     new Ada.Containers.Vectors (Positive, Face_Type);

   type Light_Info is
      record
         Enabled            : Boolean := False;
         Ambient_Intensity  : Xi.Color.Xi_Color := (0.0, 0.0, 0.0, 1.0);
         Diffuse_Intensity  : Xi.Color.Xi_Color := (1.0, 1.0, 1.0, 1.0);
         Specular_Intensity : Xi.Color.Xi_Color := (1.0, 1.0, 1.0, 1.0);
         Position           : Xi.Matrices.Vector_4 := (0.0, 0.0, 1.0, 0.0);
         Spot_Direction     : Xi.Matrices.Vector_3 := (0.0, 0.0, -1.0);
         Spot_Exponent      : Xi_Float := 0.0;
         Spot_Cutoff        : Xi_Float := 180.0;
      end record;

   package Light_Vectors is
     new Ada.Containers.Vectors (Positive, Light_Info);

   type Rendered_Surface_Record is
      record
         Window_X, Window_Y : Xi_Float;
         Width, Height      : Xi_Float;
         X_Origin, Y_Origin : Xi_Float;
         Surface            : Cairo.Cairo_Surface;
      end record;

   package Rendered_Surface_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Rendered_Surface_Record);

   type Xi_Gtk_Window_Record is
     new Xi.Render_Window.Xi_Render_Window_Record with
      record
         Rectangle          : Xi.Rectangle.Xi_Rectangle;
         Top_Window         : Gtk.Window.Gtk_Window;
         Draw_Widget        : Gtk.Drawing_Area.Gtk_Drawing_Area;
         Faces              : Face_Vectors.Vector;
         Lights             : Light_Vectors.Vector;
         Window_X, Window_Y : Xi_Float := 0.0;
         Surfaces           : Rendered_Surface_Lists.List;
      end record;

   overriding procedure Ortho
     (Target      : in out Xi_Gtk_Window_Record;
      Left, Right : Xi_Float;
      Bottom, Top : Xi_Float;
      Near, Far   : Xi_Float)
   is null;

   overriding procedure Set_Rectangle
     (Item  : in out Xi_Gtk_Window_Record;
      Rectangle : Xi.Rectangle.Xi_Rectangle);

   overriding function Get_Rectangle
     (Item : Xi_Gtk_Window_Record)
      return Xi.Rectangle.Xi_Rectangle;

   overriding procedure Set_Full_Screen
     (Window      : in out Xi_Gtk_Window_Record;
      Full_Screen : Boolean);

   overriding procedure Before_Render
     (Window : not null access Xi_Gtk_Window_Record);

   overriding procedure After_Render
     (Window : not null access Xi_Gtk_Window_Record);

   overriding procedure Set_Output_Position
     (Target             : not null access Xi_Gtk_Window_Record;
      X, Y               : Xi_Float);

   overriding procedure Set_Raster_Position
     (Target             : not null access Xi_Gtk_Window_Record;
      Position           : Xi.Matrices.Vector_3)
   is null;

   overriding procedure Enable_Light
     (Target  : not null access Xi_Gtk_Window_Record;
      Index   : Positive;
      Enabled : Boolean);

   overriding procedure Set_Light
     (Target             : not null access Xi_Gtk_Window_Record;
      Index              : Positive;
      Ambient_Intensity  : Xi.Color.Xi_Color := (0.0, 0.0, 0.0, 1.0);
      Diffuse_Intensity  : Xi.Color.Xi_Color := (1.0, 1.0, 1.0, 1.0);
      Specular_Intensity : Xi.Color.Xi_Color := (1.0, 1.0, 1.0, 1.0);
      Position           : Xi.Matrices.Vector_4 := (0.0, 0.0, 1.0, 0.0);
      Spot_Direction     : Xi.Matrices.Vector_3 := (0.0, 0.0, -1.0);
      Spot_Exponent      : Xi_Float := 0.0;
      Spot_Cutoff        : Xi_Float := 180.0);

end Xi.Rendering.Gtk_Renderer.Gtk_Window;
