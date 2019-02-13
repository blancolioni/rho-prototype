with Xi.Color;
with Xi.Matrices;
with Xi.Render_Operation;

package Xi.Rendering.Gtk_Render_Operator is

   type Xi_Gtk_Render_Operator is
     new Xi.Render_Operation.Xi_Render_Operation_Record with private;

   overriding procedure Begin_Operation
     (Item      : in out Xi_Gtk_Render_Operator;
      Operation : Xi.Render_Operation.Operation_Type);

   overriding procedure Finish_Operation
     (Item : in out Xi_Gtk_Render_Operator);

   overriding procedure Color
     (Item    : in out Xi_Gtk_Render_Operator;
      Color   : Xi.Color.Xi_Color);

   overriding procedure Normal
     (Item    : in out Xi_Gtk_Render_Operator;
      Vector  : Xi.Matrices.Vector_3);

   overriding procedure Vertex
     (Item    : in out Xi_Gtk_Render_Operator;
      Vector  : Xi.Matrices.Vector_3);

   overriding procedure Texture_Coordinate
     (Item    : Xi_Gtk_Render_Operator;
      S, T    : Xi_Float);

private

   type Xi_Gtk_Render_Operator is
     new Xi.Render_Operation.Xi_Render_Operation_Record with null record;

end Xi.Rendering.Gtk_Render_Operator;
