private with Xi.Mouse;

package Xi.Rendering.Gtk_Renderer is

   function Create_Gtk_Renderer
     return Xi_Renderer;

private

   type Gtk_Mouse_State_Record is
     new Xi.Mouse.Xi_Mouse_State_Record with
      record
         State : Xi.Mouse.Mouse_State;
      end record;

   overriding
   function State (Mouse : Gtk_Mouse_State_Record)
                   return Xi.Mouse.Mouse_State
   is (Mouse.State);

   type Gtk_Mouse_State is access all Gtk_Mouse_State_Record'Class;

   Gtk_Mouse : aliased Gtk_Mouse_State_Record;

end Xi.Rendering.Gtk_Renderer;
