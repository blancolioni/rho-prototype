with Rho.Toolkit.Label;

package body Rho.Toolkit.Values.Renderers is

   type Text_Renderer_Record is
     new Rho_Value_Renderer_Interface with null record;

   overriding function Render
     (Renderer : Text_Renderer_Record;
      Value    : Rho_Value_Interface'Class)
      return Rho.Toolkit.Widget.Rho_Widget;

   overriding procedure Update
     (Renderer : Text_Renderer_Record;
      Widget   : not null access
        Rho.Toolkit.Widget.Rho_Widget_Record'Class;
      Value    : Rho_Value_Interface'Class);

   ------------
   -- Render --
   ------------

   overriding function Render
     (Renderer : Text_Renderer_Record;
      Value    : Rho_Value_Interface'Class)
      return Rho.Toolkit.Widget.Rho_Widget
   is
      pragma Unreferenced (Renderer);
   begin
      return Rho.Toolkit.Widget.Rho_Widget
        (Rho.Toolkit.Label.Rho_New (Value.To_String));
   end Render;

   -------------------
   -- Text_Renderer --
   -------------------

   function Text_Renderer return Rho_Value_Renderer is
   begin
      return Renderer : Text_Renderer_Record;
   end Text_Renderer;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Renderer : Text_Renderer_Record;
      Widget   : not null access
        Rho.Toolkit.Widget.Rho_Widget_Record'Class;
      Value    : Rho_Value_Interface'Class)
   is
      pragma Unreferenced (Renderer);
   begin
      Rho.Toolkit.Label.Rho_Label (Widget).Set_Label (Value.To_String);
   end Update;

end Rho.Toolkit.Values.Renderers;
