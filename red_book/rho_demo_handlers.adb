with Demos;

package body Rho_Demo_Handlers is

   -----------------
   -- Choose_Demo --
   -----------------

   function Choose_Demo
     (Widget : not null access Xtk.Widget.Xtk_Widget_Record'Class;
      Button : Positive)
      return Xtk.Events.Event_Response
   is
      pragma Unreferenced (Button);
      Demo : constant Demos.Rho_Demo_Type := Demos.Demo (Widget.Id);
      pragma Unreferenced (Demo);
   begin
      return Xtk.Events.Stop_Event;
   end Choose_Demo;

end Rho_Demo_Handlers;
