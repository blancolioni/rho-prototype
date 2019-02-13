with Xtk.Events;
with Xtk.Widget;

package Rho_Demo_Handlers is

   function Choose_Demo
     (Widget : not null access Xtk.Widget.Xtk_Widget_Record'Class;
      Button : Positive)
      return Xtk.Events.Event_Response;

end Rho_Demo_Handlers;
