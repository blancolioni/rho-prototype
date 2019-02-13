with Rho.Render_Target;

package Rho.Frame_Event is

   type Rho_Frame_Event is
      record
         Render_Target         : Rho.Render_Target.Rho_Render_Target;
         Time_Since_Last_Event : Duration;
      end record;

   type Rho_Frame_Listener_Interface is interface;

   procedure Frame_Started
     (Listener : in out Rho_Frame_Listener_Interface;
      Event    : Rho_Frame_Event)
   is null;

   procedure Frame_Rendering_Queued
     (Listener : in out Rho_Frame_Listener_Interface;
      Event    : Rho_Frame_Event)
   is null;

   procedure Frame_Ended
     (Listener : in out Rho_Frame_Listener_Interface;
      Event    : Rho_Frame_Event)
   is null;

   type Rho_Frame_Listener is access all Rho_Frame_Listener_Interface'Class;

end Rho.Frame_Event;
