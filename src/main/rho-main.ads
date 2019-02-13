with Rho.Frame_Event;

with Rho.Rendering;

package Rho.Main is

   procedure Init;

   procedure Main_Loop;

   procedure Leave_Main_Loop;

   function Current_Renderer return Rho.Rendering.Rho_Renderer;

   procedure Render_One_Frame;

   procedure Add_Frame_Listener
     (Listener : not null access
        Rho.Frame_Event.Rho_Frame_Listener_Interface'Class);

   procedure Remove_Frame_Listener
     (Listener : not null access
        Rho.Frame_Event.Rho_Frame_Listener_Interface'Class);

end Rho.Main;
