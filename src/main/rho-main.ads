with Rho.Frame_Event;

package Rho.Main is

   procedure Init;

   procedure Main_Loop;

   procedure Leave_Main_Loop;

   procedure Render_One_Frame;

   procedure Add_Frame_Listener
     (Listener : not null access
        Rho.Frame_Event.Rho_Frame_Listener_Interface'Class);

   procedure Remove_Frame_Listener
     (Listener : not null access
        Rho.Frame_Event.Rho_Frame_Listener_Interface'Class);

end Rho.Main;
