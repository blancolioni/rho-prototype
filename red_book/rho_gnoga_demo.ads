with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;

package Rho_Gnoga_Demo is

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type);

end Rho_Gnoga_Demo;
