with Gnoga.Types;
with Gnoga.Gui.View;

with Rho.Handles;
with Rho.Rendering.WebGL_Renderer;
--  with Rho.Render_Window;

with Earth;

package body Rho_Gnoga_Demo is

   type App_Data is new Gnoga.Types.Connection_Data_Type with
      record
         View   : Gnoga.Gui.View.View_Type;
         Handle : Rho.Handles.Rho_Handle;
      end record;

   type App_Access is access all App_Data;

   ----------------
   -- On_Connect --
   ----------------

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      App : constant App_Access := new App_Data;
   begin
      Main_Window.Connection_Data (App);

      App.View.Create (Main_Window);
      App.Handle :=
        Rho.Handles.New_Handle
          (Event_Source => Rho.Rendering.WebGL_Renderer.WebGL_Event_Source);

      declare
         Renderer : constant Rho.Rendering.Rho_Renderer :=
                      Rho.Rendering.WebGL_Renderer.Create_WebGL_Renderer
                        (App.View);
--           Window   : constant Rho.Render_Window.Rho_Render_Window :=
--                        Renderer.Create_Top_Level_Window;
--           pragma Unreferenced (Window);
      begin
         App.Handle.Use_Renderer (Renderer);
      end;

      Earth.Create_Demo (App.Handle);
      App.Handle.Main_Loop;

   end On_Connect;

end Rho_Gnoga_Demo;
