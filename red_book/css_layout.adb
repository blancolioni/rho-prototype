with Maas;                               use Maas;

with Rho.Main;
with Rho.Mouse;
with Rho.Paths;
with Rho.Render_Window;

with Xtk.Builder;
with Xtk.Page;
with Xtk.Widget;

package body Css_Layout is

   -------------------
   -- Create_Window --
   -------------------

   procedure Create_Window is
      Window : constant Rho.Render_Window.Rho_Render_Window :=
                 Rho.Main.Current_Renderer.Create_Top_Level_Window;
   begin
      Xtk.Initialize;

      declare
         Builder : constant Xtk.Builder.Xtk_Builder :=
                     Xtk.Builder.Xtk_New_From_File
                       (Rho.Paths.Config_File ("css_demo.html"));

         Page : constant Xtk.Page.Xtk_Page :=
                  Builder.Get_Page;

      begin
         Page.Set_Viewport (Window.Full_Viewport);
         Page.Show_All;
         Window.Add_Top_Level (Page);
      end;

   end Create_Window;

end Css_Layout;
