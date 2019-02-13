with Rho.Scene_Renderer;

package Rho.Render_Window is

   type Rho_Render_Window_Record is
     abstract new Rho.Scene_Renderer.Rho_Scene_Renderer_Record
       with private;

   procedure Set_Full_Screen
     (Window      : in out Rho_Render_Window_Record;
      Full_Screen : Boolean)
   is abstract;

   type Rho_Render_Window is access all Rho_Render_Window_Record'Class;

private

   type Rho_Render_Window_Record is
     abstract new Rho.Scene_Renderer.Rho_Scene_Renderer_Record with
      record
         null;
      end record;

end Rho.Render_Window;
