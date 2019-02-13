with Maas;                               use Maas;

with Rho.Assets;
with Rho.Camera;
with Rho.Elementary_Functions;
with Rho.Light;
with Rho.Main;
with Rho.Node;
with Rho.Scene;
with Rho.Shapes;
with Rho.Render_Window;
with Rho.Transition.Orientation;

package body Cube is

   -------------------
   -- Create_Window --
   -------------------

   procedure Create_Window is
      Scene : constant Rho.Scene.Rho_Scene := Rho.Scene.Create_Scene;
      Camera : constant Rho.Camera.Rho_Camera := Scene.Active_Camera;
      Node   : constant Rho.Node.Rho_Node := Scene.Create_Node ("cube");
      Light  : Rho.Light.Rho_Light;
      Window : constant Rho.Render_Window.Rho_Render_Window :=
                 Rho.Main.Current_Renderer.Create_Top_Level_Window;
   begin
      Window.Set_Wireframe (False);
      Window.Set_Back_Face_Removal (True);
      Camera.Set_Viewport (Window.Full_Viewport);
      Camera.Set_Position (0.0, 0.0, 5.0);
      Camera.Set_Orientation (0.0, 0.0, 1.0, 0.0);
      Camera.Look_At (0.0, 0.0, 0.0);
      Camera.Frustum (-1.0, 1.0, -1.0, 1.0, 1.5, 20.0);
      Node.Set_Entity (Rho.Shapes.Cube);
      Node.Entity.Set_Material (Rho.Assets.Material ("Rho.Blue"));
      Rho.Light.Rho_New (Light, Rho.Light.Ambient);
      Light.Set_Color (1.0, 1.0, 1.0, 1.0);
      Scene.Add_Light (Light);

      declare
         use Rho.Transition, Rho.Transition.Orientation;
         R2 : constant Rho_Float := Rho.Elementary_Functions.Sqrt (2.0) / 2.0;
         Transition : constant Rho_Orientation_Transition :=
                        New_Orientation_Transition
                          (Node            => Node,
                           Transition_Time => 5.0,
                           Target_Angle    => 360.0,
                           Angle_X         => R2,
                           Angle_Y         => R2,
                           Angle_Z         => 0.0,
                           Cyclic          => True);
      begin
         Scene.Add_Transition (Transition);
      end;

      Window.Set_Scene (Scene);
   end Create_Window;

end Cube;
