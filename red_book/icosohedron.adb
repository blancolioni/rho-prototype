with GL;

with Maas;                               use Maas;

with Rho.Assets;
with Rho.Camera;
with Rho.Color;
with Rho.Entity;
with Rho.Frame_Event;
with Rho.Keyboard;
with Rho.Main;
with Rho.Matrices;
with Rho.Node;
with Rho.Scene;
with Rho.Shapes;
with Rho.Render_Window;
with Rho.Texture;

with Rho.Float_Arrays;

package body Icosohedron is

   type Frame_Listener is
     new Rho.Frame_Event.Rho_Frame_Listener_Interface with
      record
         Earth_Node   : Rho.Node.Rho_Node;
         Spin         : Rho_Float := 0.0;
         Camera       : Rho.Camera.Rho_Camera;
      end record;

   overriding procedure Frame_Started
     (Listener : in out Frame_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event);

   function Vertex_Color
     (X, Y, Z : Signed_Unit_Float)
      return Rho.Color.Rho_Color
   is (abs X, abs Y, abs Z, 1.0);

   -------------------
   -- Create_Window --
   -------------------

   procedure Create_Window is
      Scene : constant Rho.Scene.Rho_Scene := Rho.Scene.Create_Scene;
      Camera : constant Rho.Camera.Rho_Camera := Scene.Active_Camera;
      Sphere : constant Rho.Entity.Rho_Entity :=
                   Rho.Shapes.Icosohedral_Sphere (5, Vertex_Color'Access);
      Node   : constant Rho.Node.Rho_Node := Scene.Create_Node ("sphere");
      Window : constant Rho.Render_Window.Rho_Render_Window :=
                 Rho.Main.Current_Renderer.Create_Top_Level_Window;
   begin

      GL.Enable_Debug;

      Sphere.Set_Material (Rho.Assets.Material ("Rho.Black"));

      Camera.Set_Viewport (Window.Full_Viewport);
      Camera.Set_Position (0.0, 0.0, 3.0);
      Camera.Look_At (0.0, 0.0, 0.0);
      Camera.Frustum (-1.0, 1.0, -1.0, 1.0, 1.5, 20.0);

      Node.Set_Entity (Sphere);

      Window.Set_Scene (Scene);

      declare
         Listener : constant Rho.Frame_Event.Rho_Frame_Listener :=
                      new Frame_Listener'
                        (Earth_Node   => Node,
                         Spin         => 0.0,
                         Camera       => Camera);
      begin
         Rho.Main.Add_Frame_Listener (Listener);
      end;

   end Create_Window;

   -------------------
   -- Frame_Started --
   -------------------

   overriding procedure Frame_Started
     (Listener : in out Frame_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event)
   is
   begin

      if Rho.Keyboard.Key_Down
        (Rho.Keyboard.Key_Esc)
      then
         Rho.Main.Leave_Main_Loop;
         return;
      end if;

      declare
         Aspect : constant Rho_Float :=
                    Listener.Camera.Viewport.Width
                      / Listener.Camera.Viewport.Height;
         Width  : constant Rho_Float := 2.0 * Aspect;
      begin
         Listener.Camera.Frustum
           (-Width / 2.0, Width / 2.0, -1.0, 1.0, 1.5, 20.0);
      end;

      Listener.Spin :=
        Listener.Spin + 10.0 * Rho_Float (Event.Time_Since_Last_Event);

      declare
         use Rho.Float_Arrays;
         use Rho.Matrices;
         Earth_Tilt     : constant Matrix_3 :=
                            Rotation_Matrix
                              (27.0, (1.0, 0.0, 0.0));
         Earth_Rotation : constant Matrix_3 :=
                            Rotation_Matrix
                              (Listener.Spin, (0.0, 1.0, 0.0));
      begin
         Listener.Earth_Node.Set_Orientation
           (Earth_Tilt
            * Earth_Rotation);
      end;

   end Frame_Started;

end Icosohedron;
