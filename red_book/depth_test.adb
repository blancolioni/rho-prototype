with GL;

with Maas;                               use Maas;

with Rho.Assets;
with Rho.Camera;
with Rho.Entity;
with Rho.Frame_Event;
with Rho.Keyboard;
with Rho.Light;
with Rho.Main;
with Rho.Materials.Material;
with Rho.Matrices;
with Rho.Node;
with Rho.Scene;
with Rho.Shapes;
with Rho.Render_Window;
with Rho.Texture;

with Rho.Paths;
with Rho.Float_Arrays;

package body Depth_Test is

   type Frame_Listener is
     new Rho.Frame_Event.Rho_Frame_Listener_Interface with
      record
         Earth_Node : Rho.Node.Rho_Node;
         Blue_Node  : Rho.Node.Rho_Node;
         Spin       : Rho_Float := 0.0;
         Camera     : Rho.Camera.Rho_Camera;
         DZ         : Signed_Unit_Float := 1.0;
      end record;

   overriding procedure Frame_Started
     (Listener : in out Frame_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event);

   -------------------
   -- Create_Window --
   -------------------

   procedure Create_Window is
      Scene : constant Rho.Scene.Rho_Scene := Rho.Scene.Create_Scene;
      Camera : constant Rho.Camera.Rho_Camera := Scene.Active_Camera;
      Sphere_1 : constant Rho.Entity.Rho_Entity :=
                   Rho.Shapes.Quadric_Sphere (40, 20);
      Sphere_2 : constant Rho.Entity.Rho_Entity :=
                   Rho.Shapes.Icosohedral_Sphere (3);
      Earth_Node : constant Rho.Node.Rho_Node := Scene.Create_Node ("earth");
      Blue_Node  : constant Rho.Node.Rho_Node := Scene.Create_Node ("node");
      Light      : Rho.Light.Rho_Light;
      Window : constant Rho.Render_Window.Rho_Render_Window :=
                 Rho.Main.Current_Renderer.Create_Top_Level_Window;
      Earth_Material : constant Rho.Materials.Material.Rho_Material :=
                         Rho.Materials.Material.Rho_New_With_Texture
                           ("earth",
                            Rho.Texture.Create_From_Png
                              ("earth",
                               Rho.Paths.Config_File ("earth-large.png")),
                            Lighting => True);
      Blue_Material : constant Rho.Materials.Material.Rho_Material :=
                         Rho.Assets.Material ("Maas/Blue");
   begin

      GL.Enable_Debug;

      Window.Set_Wireframe (False);
      Window.Set_Back_Face_Removal (True);
      Window.Set_Depth_Test (True);

      Camera.Set_Viewport (Window.Full_Viewport);
      Camera.Set_Position (0.0, 0.0, 4.0);
      Camera.Look_At (0.0, 0.0, 0.0);
      Camera.Frustum (-1.0, 1.0, -1.0, 1.0, 0.5, 8.0);

      Rho.Light.Rho_New (Light, Rho.Light.Point);
      Light.Set_Position (3.0, 0.5, 0.0);
      Light.Set_Color (1.0, 1.0, 1.0, 1.0);
      Light.Set_Attenuation (0.2);
      Light.Set_Ambient_Coefficient (0.005);

      Earth_Material.Technique (1).Pass (1).Set_Specular
        (Value => (1.0, 1.0, 1.0, 1.0));
      Earth_Material.Technique (1).Pass (1).Set_Shininess (80.0);

      Sphere_1.Set_Material (Earth_Material);
      Earth_Node.Set_Entity (Sphere_1);

      Sphere_2.Set_Material (Blue_Material);
      Blue_Node.Set_Entity (Sphere_2);
      Blue_Node.Set_Position (1.5, 0.0, 0.0);

      Scene.Add_Light (Light);

      Window.Set_Scene (Scene);

      declare
         Listener : constant Rho.Frame_Event.Rho_Frame_Listener :=
                      new Frame_Listener'
                        (Earth_Node => Earth_Node,
                         Blue_Node  => Blue_Node,
                         DZ         => 0.2,
                         Spin       => 0.0,
                         Camera     => Camera);
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
        Listener.Spin + 5.0 * Rho_Float (Event.Time_Since_Last_Event);

      declare
         Z : Rho_Float := Listener.Blue_Node.Z;
      begin
         Z := Z + Listener.DZ * Rho_Float (Event.Time_Since_Last_Event);
         if abs Z > 1.0 then
            Listener.DZ := -Listener.DZ;
         end if;
         Listener.Blue_Node.Set_Z (Z);
      end;

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
            * Earth_Rotation
            * Rotation_Matrix (-90.0, (1.0, 0.0, 0.0)));
      end;
   end Frame_Started;

end Depth_Test;
