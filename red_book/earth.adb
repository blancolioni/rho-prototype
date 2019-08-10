with Rho;                              use Rho;

with Rho.Camera;
with Rho.Entity;
with Rho.Float_Arrays;
with Rho.Frame_Event;
with Rho.Light;
with Rho.Materials.Material;
with Rho.Matrices;
with Rho.Node;
with Rho.Scene;
with Rho.Shapes;
with Rho.Render_Window;
with Rho.Texture;

package body Earth is

   type Frame_Listener is
     new Rho.Frame_Event.Rho_Frame_Listener_Interface with
      record
         Earth_Node : Rho.Node.Rho_Node;
         Spin       : Rho_Float := 0.0;
         Camera     : Rho.Camera.Rho_Camera;
      end record;

   overriding procedure Frame_Started
     (Listener : in out Frame_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event);

   -------------------
   -- Create_Window --
   -------------------

   procedure Create_Demo
     (Handle : Rho.Handles.Rho_Handle)
   is
      Scene : constant Rho.Scene.Rho_Scene := Rho.Scene.Create_Scene (Handle);
      Camera : constant Rho.Camera.Rho_Camera := Scene.Active_Camera;
      Sphere : constant Rho.Entity.Rho_Entity :=
                 Rho.Shapes.Quadric_Sphere
                   (Context => Handle,
                    Slices  => 20,
                    Stacks  => 10);
      Node   : constant Rho.Node.Rho_Node := Scene.Create_Node ("earth");
      Light  : Rho.Light.Rho_Light;
      Window : constant Rho.Render_Window.Rho_Render_Window :=
                   Handle.Renderer.Create_Top_Level_Window;
      Texture  : constant Rho.Texture.Rho_Texture :=
                   Rho.Texture.Create_From_External_Id
                     (Handle, "earth", "earth");
      Material : constant Rho.Materials.Material.Rho_Material :=
                   Rho.Materials.Material.Rho_New_With_Texture
                     (Context  => Handle,
                      Name     => "earth",
                      Texture  => Texture);
   begin
      Window.Set_Wireframe (False);
      Window.Set_Back_Face_Removal (True);
      Camera.Set_Viewport (Window.Full_Viewport);
      Camera.Set_Position (0.0, 0.0, 2.5);
      Camera.Set_Orientation (0.0, 0.0, 1.0, 0.0);
      Camera.Look_At (0.0, 0.0, 0.0);
      Camera.Frustum (-1.0, 1.0, -1.0, 1.0, 1.5, 20.0);
      Sphere.Set_Material (Material);
      Rho.Light.Rho_New (Light, Handle, Rho.Light.Point);
      Light.Set_Position (1.0, 1.0, 1.0);

      Node.Set_Entity (Sphere);

      Scene.Add_Light (Light);

      Window.Set_Scene (Scene);

      declare
         Listener : constant Rho.Frame_Event.Rho_Frame_Listener :=
                      new Frame_Listener'
                        (Earth_Node => Node,
                         Spin       => 0.0,
                         Camera     => Camera);
      begin
         Handle.Add_Frame_Listener (Listener);
      end;

   end Create_Demo;

   -------------------
   -- Frame_Started --
   -------------------

   overriding procedure Frame_Started
     (Listener : in out Frame_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event)
   is
      pragma Unreferenced (Event);
   begin
      declare
         Aspect : constant Rho_Float :=
                    Listener.Camera.Viewport.Width
                      / Listener.Camera.Viewport.Height;
         Width  : constant Rho_Float := 2.0 * Aspect;
      begin
         Listener.Camera.Frustum
           (-Width / 2.0, Width / 2.0, -1.0, 1.0, 1.5, 20.0);
      end;
      Listener.Spin := Listener.Spin + 0.05;
      declare
         use Rho.Matrices;
         use Rho.Float_Arrays;
         Earth_Rotation : constant Matrix_3 :=
                            Rotation_Matrix
                              (Listener.Spin, (0.0, 1.0, 0.0))
                            * Rotation_Matrix (-70.0, (1.0, 0.0, 0.0));
      begin
         Listener.Earth_Node.Set_Orientation (Earth_Rotation);
      end;
   end Frame_Started;

end Earth;
