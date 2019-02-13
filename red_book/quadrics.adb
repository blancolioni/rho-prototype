with Maas;                               use Maas;

with Rho.Assets;
with Rho.Camera;
with Rho.Frame_Event;
with Rho.Light;
with Rho.Main;
with Rho.Materials.Material;
with Rho.Mouse;
with Rho.Node;
with Rho.Scene;
with Rho.Shapes;
with Rho.Render_Window;
with Rho.Transition.Container.Sequential;
with Rho.Transition.Orientation;
with Rho.Transition.Translation;
with Rho.Value;

package body Quadrics is

   type Local_Frame_Listener is
     new Rho.Frame_Event.Rho_Frame_Listener_Interface with
      record
         Scene         : Rho.Scene.Rho_Scene;
         Moving_Camera : Rho.Camera.Rho_Camera;
      end record;

   overriding procedure Frame_Started
     (Listener : in out Local_Frame_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event);

   -------------------
   -- Create_Window --
   -------------------

   procedure Create_Window is
      Scene : constant Rho.Scene.Rho_Scene := Rho.Scene.Create_Scene;
      Camera : constant Rho.Camera.Rho_Camera := Scene.Active_Camera;
      Moving_Camera : constant Rho.Camera.Rho_Camera :=
                        Rho.Camera.Create;
      Sphere_Node       : constant Rho.Node.Rho_Node :=
                            Scene.Create_Node ("sphere");
      Cylinder_Node     : constant Rho.Node.Rho_Node :=
                            Scene.Create_Node ("cylinder");
      Cone_Node         : constant Rho.Node.Rho_Node :=
                            Scene.Create_Node ("cone");
      Frustum_Node      : constant Rho.Node.Rho_Node :=
                            Scene.Create_Node ("frustum");
      Window         : constant Rho.Render_Window.Rho_Render_Window :=
                            Rho.Main.Current_Renderer.Create_Top_Level_Window;
      Material          : constant Rho.Materials.Material.Rho_Material :=
                            Rho.Assets.Material ("Rho.Wireframe_Color");
      Light             : Rho.Light.Rho_Light;
   begin

      --  Window.Set_Wireframe (True);
      --        Window.Set_Back_Face_Removal (False);

      Camera.Set_Viewport (Window.Full_Viewport);
      Camera.Set_Position (0.0, 0.0, 5.0);
      Camera.Set_Orientation (0.0, 0.0, 1.0, 0.0);
      Camera.Look_At (0.0, 0.0, 0.0);
      Camera.Frustum (-1.0, 1.0, -1.0, 1.0, 1.5, 20.0);

      Frustum_Node.Append_Child (Moving_Camera);
      Moving_Camera.Set_Viewport (Window.Full_Viewport);
      Moving_Camera.Set_Position (0.0, 0.0, 1.5);
      Moving_Camera.Perspective (60.0, 1.5, 10.0);

      Rho.Light.Rho_New (Light, Rho.Light.Ambient);
      Light.Set_Ambient_Coefficient (1.0);
      Light.Set_Color (1.0, 1.0, 1.0, 1.0);
      Light.Set_Position (2.0, 2.0, -2.0);
      Scene.Add_Light (Light);

      Sphere_Node.Set_Entity (Rho.Shapes.Quadric_Sphere (16, 8));
      Sphere_Node.Entity.Set_Material
        (Material.Instantiate);
      Sphere_Node.Entity.Material.Set_Parameter_Value
        ("color", Rho.Value.Color_Value (0.0, 0.0, 1.0, 1.0));
      Sphere_Node.Set_Position (-1.2, 1.2, 0.0);

      Cylinder_Node.Set_Entity (Rho.Shapes.Quadric_Cylinder (16, 8));
      Cylinder_Node.Entity.Set_Material
        (Material.Instantiate);
      Cylinder_Node.Entity.Material.Set_Parameter_Value
        ("color", Rho.Value.Color_Value (0.0, 1.0, 0.0, 1.0));
      Cylinder_Node.Set_Position (1.2, 1.2, 0.0);
      Cylinder_Node.Set_Orientation (0.0, 1.0, 0.0, 0.0);

      Cone_Node.Set_Entity (Rho.Shapes.Quadric_Cone (16, 8));
      Cone_Node.Entity.Set_Material
        (Material.Instantiate);
      Cone_Node.Entity.Material.Set_Parameter_Value
        ("color", Rho.Value.Color_Value (1.0, 0.0, 0.0, 1.0));
      --  Cone_Node.Scale (1.0, 2.0, 1.0);
      Cone_Node.Rotate (45.0, 1.0, 0.0, 0.0);
      Cone_Node.Set_Position (-1.2, -1.2, 0.0);

      Frustum_Node.Set_Entity
        (Rho.Shapes.Quadric_Conical_Frustum (16, 8, 0.5));
      Frustum_Node.Entity.Set_Material
        (Material.Instantiate);
      Frustum_Node.Entity.Material.Set_Parameter_Value
        ("color", Rho.Value.Color_Value (1.0, 0.0, 1.0, 1.0));
      --  Frustum_Node.Scale (1.0, 2.0, 1.0);
      --  Frustum_Node.Rotate (45.0, 1.0, 0.0, 0.0);
      Frustum_Node.Set_Position (1.2, -1.2, 0.0);

      declare
         use Rho.Transition, Rho.Transition.Orientation;
         Transition : constant Rho_Orientation_Transition :=
                        New_Orientation_Transition
                          (Node            => Frustum_Node,
                           Transition_Time => 5.0,
                           Target_Angle    => 360.0,
                           Angle_X         => 1.0,
                           Angle_Y         => 0.0,
                           Angle_Z         => 0.0,
                           Cyclic          => True);
      begin
         Scene.Add_Transition (Transition);
      end;

      declare
         use Rho.Transition, Rho.Transition.Container.Sequential;
         use Rho.Transition.Translation;
         Transition : constant Rho_Sequential_Transition :=
                        New_Sequential_Transition
                          (Cyclic => True);
      begin
         Transition.Append
           (Translate (Cylinder_Node, 5.0, 1.0, 1.0, -5.0));
         Transition.Append
           (Translate (Cylinder_Node, 5.0, 1.0, 1.0, 0.0));
         Scene.Add_Transition (Transition);
      end;

      declare
         Listener : constant Rho.Frame_Event.Rho_Frame_Listener :=
                      new Local_Frame_Listener'
                        (Scene         => Scene,
                         Moving_Camera => Moving_Camera);
      begin
         Rho.Main.Add_Frame_Listener (Listener);
      end;

      Window.Set_Scene (Scene);

   end Create_Window;

   -------------------
   -- Frame_Started --
   -------------------

   overriding procedure Frame_Started
     (Listener : in out Local_Frame_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event)
   is
      pragma Unreferenced (Event);
      use type Rho.Mouse.Button_State;
      Left_State : constant Rho.Mouse.Button_State :=
                     Rho.Mouse.Current_Mouse.Current_Button_State
                       (Rho.Mouse.Left);
   begin
      if Left_State = Rho.Mouse.Down then
         Listener.Scene.Use_Camera (Listener.Moving_Camera);
      else
         Listener.Scene.Use_Default_Camera;
      end if;
   end Frame_Started;

end Quadrics;
