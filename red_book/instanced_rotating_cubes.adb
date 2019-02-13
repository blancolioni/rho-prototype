with Maas;                               use Maas;

with Rho.Camera;
with Rho.Frame_Event;
with Rho.Main;
with Rho.Matrices;
with Rho.Mouse;
with Rho.Node;
with Rho.Scene;
with Rho.Shapes;
with Rho.Render_Window;

package body Instanced_Rotating_Cubes is

   type Frame_Listener is
     new Rho.Frame_Event.Rho_Frame_Listener_Interface with
      record
         Top_Node     : Rho.Node.Rho_Node;
         Bar_Degrees  : Rho_Float := 0.0;
         Cube_Degrees : Rho_Float := 0.0;
         Speed        : Rho_Float := 0.003;
      end record;

   overriding procedure Frame_Started
     (Listener : in out Frame_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event);

   -------------------
   -- Create_Window --
   -------------------

   procedure Create_Window is
      Scene       : constant Rho.Scene.Rho_Scene := Rho.Scene.Create_Scene;
      Camera      : constant Rho.Camera.Rho_Camera := Scene.Active_Camera;
      Top_Node    : constant Rho.Node.Rho_Node := Scene.Create_Node ("top");
      Window      : constant Rho.Render_Window.Rho_Render_Window :=
                      Rho.Main.Current_Renderer.Create_Top_Level_Window;
   begin
      Window.Set_Wireframe (True);
      Window.Set_Back_Face_Removal (False);

      Camera.Set_Viewport (Window.Full_Viewport);
      Camera.Set_Position (0.0, 0.0, 5.0);
      Camera.Set_Orientation (0.0, 0.0, 1.0, 0.0);
      Camera.Look_At (0.0, 0.0, 0.0);
      Camera.Frustum (-1.0, 1.0, -1.0, 1.0, 1.5, 20.0);

      Top_Node.Set_Entity (Rho.Shapes.Cube);
      Top_Node.Set_Instanced (2);

      Window.Set_Scene (Scene);

      declare
         Listener : constant Rho.Frame_Event.Rho_Frame_Listener :=
                      new Frame_Listener'
                        (Top_Node     => Top_Node,
                         Bar_Degrees  => 0.0,
                         Cube_Degrees => 0.0,
                         Speed        => 3.0);
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
      pragma Unreferenced (Event);
      use type Rho.Mouse.Button_State;
      Left_State : constant Rho.Mouse.Button_State :=
                     Rho.Mouse.Current_Mouse.Current_Button_State
                       (Rho.Mouse.Left);
      Right_State : constant Rho.Mouse.Button_State :=
                      Rho.Mouse.Current_Mouse.Current_Button_State
                        (Rho.Mouse.Right);
   begin
      if Left_State = Rho.Mouse.Down then
         Listener.Bar_Degrees := Listener.Bar_Degrees + Listener.Speed;
      end if;

      if Right_State = Rho.Mouse.Down then
         Listener.Cube_Degrees := Listener.Cube_Degrees + Listener.Speed;
      end if;

      declare
         use Rho.Matrices;
         Bar_Rotation  : constant Matrix_3 :=
                           Rotation_Matrix
                             (Listener.Bar_Degrees, (0.0, 1.0, 0.0));
      begin
         Listener.Top_Node.Set_Orientation (Bar_Rotation);
      end;
   end Frame_Started;

end Instanced_Rotating_Cubes;
