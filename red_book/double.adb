with Ada.Calendar;

with Maas;                               use Maas;

with Rho.Assets;
with Rho.Camera;
with Rho.Frame_Event;
with Rho.Logging;
with Rho.Main;
with Rho.Matrices;
with Rho.Mouse;
with Rho.Node;
with Rho.Scene;
with Rho.Shapes;
with Rho.Render_Window;

package body Double is

   type Double_Frame_Listener is
     new Rho.Frame_Event.Rho_Frame_Listener_Interface with
      record
         Count : Natural := 0;
         Spin  : Rho_Float := 0.0;
         Node  : Rho.Node.Rho_Node;
      end record;

   overriding procedure Frame_Started
     (Listener : in out Double_Frame_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event);

   Sample_Time : Ada.Calendar.Time;

   -------------------
   -- Create_Window --
   -------------------

   procedure Create_Window is
      Scene : constant Rho.Scene.Rho_Scene := Rho.Scene.Create_Scene;
      Camera : constant Rho.Camera.Rho_Camera := Scene.Active_Camera;
      Node  : constant Rho.Node.Rho_Node := Scene.Create_Node ("wire_cube");
      Window : constant Rho.Render_Window.Rho_Render_Window :=
                 Rho.Main.Current_Renderer.Create_Top_Level_Window;
   begin
      Window.Set_Wireframe (True);
      Camera.Set_Viewport (Window.Full_Viewport);
      Camera.Set_Position (0.0, 0.0, 5.0);
      Camera.Set_Orientation (0.0, 0.0, 1.0, 0.0);
      Camera.Look_At (0.0, 0.0, 0.0);
      Camera.Frustum (-1.0, 1.0, -1.0, 1.0, 1.5, 20.0);
      Node.Set_Entity (Rho.Shapes.Quadric_Sphere (8, 8));
      Node.Entity.Set_Material (Rho.Assets.Material ("Rho.Blue"));
      Node.Scale (1.0, 1.0, 1.0);
      Window.Set_Scene (Scene);

      declare
         Listener : constant Rho.Frame_Event.Rho_Frame_Listener :=
                      new Double_Frame_Listener'
                        (Spin => 0.0, Node => Node, Count => 0);
      begin
         Rho.Main.Add_Frame_Listener (Listener);
      end;

      Sample_Time := Ada.Calendar.Clock;

   end Create_Window;

   -------------------
   -- Frame_Started --
   -------------------

   overriding procedure Frame_Started
     (Listener : in out Double_Frame_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event)
   is
      use type Rho.Mouse.Button_State;
      State : constant Rho.Mouse.Button_State :=
                Rho.Mouse.Current_Mouse.Current_Button_State
                  (Rho.Mouse.Left);
   begin
      if State = Rho.Mouse.Down then
         Listener.Spin := Listener.Spin
           + 5.0 * Rho_Float (Event.Time_Since_Last_Event);
         if Listener.Spin >= 360.0 then
            Listener.Spin := Listener.Spin - 360.0;
         end if;
         Listener.Node.Set_Orientation
           (Rho.Matrices.Rotation_Matrix
              (Listener.Spin, (0.0, 0.0, 1.0)));
      end if;

      Listener.Count := Listener.Count + 1;
      if Listener.Count = 100 then
         declare
            use Ada.Calendar;
            Now : constant Time := Clock;
            D : constant Duration := Now - Sample_Time;
         begin
            Rho.Logging.Put (100.0 / Rho_Float (D), 2);
            Rho.Logging.Put_Line (" FPS");
            Sample_Time := Now;
         end;
         Listener.Count := 0;
      end if;

   end Frame_Started;

end Double;
