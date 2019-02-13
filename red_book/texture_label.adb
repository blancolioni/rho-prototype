with Ada.Calendar;

with Maas;                               use Maas;

with Rho.Camera;
with Rho.Elementary_Functions;
with Rho.Font;
with Rho.Frame_Event;
with Rho.Label;
with Rho.Logging;
with Rho.Main;
with Rho.Matrices;
with Rho.Mouse;
with Rho.Node;
with Rho.Scene;
with Rho.Shapes;
with Rho.Render_Window;

package body Texture_Label is

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
      Node  : constant Rho.Node.Rho_Node := Scene.Create_Node ("top");
      Window : constant Rho.Render_Window.Rho_Render_Window :=
                 Rho.Main.Current_Renderer.Create_Top_Level_Window;
      Font   : constant Rho.Font.Rho_Font :=
                 Rho.Font.Get_Font
                   (Font_Name => "sans-serif",
                    Font_Size => 48.0,
                    Slant     => Rho.Font.Italic,
                    Weight    => Rho.Font.Normal);
      Label  : constant Rho.Label.Rho_Label :=
                 Rho.Label.Create_Label ("label1", "Texture Label", Font);
      Label_Node : constant Rho.Node.Rho_Node :=
                     Scene.Create_Node
                       ("label node");
   begin
      Camera.Set_Viewport (Window.Full_Viewport);
      Camera.Set_Position (0.0, 0.0, 50.0);
      Camera.Set_Orientation (0.0, 0.0, 1.0, 0.0);
      Camera.Look_At (0.0, 1.0, 0.0, 0.0, 0.0, 0.0);
      Camera.Frustum (-1.0, 1.0, -1.0, 1.0, 1.5, 150.0);
      Node.Scale (10.0, 1.0, 1.0);
      Node.Set_Position (-7.0, 0.0, 0.0);
      Node.Set_Entity (Rho.Shapes.Cube);
      Label_Node.Scale (0.05, 0.05, 0.05);
      Label_Node.Set_Entity (Label);
      Label_Node.Set_Position (1.0, 0.0, 0.0);
      Window.Set_Scene (Scene);

      declare
         Listener : constant Rho.Frame_Event.Rho_Frame_Listener :=
                      new Double_Frame_Listener'
                        (Spin => 0.0, Node => Label_Node, Count => 0);
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
      use Rho.Elementary_Functions;
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
         if True then
            Listener.Node.Look_At
              ((0.0, 1.0, 0.0),
               (100.0 * Cos (Listener.Spin, 360.0),
                0.0,
                -100.0 * Sin (Listener.Spin, 360.0)));
         else
            Listener.Node.Set_Orientation
              (Rho.Matrices.Rotation_Matrix
                 (Listener.Spin, (0.0, 1.0, 0.0)));
         end if;
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

end Texture_Label;
