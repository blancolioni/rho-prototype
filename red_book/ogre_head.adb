with Ada.Calendar;

with GL;

with Maas;                               use Maas;

with Rho.Assets;
with Rho.Camera;
with Rho.Entity;
with Rho.Float_Arrays;
with Rho.Frame_Event;
with Rho.Light;
with Rho.Logging;
with Rho.Main;
with Rho.Matrices;
with Rho.Mesh;
with Rho.Mouse;
with Rho.Node;
with Rho.Scene;
with Rho.Render_Window;

package body Ogre_Head is

   type Double_Frame_Listener is
     new Rho.Frame_Event.Rho_Frame_Listener_Interface with
      record
         Count : Natural := 0;
         Roll  : Rho_Float := 0.0;
         Pitch : Rho_Float := 0.0;
         Node  : Rho.Node.Rho_Node;
         Camera : Rho.Camera.Rho_Camera;
         Spin   : Rho_Float := 0.0;
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
      Node  : constant Rho.Node.Rho_Node := Scene.Create_Node ("ogre");
      Light  : Rho.Light.Rho_Light;
      Window : constant Rho.Render_Window.Rho_Render_Window :=
                 Rho.Main.Current_Renderer.Create_Top_Level_Window;
      Ogre_Mesh : constant Rho.Mesh.Rho_Mesh :=
                    Rho.Assets.Mesh ("ogrehead");
      Ogre      : constant Rho.Entity.Rho_Entity :=
                    Ogre_Mesh.Create_Entity;
   begin

      if False then
         GL.Enable_Debug;
      end if;

      Window.Set_Wireframe (False);
      Window.Set_Back_Face_Removal (True);
      Window.Set_Depth_Test (True);

      Camera.Set_Viewport (Window.Full_Viewport);
      Camera.Set_Position (0.0, 0.0, 100.0);
      Camera.Look_At (0.0, 1.0, 0.0, 0.0, 0.0, 0.0);
      Camera.Frustum (-15.0, 15.0, -15.0, 15.0, 5.0, 500.0);

      Rho.Light.Rho_New (Light, Rho.Light.Ambient);
      Light.Set_Position (100.0, 30.0, 30.0);
      Light.Set_Color (0.5, 0.5, 0.5, 1.0);
      Scene.Add_Light (Light);
      Node.Set_Entity (Ogre);
      Window.Set_Scene (Scene);

      declare
         Listener : constant Rho.Frame_Event.Rho_Frame_Listener :=
                      new Double_Frame_Listener'
                        (Roll => 0.0, Pitch => 0.0,
                         Node => Node, Count => 0,
                         Spin => 0.0,
                         Camera => Camera);
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
      Left_State : constant Rho.Mouse.Button_State :=
                     Rho.Mouse.Current_Mouse.Current_Button_State
                       (Rho.Mouse.Left);
      Right_State : constant Rho.Mouse.Button_State :=
                      Rho.Mouse.Current_Mouse.Current_Button_State
                        (Rho.Mouse.Right);
      Look_At : Rho.Matrices.Vector_3 := (0.0, 0.0, -100.0);
   begin
      declare
         Aspect : constant Rho_Float :=
                    Listener.Camera.Viewport.Width
                      / Listener.Camera.Viewport.Height;
         Width  : constant Rho_Float := 30.0 * Aspect;
      begin
         Listener.Camera.Frustum
           (-Width / 2.0, Width / 2.0, -15.0, 15.0, 50.0, 250.0);
      end;
      if Left_State = Rho.Mouse.Down then
         Look_At := (0.0, -10.0, -100.0);
         Listener.Roll := Listener.Roll
           + 15.0 * Rho_Float (Event.Time_Since_Last_Event);
         if Listener.Roll >= 360.0 then
            Listener.Roll := Listener.Roll - 360.0;
         end if;

      end if;

      if Right_State = Rho.Mouse.Down then
         Look_At := (0.0, 10.0, -100.0);
         Listener.Pitch := Listener.Pitch
           + 15.0 * Rho_Float (Event.Time_Since_Last_Event);
         if Listener.Pitch >= 360.0 then
            Listener.Pitch := Listener.Pitch - 360.0;
         end if;
      end if;

      declare
         use Rho.Float_Arrays;
      begin
         if True then
            Listener.Node.Look_At ((0.0, 1.0, 0.0), Look_At);
         else
            Listener.Node.Set_Orientation
              (Rho.Matrices.Rotation_Matrix
                 (Listener.Roll, (0.0, 0.0, 1.0))
               * Rho.Matrices.Rotation_Matrix
                 (Listener.Pitch, (1.0, 0.0, 0.0)));
         end if;
      end;

      Listener.Count := Listener.Count + 1;
      if True and then Listener.Count = 1000 then
         declare
            use Ada.Calendar;
            Now : constant Time := Clock;
            D : constant Duration := Now - Sample_Time;
         begin
            Rho.Logging.Put (1000.0 / Rho_Float (D), 2);
            Rho.Logging.Put_Line (" FPS");
            Sample_Time := Now;
         end;
         Listener.Count := 0;
      end if;

      Listener.Node.Rotate (Listener.Spin, 0.0, 1.0, 0.0);
      Listener.Spin := Listener.Spin
        + 1.0 * Rho_Float (Event.Time_Since_Last_Event);
   end Frame_Started;

end Ogre_Head;
