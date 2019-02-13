with Maas;                               use Maas;

with Rho.Camera;
with Rho.Frame_Event;
with Rho.Main;
with Rho.Mouse;
with Rho.Scene;
with Rho.Render_Window;

with Xtk.Button;
with Xtk.Div_Element;
with Xtk.Events;
with Xtk.Label;
with Xtk.Orientable;
with Xtk.Panel;
with Xtk.Progress_Bar;

with Xtk.FPS;

package body Progress is

   Progress_Enabled : Boolean := False;
   Reset            : Boolean := False;

   type Button_Command is (Start_Progress, Stop_Progress, Reset_Progress);

   type Command_Button (Command : Button_Command) is
     new Xtk.Button.Xtk_Button_Record
     and Xtk.Events.Activate_Interface
       with null record;

   overriding procedure On_Activate
     (Button : in out Command_Button);

   type Progress_Listener is
     new Rho.Frame_Event.Rho_Frame_Listener_Interface with
      record
         Current_Value  : Rho_Float;
         Progress_Bar     : Xtk.Progress_Bar.Xtk_Progress_Bar;
         Position_Label : Xtk.Label.Xtk_Label;
      end record;

   overriding procedure Frame_Started
     (Listener : in out Progress_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event);

   -------------------
   -- Create_Window --
   -------------------

   procedure Create_Window is
      Scene : constant Rho.Scene.Rho_Scene := Rho.Scene.Create_Scene;
      Camera : constant Rho.Camera.Rho_Camera := Scene.Active_Camera;
      Window : constant Rho.Render_Window.Rho_Render_Window :=
                 Rho.Main.Current_Renderer.Create_Top_Level_Window;
      Progress_Bar     : Xtk.Progress_Bar.Xtk_Progress_Bar;
      Start_Button : Xtk.Button.Xtk_Button;
      Stop_Button  : Xtk.Button.Xtk_Button;
      Reset_Button : Xtk.Button.Xtk_Button;
      Position_Label   : Xtk.Label.Xtk_Label;
      Button_Grid      : Xtk.Grid.Xtk_Grid;
      Top_Grid         : Xtk.Grid.Xtk_Grid;
      Panel            : Xtk.Panel.Xtk_Panel;
   begin
      Xtk.Initialize;
      Camera.Set_Viewport (Window.Full_Viewport);
      Camera.Set_Position (0.0, 0.0, 5.0);
      Camera.Set_Orientation (0.0, 0.0, 1.0, 0.0);
      Camera.Look_At (0.0, 1.0, 0.0, 0.0, 0.0, 0.0);
      Camera.Frustum (-1.0, 1.0, -1.0, 1.0, 1.5, 20.0);
      Window.Set_Scene (Scene);

      Xtk.Progress_Bar.Xtk_New
        (Item  => Progress_Bar,
         Low   => 0.0,
         High  => 1.0,
         Value => 0.2);

      Progress_Bar.Set_Id ("progress-bar");

      Start_Button := new Command_Button (Start_Progress);
      Start_Button.Create_With_Label ("Start", "start-button");

      Stop_Button := new Command_Button (Stop_Progress);
      Stop_Button.Create_With_Label ("Stop", "stop-button");

      Reset_Button := new Command_Button (Reset_Progress);
      Reset_Button.Create_With_Label ("Reset", "reset-button");

      Xtk.Label.Xtk_New (Position_Label, "");
      Position_Label.Set_Id ("position-label");

      Xtk.Grid.Xtk_New (Button_Grid);
      Button_Grid.Set_Id ("button-grid");
      Button_Grid.Set_Orientation (Xtk.Orientable.Across);
      Button_Grid.Add (Start_Button);
      Button_Grid.Add (Stop_Button);
      Button_Grid.Add (Reset_Button);
      Button_Grid.Add (Position_Label);

      Xtk.Grid.Xtk_New (Top_Grid);
      Top_Grid.Set_Id ("top-grid");
      Top_Grid.Set_Orientation (Xtk.Orientable.Down);
      Top_Grid.Add (Button_Grid);
      Top_Grid.Add (Progress_Bar);

      Xtk.Panel.Xtk_New
        (Panel    => Panel,
         Top      => Top_Grid);

      Panel.Set_Viewport (Window.Full_Viewport);
      Panel.Center;

      Window.Add_Top_Level (Panel);
      Panel.Show_All;

      if True then
         declare
            FPS_Panel : Xtk.Panel.Xtk_Panel;
         begin
            Xtk.Panel.Xtk_New
              (FPS_Panel,
               Xtk.FPS.Create_FPS_Widget);

            FPS_Panel.Set_Viewport (Window.Full_Viewport);
            FPS_Panel.Position_Anchor (Xtk.Left, Xtk.Top);
            Window.Add_Top_Level (FPS_Panel);
            FPS_Panel.Show_All;
         end;
      end if;

      declare
         Listener : constant Rho.Frame_Event.Rho_Frame_Listener :=
                      new Progress_Listener'(Current_Value => 0.0,
                                             Progress_Bar => Progress_Bar,
                                             Position_Label => Position_Label);
      begin
         Rho.Main.Add_Frame_Listener (Listener);
      end;

   end Create_Window;

   -------------------
   -- Frame_Started --
   -------------------

   overriding procedure Frame_Started
     (Listener : in out Progress_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event)
   is
      pragma Unreferenced (Event);
   begin
      if Reset then
         Reset := False;
         Listener.Current_Value := 0.0;
      end if;

      Listener.Progress_Bar.Set_Value (Listener.Current_Value);
      if Progress_Enabled then
         Listener.Current_Value :=
           Rho_Float'Min (Listener.Current_Value + 0.001, 1.0);
         if Listener.Current_Value in 0.25 .. 0.26 then
            Listener.Current_Value := 0.7;
         end if;
      end if;
      Listener.Position_Label.Set_Label
        (Integer'Image (Integer (Rho.Mouse.Current_Mouse.State.X))
         & Integer'Image (Integer (Rho.Mouse.Current_Mouse.State.Y)));

   end Frame_Started;

   -----------------
   -- On_Activate --
   -----------------

   overriding procedure On_Activate
     (Button : in out Command_Button)
   is
   begin
      case Button.Command is
         when Stop_Progress =>
            Progress_Enabled := False;
         when Start_Progress =>
            Progress_Enabled := True;
         when Reset_Progress =>
            Reset := True;
      end case;
   end On_Activate;

end Progress;
