with Ada.Text_IO;

with Demos;

with Rho_Demo_Loader;

with Rho.Keyboard;
with Rho.Main;
with Rho.Mouse;
with Rho.Render_Window;
with Rho.Paths;

with Rho.Toolkit.Builder;
with Rho.Toolkit.Events;
with Rho.Toolkit.Page;
with Rho.Toolkit.Widget;
with Rho.Scene;

package body Rho_Demo_Main is

   use Rho;

   Window       : Rho.Render_Window.Rho_Render_Window;
   Top_Level    : Rho.Toolkit.Page.Rho_Page;
   Control_Page : Rho.Toolkit.Page.Rho_Page;

   function Choose_Demo
     (Widget : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class;
      X, Y   : Rho_Float;
      Button : Rho.Mouse.Mouse_Button;
      Mask   : Rho.Keyboard.Control_Mask)
      return Rho.Toolkit.Events.Event_Response;

   function Exit_Demo
     (Widget : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class;
      X, Y   : Rho_Float;
      Button : Rho.Mouse.Mouse_Button;
      Mask   : Rho.Keyboard.Control_Mask)
      return Rho.Toolkit.Events.Event_Response;

   -----------------
   -- Choose_Demo --
   -----------------

   function Choose_Demo
     (Widget : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class;
      X, Y   : Rho_Float;
      Button : Rho.Mouse.Mouse_Button;
      Mask   : Rho.Keyboard.Control_Mask)
      return Rho.Toolkit.Events.Event_Response
   is
      pragma Unreferenced (X, Y, Mask);
      use Rho.Mouse;
      Demo : constant Demos.Rho_Demo_Type := Demos.Demo (Widget.Id);
   begin
      if Button = Left then
         Ada.Text_IO.Put_Line
           ("Loading demo: " & Demo.Name);
         declare
            Old_Scene : constant Rho.Scene.Rho_Scene := Window.Scene
              with Unreferenced;
            New_Scene : constant Rho.Scene.Rho_Scene := Demo.Scene;
         begin
            Window.Set_Wireframe (False);
            Window.Set_Scene (New_Scene);
            Top_Level.Hide;
            Control_Page.Show_All;
         end;
         return Rho.Toolkit.Events.Stop_Event;
      else
         return Rho.Toolkit.Events.Propagate_Event;
      end if;
   end Choose_Demo;

   ---------------
   -- Exit_Demo --
   ---------------

   function Exit_Demo
     (Widget : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class;
      X, Y   : Rho_Float;
      Button : Rho.Mouse.Mouse_Button;
      Mask   : Rho.Keyboard.Control_Mask)
      return Rho.Toolkit.Events.Event_Response
   is
      pragma Unreferenced (Widget, X, Y, Button, Mask);
   begin
      Window.Set_Scene (null);
      Control_Page.Hide;
      Top_Level.Show_All;
      return Rho.Toolkit.Events.Stop_Event;
   end Exit_Demo;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Rho.Main.Init;

      Window :=  Rho.Main.Current_Renderer.Create_Top_Level_Window;

      Rho.Toolkit.Initialize;

      Rho_Demo_Loader.Load_Demos;

      declare
         Builder : constant Rho.Toolkit.Builder.Rho_Builder :=
                     Rho.Toolkit.Builder.Rho_New_From_File
                       (Rho.Paths.Config_File ("demos/demo.html"));

         Page    : constant Rho.Toolkit.Page.Rho_Page :=
                     Builder.Get_Page;

      begin
         Top_Level := Page;
         Page.Set_Viewport (Window.Full_Viewport);
         Page.Show_All;
         Window.Add_Top_Level (Page);

         for I in 1 .. Demos.Demo_Count loop
            declare
               Name : constant String := Demos.Demo (I).Identity;
               W    : constant Rho.Toolkit.Widget.Rho_Widget :=
                        Builder.Get (Name);
            begin
               W.On_Button_Press
                 (Choose_Demo'Access);
            end;
         end loop;
      end;

      declare
         Builder : constant Rho.Toolkit.Builder.Rho_Builder :=
                     Rho.Toolkit.Builder.Rho_New_From_File
                       (Rho.Paths.Config_File ("demos/control.html"));

         Page    : constant Rho.Toolkit.Page.Rho_Page :=
                     Builder.Get_Page;

      begin
         Control_Page := Page;
         Page.Set_Viewport (Window.Full_Viewport);
         Builder.Get ("exit").On_Button_Press (Exit_Demo'Access);
         Window.Add_Top_Level (Control_Page);
      end;

      Rho.Main.Main_Loop;

   end Start;

end Rho_Demo_Main;
