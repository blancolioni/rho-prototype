with Rho.Main;

with Rho.Toolkit.Events;
with Rho.Toolkit.Signals;

package body Rho.Toolkit.Page is

   type Page_Listener (Page : access Rho_Page_Record'Class) is
     new Rho.Frame_Event.Rho_Frame_Listener_Interface with
      record
         Last_Mouse  : Rho.Mouse.Mouse_State;
         Last_Widget : Rho.Toolkit.Widget.Rho_Widget := null;
      end record;

   overriding procedure Frame_Started
     (Listener : in out Page_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event);

   --------------------
   -- Execute_Render --
   --------------------

   overriding procedure Execute_Render
     (Page   : in out Rho_Page_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
   begin
      if not Page.Visible then
         return;
      end if;
      if Float (Page.Viewport.Width) /= Page.Layout_Width
        or else Float (Page.Viewport.Height) /= Page.Layout_Height
      then
         Page.Set_Layout_Width (Float (Page.Viewport.Width));
         Page.Set_Layout_Height (Float (Page.Viewport.Height));
         Page.Apply_Layout;
      end if;

      for Child of Page.Child_Elements loop
         Rho_Top_Level_Interface'Class (Child.all).Execute_Render (Target);
      end loop;
   end Execute_Render;

   -------------------
   -- Frame_Started --
   -------------------

   overriding procedure Frame_Started
     (Listener : in out Page_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event)
   is
      pragma Unreferenced (Event);
      use type Ada.Calendar.Time;
      use type Rho.Keyboard.Rho_Key_Code;
      use type Rho.Toolkit.Widget.Rho_Widget;
      Key_Data : Rho.Toolkit.Events.Key_Event_Data;
      Button_Data : Rho.Toolkit.Events.Button_Event_Data;
      Mouse : constant Rho.Mouse.Mouse_State :=
                Rho.Mouse.Current_Mouse.State;

   begin
      if Listener.Page.Focus /= null then
         if Listener.Page.Current_Key /= Rho.Keyboard.No_Key then
            Key_Data.Key := Listener.Page.Current_Key;
            Key_Data.Control := Listener.Page.Current_Control;
            if not Rho.Keyboard.Key_Down (Listener.Page.Current_Key) then
               Listener.Page.Focus.Emit
                 (Rho.Toolkit.Signals.Signal_Key_Release_Event, Key_Data);
               Listener.Page.Current_Key := Rho.Keyboard.No_Key;
            elsif Ada.Calendar.Clock - Listener.Page.Last_Key_Press
              >= Listener.Page.Key_Repeat
            then
               Listener.Page.Focus.Emit
                 (Rho.Toolkit.Signals.Signal_Key_Press_Event, Key_Data);
               Listener.Page.Last_Key_Press := Ada.Calendar.Clock;
            end if;
         else
            Listener.Page.Current_Key :=
              Rho.Keyboard.First_Key_Down;

            if Listener.Page.Current_Key /= Rho.Keyboard.No_Key then
               Key_Data :=
                 (Listener.Page.Current_Key,
                  Listener.Page.Current_Control);

               Listener.Page.Focus.Emit
                 (Rho.Toolkit.Signals.Signal_Key_Press_Event, Key_Data);
               Listener.Page.Last_Key_Press := Ada.Calendar.Clock;
            end if;
         end if;
      end if;

      declare
         use Rho.Mouse;
      begin
         for Button in Mouse_Button loop
            if Mouse.Button (Button)
              /= Listener.Page.Current_Mouse.Button (Button)
            then
               declare
                  X : constant Rho_Float := Mouse.X;
                  Y : constant Rho_Float :=
                        Listener.Page.Viewport.Height - Mouse.Y;
                  Widget : constant Rho.Toolkit.Widget.Rho_Widget :=
                             Listener.Page.Get_Child_Widget (X, Y);
               begin
                  if Widget /= null then
                     Button_Data.Button := Button;
                     Button_Data.Control := Listener.Page.Current_Control;
                     Button_Data.X := X;
                     Button_Data.Y := Y;
                     if Mouse.Button (Button) = Down then
                        Widget.Log ("got focus");
                        Listener.Page.Focus := Widget;
                        Widget.Emit
                          (Rho.Toolkit.Signals.Signal_Button_Press_Event,
                           Button_Data);
                     end if;
                  end if;
               end;
            end if;
         end loop;
         Listener.Page.Current_Mouse := Mouse;
      end;
   end Frame_Started;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Page : in out Rho_Page_Record)
   is
   begin
      Rho.Toolkit.Container.Rho_Container_Record (Page).Initialize;
      Page.Last_Key_Press := Ada.Calendar.Clock;
      Page.Listener := new Page_Listener (Page'Unchecked_Access);
      Rho.Main.Add_Frame_Listener (Page.Listener);
   end Initialize;

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New (Page : in out Rho_Page) is
   begin
      Page := new Rho_Page_Record;
   end Rho_New;

   ---------------
   -- Set_Focus --
   ---------------

   procedure Set_Focus
     (Page  : in out Rho_Page_Record;
      Focus : access Rho.Toolkit.Widget.Rho_Widget_Record'Class)
   is
   begin
      Page.Focus := Rho.Toolkit.Widget.Rho_Widget (Focus);
   end Set_Focus;

   ------------------
   -- Set_Viewport --
   ------------------

   overriding procedure Set_Viewport
     (Page     : in out Rho_Page_Record;
      Viewport : Rho.Viewport.Rho_Viewport)
   is
   begin
      Page.Viewport := Viewport;
      Page.Set_Layout_Width (Float (Viewport.Width));
      Page.Set_Layout_Height (Float (Viewport.Height));

      Page.Set_Style
        ("width", Integer'Image (Integer (Viewport.Width)) & "px");
      Page.Set_Style
        ("height", Integer'Image (Integer (Viewport.Height)) & "px");
      for Child of Page.Child_Elements loop
         Rho_Top_Level_Interface'Class (Child.all).Set_Viewport (Viewport);
      end loop;
      Page.Apply_Layout;
   end Set_Viewport;

end Rho.Toolkit.Page;
