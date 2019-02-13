private with Ada.Calendar;

private with Rho.Frame_Event;
private with Rho.Render_Target;

private with Rho.Keyboard;
private with Rho.Mouse;

with Rho.Toolkit.Container;
with Rho.Toolkit.Widget;

package Rho.Toolkit.Page is

   type Rho_Page_Record is
     new Rho.Toolkit.Container.Rho_Container_Record
     and Rho_Top_Level_Interface
   with private;

   overriding procedure Initialize
     (Page : in out Rho_Page_Record);

   procedure Set_Focus
     (Page  : in out Rho_Page_Record;
      Focus : access Rho.Toolkit.Widget.Rho_Widget_Record'Class);

   type Rho_Page is access all Rho_Page_Record'Class;

   procedure Rho_New (Page : in out Rho_Page);

private

   type Rho_Page_Record is
     new Rho.Toolkit.Container.Rho_Container_Record
     and Rho_Top_Level_Interface with
      record
         Viewport        : Rho.Viewport.Rho_Viewport;
         Listener        : Rho.Frame_Event.Rho_Frame_Listener;
         Focus           : Rho.Toolkit.Widget.Rho_Widget;
         Key_Repeat      : Duration := 0.5;
         Current_Key     : Rho.Keyboard.Rho_Key_Code := Rho.Keyboard.No_Key;
         Current_Control : Rho.Keyboard.Control_Mask := (others => False);
         Current_Mouse   : Rho.Mouse.Mouse_State;
         Last_Key_Press  : Ada.Calendar.Time;
      end record;

   overriding function Loaded
     (Page : Rho_Page_Record)
      return Boolean
   is (True);

   overriding procedure Execute_Render
     (Page   : in out Rho_Page_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   overriding function Viewport
     (Page : Rho_Page_Record)
      return Rho.Viewport.Rho_Viewport
   is (Page.Viewport);

   overriding procedure Set_Viewport
     (Page     : in out Rho_Page_Record;
      Viewport : Rho.Viewport.Rho_Viewport);

end Rho.Toolkit.Page;
