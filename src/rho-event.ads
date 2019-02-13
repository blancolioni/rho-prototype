with Rho.Mouse;

package Rho.Event is

   type Rho_Signal is
     (Mouse_Button_Press, Mouse_Button_Release,
      Mouse_Button_Click, Mouse_Button_Double_Click,
      Mouse_Pointer_Enter, Mouse_Pointer_Leave,
      Mouse_Pointer_Move);

   type Rho_Event is private;

   function Mouse_Button_Press
     (Button : Rho.Mouse.Mouse_Button;
      X, Y   : Rho_Float)
      return Rho_Event;

   function Mouse_Button_Release
     (Button : Rho.Mouse.Mouse_Button;
      X, Y   : Rho_Float)
      return Rho_Event;

   function Mouse_Button_Click
     (Button : Rho.Mouse.Mouse_Button;
      X, Y   : Rho_Float)
      return Rho_Event;

   function Get_Signal
     (Event : Rho_Event)
      return Rho_Signal;

   function Event_X
     (Event : Rho_Event)
      return Rho_Float;

   function Event_Y
     (Event : Rho_Event)
      return Rho_Float;

   type Rho_Event_Manager is interface;

   type Rho_Event_Source is interface;

   procedure On_Event
     (Source : not null access Rho_Event_Source;
      Event  : Rho_Event)
   is abstract;

   procedure Register_Event_Source
     (Manager : in out Rho_Event_Manager;
      Source  : not null access Rho_Event_Source'Class;
      Signal  : Rho_Signal)
   is abstract;

--     type Rho_Event_Class is
--       (Rho_Exposure,
--        Rho_Pointer_Motion, Rho_Pointer_Motion_Hint,
--        Rho.Toolkit_Button_Press, Rho.Toolkit_Button_Release,
--        Rho_Key_Press, Rho_Key_Release,
--        Rho_Enter, Rho_Leave);
--
--     type Rho_Event_Mask is array (Positive range <>) of Rho_Event_Class;
--
--     type Rho_Button is (Left_Button, Middle_Button, Right_Button);
--
--     type Rho_Key_Type is new Natural;
--
--     type Rho_Modifier_Key is (Shift, Control, Alt, Meta);
--
--     type Rho_Key_Modifiers is array (Rho_Modifier_Key) of Boolean;

private

   type Rho_Event is
      record
         Signal : Rho_Signal;
         Button : Rho.Mouse.Mouse_Button;
         X, Y   : Rho_Float;
      end record;

end Rho.Event;
