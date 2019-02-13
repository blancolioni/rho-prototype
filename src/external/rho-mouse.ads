package Rho.Mouse is

   type Mouse_Button is (Left, Middle, Right);

   type Mouse_Wheel is (None, Move_Up, Move_Down);

   type Button_State is (Up, Down);

   type Button_State_Array is array (Mouse_Button) of Button_State;

   type Mouse_State is
      record
         X, Y      : Rho_Float := -1.0;
         Reverse_Y : Rho_Float := -1.0;
         Button    : Button_State_Array := (others => Up);
         Wheel     : Mouse_Wheel := None;
      end record;

   type Rho_Mouse_State_Record is abstract tagged private;

   function State (State  : Rho_Mouse_State_Record) return Mouse_State
                   is abstract;

   function Current_Button_State (State  : Rho_Mouse_State_Record;
                                  Button : Mouse_Button)
                                  return Button_State
   is (Rho_Mouse_State_Record'Class (State).State.Button (Button));

   function Button_Down (State  : Rho_Mouse_State_Record;
                         Button : Mouse_Button)
                         return Boolean
   is (Rho_Mouse_State_Record'Class (State).State.Button (Button) = Down);

   function Button_Up (State  : Rho_Mouse_State_Record;
                       Button : Mouse_Button)
                       return Boolean
   is (Rho_Mouse_State_Record'Class (State).State.Button (Button) = Up);

   function Wheel_Up (State  : Rho_Mouse_State_Record)
                      return Boolean
   is (Rho_Mouse_State_Record'Class (State).State.Wheel = Move_Up);

   function Wheel_Down (State  : Rho_Mouse_State_Record)
                        return Boolean
   is (Rho_Mouse_State_Record'Class (State).State.Wheel = Move_Down);

   type Rho_Mouse_State is access all Rho_Mouse_State_Record'Class;

   function Current_Mouse return Rho_Mouse_State;

   procedure Set_Current_Mouse
     (Mouse : Rho_Mouse_State);

private

   type Rho_Mouse_State_Record is abstract tagged null record;

end Rho.Mouse;
