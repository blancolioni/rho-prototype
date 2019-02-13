with Rho.Keyboard;
with Rho.Mouse;
with Rho.Rectangle;

package Rho.Toolkit.Events is

   type Event_Response is
     (Stop_Event,
      Propagate_Event);

   type User_Data_Interface is interface;

   type User_Data is access all User_Data_Interface'Class;

   type Signal_Data_Interface is interface;

   type Configure_Event_Data is
     new Signal_Data_Interface with
      record
         Width, Height : Rho.Non_Negative_Float;
      end record;

   type Draw_Event_Data is
     new Signal_Data_Interface with
      record
         Context : Cairo.Cairo_Context;
         Region  : Rho.Rectangle.Rho_Rectangle;
      end record;

   type Key_Event_Data is
     new Signal_Data_Interface with
      record
         Key     : Rho.Keyboard.Rho_Key_Code;
         Control : Rho.Keyboard.Control_Mask;
      end record;

   type Button_Event_Data is
     new Signal_Data_Interface with
      record
         Button  : Rho.Mouse.Mouse_Button;
         Control : Rho.Keyboard.Control_Mask;
         X, Y    : Rho_Float;
      end record;

end Rho.Toolkit.Events;
