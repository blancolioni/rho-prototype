package Rho.Toolkit.Signals is

   type Signal_Type (<>) is private;

   Signal_Configure_Event      : constant Signal_Type;
   Signal_Draw_Event           : constant Signal_Type;

   Signal_Key_Press_Event      : constant Signal_Type;
   Signal_Key_Release_Event    : constant Signal_Type;

   Signal_Button_Press_Event   : constant Signal_Type;
   Signal_Button_Release_Event : constant Signal_Type;

private

   type Signal_Type is new String;

   Signal_Configure_Event      : constant Signal_Type := "signal-configure";
   Signal_Draw_Event           : constant Signal_Type := "signal-draw";

   Signal_Key_Press_Event      : constant Signal_Type :=
                                "signal-key-press-event";

   Signal_Key_Release_Event    : constant Signal_Type :=
                                "signal-key-release-event";

   Signal_Button_Press_Event   : constant Signal_Type :=
                                   "signal-button-press-event";
   Signal_Button_Release_Event : constant Signal_Type :=
                                   "signal-button-release-event";

end Rho.Toolkit.Signals;
