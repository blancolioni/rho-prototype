package body Rho.Event is

   -------------
   -- Event_X --
   -------------

   function Event_X
     (Event : Rho_Event)
      return Rho_Float
   is
   begin
      return Event.X;
   end Event_X;

   -------------
   -- Event_Y --
   -------------

   function Event_Y
     (Event : Rho_Event)
      return Rho_Float
   is
   begin
      return Event.Y;
   end Event_Y;

   ----------------
   -- Get_Signal --
   ----------------

   function Get_Signal
     (Event : Rho_Event)
      return Rho_Signal
   is
   begin
      return Event.Signal;
   end Get_Signal;

   ------------------------
   -- Mouse_Button_Click --
   ------------------------

   function Mouse_Button_Click
     (Button : Rho.Mouse.Mouse_Button;
      X, Y   : Rho_Float)
      return Rho_Event
   is
   begin
      return (Mouse_Button_Click, Button, X, Y);
   end Mouse_Button_Click;

   ------------------------
   -- Mouse_Button_Press --
   ------------------------

   function Mouse_Button_Press
     (Button : Rho.Mouse.Mouse_Button;
      X, Y   : Rho_Float)
      return Rho_Event
   is
   begin
      return (Mouse_Button_Press, Button, X, Y);
   end Mouse_Button_Press;

   --------------------------
   -- Mouse_Button_Release --
   --------------------------

   function Mouse_Button_Release
     (Button : Rho.Mouse.Mouse_Button;
      X, Y   : Rho_Float)
      return Rho_Event
   is
   begin
      return (Mouse_Button_Release, Button, X, Y);
   end Mouse_Button_Release;

end Rho.Event;
