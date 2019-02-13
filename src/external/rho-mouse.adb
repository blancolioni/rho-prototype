package body Rho.Mouse is

   Local_Current_Mouse : Rho_Mouse_State;

   -------------------
   -- Current_Mouse --
   -------------------

   function Current_Mouse return Rho_Mouse_State is
   begin
      return Local_Current_Mouse;
   end Current_Mouse;

   -----------------------
   -- Set_Current_Mouse --
   -----------------------

   procedure Set_Current_Mouse
     (Mouse : Rho_Mouse_State)
   is
   begin
      Local_Current_Mouse := Mouse;
   end Set_Current_Mouse;

end Rho.Mouse;
