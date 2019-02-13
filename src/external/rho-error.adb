with GL;

package body Rho.Error is

   -----------------
   -- Check_Error --
   -----------------

   procedure Check_Error (Last_Call : String := "") is
      use type GL.ErrorEnm;
      Error : constant GL.ErrorEnm := GL.GetError;
   begin
      if Error /= GL.NO_ERROR then
         if Last_Call = "" then
            raise Constraint_Error with
              "GL error: " & Error'Img;
         else
            raise Constraint_Error with
              "GL error in " & Last_Call & ": " & Error'Img;
         end if;
      end if;
   end Check_Error;

end Rho.Error;
