with Ada.Text_IO;

package body Rho.Toolkit.Errors is

   -----------
   -- Error --
   -----------

   procedure Error (Message : String) is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "Maas: " & Message);
   end Error;

   -----------------
   -- Fatal_Error --
   -----------------

   procedure Fatal_Error (Message : String) is
   begin
      Error ("FATAL: " & Message);
      raise Program_Error with Message;
   end Fatal_Error;

   -------------
   -- Warning --
   -------------

   procedure Warning (Message : String) is
   begin
      Error ("warning: " & Message);
   end Warning;

end Rho.Toolkit.Errors;
