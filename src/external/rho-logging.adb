with Ada.Text_IO;
with Ada.Float_Text_IO;

package body Rho.Logging is

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Ada.Text_IO.New_Line;
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Message : String) is
   begin
      Ada.Text_IO.Put (Message);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (X        : Rho_Float;
                  Decimals : Natural := 3)
   is
   begin
      Ada.Float_Text_IO.Put (Float (X), 1, Decimals, 0);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (V : Rho.Float_Arrays.Real_Vector) is
      First : Boolean := True;
   begin
      for X of V loop
         if First then
            Put ("(");
            First := False;
         else
            Put (",");
         end if;
         Ada.Float_Text_IO.Put (Float (X), 1, 4, 0);
      end loop;
      Put (")");
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (M : Rho.Float_Arrays.Real_Matrix) is
   begin
      for I in M'Range (1) loop
         for J in M'Range (2) loop
            Ada.Float_Text_IO.Put (Float (M (I, J)), 1, 4, 0);
            Ada.Text_IO.Put (" ");
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Put_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Message : String) is
   begin
      Put (Message);
      New_Line;
   end Put_Line;

end Rho.Logging;
