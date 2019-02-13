with Rho.Float_Arrays;

package Rho.Logging is

   procedure Put (Message : String);
   procedure Put_Line (Message : String);
   procedure Put (V : Rho.Float_Arrays.Real_Vector);
   procedure Put_Vector (V : Rho.Float_Arrays.Real_Vector)
     renames Put;
   procedure Put (X        : Rho_Float;
                  Decimals : Natural := 3);
   procedure Put_Line (M : Rho.Float_Arrays.Real_Matrix);
   procedure New_Line;

end Rho.Logging;
