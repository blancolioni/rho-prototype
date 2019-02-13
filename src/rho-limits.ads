package Rho.Limits is

   Max_Lights : constant := 2;

   type Light_Count is range 0 .. Max_Lights;
   subtype Light_Index is Light_Count range 1 .. Max_Lights;

end Rho.Limits;
