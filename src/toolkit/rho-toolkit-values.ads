package Rho.Toolkit.Values is

   type Rho_Value_Interface is interface;

   function To_String (Value : Rho_Value_Interface) return String
                       is abstract;

   function To_Rho_Value (Text : String) return Rho_Value_Interface'Class;

end Rho.Toolkit.Values;
