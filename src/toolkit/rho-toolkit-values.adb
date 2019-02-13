with Ada.Strings.Unbounded;

package body Rho.Toolkit.Values is

   type Rho_String_Value is
     new Rho_Value_Interface with
      record
         String_Value : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function To_String
     (Value : Rho_String_Value)
      return String
   is (Ada.Strings.Unbounded.To_String (Value.String_Value));

   ------------------
   -- To_Rho_Value --
   ------------------

   function To_Rho_Value (Text : String) return Rho_Value_Interface'Class is
   begin
      return Rho_String_Value'
        (String_Value => Ada.Strings.Unbounded.To_Unbounded_String (Text));
   end To_Rho_Value;

end Rho.Toolkit.Values;
