generic
   type Enum is (<>);
package Rho.Toolkit.Properties.Enumerated_Property is

   type Rho_Property is
     new Root_Rho_Property with private;

   overriding procedure Parse_Value
     (Property : in out Rho_Property;
      Value    : in     String);

   overriding function To_String
     (Property : Rho_Property)
      return String;

   procedure Property
     (List  : in out Rho_Property_List'Class;
      Name  : String;
      Value : Enum);

   function Value (Property : Rho_Property) return Enum;

   procedure Set_Value (Property : in out Root_Rho_Property'Class;
                        Value    : Enum);

private

   type Rho_Property is
     new Root_Rho_Property with
      record
         Value : Enum;
      end record;

end Rho.Toolkit.Properties.Enumerated_Property;
