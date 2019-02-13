package Rho.Toolkit.Properties.String_Property is

   type Rho_String_Property is
     new Root_Rho_Property with private;

   overriding procedure Parse_Value
     (Property : in out Rho_String_Property;
      Value    : in     String);

   overriding function To_String
     (Property : Rho_String_Property)
      return String;

   procedure Property
     (List  : in out Rho_Property_List'Class;
      Name  : String;
      Value : String);

private

   type Rho_String_Property is
     new Root_Rho_Property with
      record
         Value : Ada.Strings.Unbounded.Unbounded_String;
      end record;

end Rho.Toolkit.Properties.String_Property;
