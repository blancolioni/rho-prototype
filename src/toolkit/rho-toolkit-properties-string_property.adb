package body Rho.Toolkit.Properties.String_Property is

   -----------------
   -- Parse_Value --
   -----------------

   overriding procedure Parse_Value
     (Property : in out Rho_String_Property;
      Value    : in     String)
   is
   begin
      Property.Value := Ada.Strings.Unbounded.To_Unbounded_String (Value);
   end Parse_Value;

   --------------
   -- Property --
   --------------

   procedure Property
     (List  : in out Rho_Property_List'Class;
      Name  : String;
      Value : String)
   is
      P : Rho_String_Property;
   begin
      P.Create (Name);
      P.Value := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      List.Add (P);
   end Property;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Property : Rho_String_Property)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Property.Value);
   end To_String;

end Rho.Toolkit.Properties.String_Property;
