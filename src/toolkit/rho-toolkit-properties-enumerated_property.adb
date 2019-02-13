with Ada.Strings.Fixed;

package body Rho.Toolkit.Properties.Enumerated_Property is

   -----------------
   -- Parse_Value --
   -----------------

   overriding procedure Parse_Value
     (Property : in out Rho_Property;
      Value    : in     String)
   is
   begin
      Property.Value := Enum'Value (Value);
   end Parse_Value;

   --------------
   -- Property --
   --------------

   procedure Property
     (List  : in out Rho_Property_List'Class;
      Name  : String;
      Value : Enum)
   is
      P : Rho_Property;
   begin
      P.Create (Name);
      P.Value := Value;
      List.Add (P);
   end Property;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Property : in out Root_Rho_Property'Class;
      Value    : Enum)
   is
   begin
      Rho_Property (Property).Value := Value;
   end Set_Value;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (Property : Rho_Property)
      return String
   is
   begin
      return Ada.Strings.Fixed.Trim
        (Enum'Image (Property.Value), Ada.Strings.Left);
   end To_String;

   -----------
   -- Value --
   -----------

   function Value (Property : Rho_Property) return Enum is
   begin
      return Property.Value;
   end Value;

end Rho.Toolkit.Properties.Enumerated_Property;
