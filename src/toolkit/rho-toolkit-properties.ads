private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Fixed.Hash_Case_Insensitive;
private with Ada.Strings.Fixed.Equal_Case_Insensitive;
private with Ada.Strings.Unbounded;

package Rho.Toolkit.Properties is

   type Root_Rho_Property is abstract tagged private;

   procedure Create
     (Property : in out Root_Rho_Property'Class;
      Name     : in String);

   function Name (Property : Root_Rho_Property'Class) return String;

   procedure Parse_Value
     (Property : in out Root_Rho_Property;
      Value    : in     String)
   is abstract;

   function To_String
     (Property : Root_Rho_Property)
      return String
      is abstract;

   type Rho_Property_List_Interface is interface;

   function Property
     (List : Rho_Property_List_Interface;
      Name : String)
      return Root_Rho_Property'Class
      is abstract;

   function Has_Property
     (List : Rho_Property_List_Interface;
      Name : String)
      return Boolean
      is abstract;

   type Rho_Property_List is new Rho_Property_List_Interface with private;

   procedure Add
     (To_List  : in out Rho_Property_List;
      Property : Root_Rho_Property'Class);

   procedure Update
     (To_List  : in out Rho_Property_List;
      Property : Root_Rho_Property'Class);

   overriding function Property
     (List : Rho_Property_List;
      Name : String)
      return Root_Rho_Property'Class;

   overriding function Has_Property
     (List : Rho_Property_List;
      Name : String)
      return Boolean;

private

   type Root_Rho_Property is abstract tagged
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Property_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Root_Rho_Property'Class,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   type Rho_Property_List is new Rho_Property_List_Interface with
      record
         Map : Property_Maps.Map;
      end record;

end Rho.Toolkit.Properties;
