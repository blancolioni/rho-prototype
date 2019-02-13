with Ada.Containers.Indefinite_Hashed_Maps;

private with Ada.Strings.Fixed.Equal_Case_Insensitive;
private with Ada.Strings.Fixed.Hash_Case_Insensitive;

generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Rho.Toolkit.Signals.Maps is

   function Hash (Signal : Signal_Type) return Ada.Containers.Hash_Type;

   function Equal (Left, Right : Signal_Type) return Boolean;

   package Signal_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Signal_Type,
        Element_Type    => Element_Type,
        Hash            => Hash,
        Equivalent_Keys => Equal,
        "="             => "=");

   type Map is new Signal_Maps.Map with private;

private

   function Hash (Signal : Signal_Type) return Ada.Containers.Hash_Type
   is (Ada.Strings.Fixed.Hash_Case_Insensitive (String (Signal)));

   function Equal (Left, Right : Signal_Type) return Boolean
   is (Ada.Strings.Fixed.Equal_Case_Insensitive
       (String (Left), String (Right)));

   type Map is new Signal_Maps.Map with null record;

end Rho.Toolkit.Signals.Maps;
