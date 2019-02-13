with Ada.Unchecked_Deallocation;

with Rho.Matrices;

package Rho.Color is

   type Interpolation_Type is (Source, Destination, Linear);

   type Rho_Color is
      record
         Red, Green, Blue : Unit_Float;
         Alpha : Unit_Float;
      end record;

   function Shade
     (Color : Rho_Color;
      Factor : Rho_Float)
      return Rho_Color;

   function Interpolate
     (Left, Right : Rho_Color;
      Factor      : Unit_Float)
      return Rho_Color;

   function To_Vector_3
     (Color : Rho_Color)
      return Rho.Matrices.Vector_3
   is ((Color.Red, Color.Green, Color.Blue));

   function To_Vector_4
     (Color : Rho_Color)
      return Rho.Matrices.Vector_4
   is ((Color.Red, Color.Green, Color.Blue, Color.Alpha));

   type Rho_Color_1D_Array is array (Positive range <>) of Rho.Color.Rho_Color;

   function Scale
     (Colors        : Rho_Color_1D_Array;
      To_Size       : Positive;
      Interpolation : Interpolation_Type := Linear)
      return Rho_Color_1D_Array;

   type Rho_Color_1D_Array_Access is access Rho_Color_1D_Array;

   type Rho_Color_2D_Array is
     array (Positive range <>, Positive range <>) of Rho.Color.Rho_Color;

   type Rho_Color_2D_Array_Access is
     access all Rho_Color_2D_Array;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Rho_Color_2D_Array, Rho_Color_2D_Array_Access);

   type Rho_Has_Color is interface;

   procedure Set_Color (Item  : in out Rho_Has_Color;
                        Color : Rho_Color)
   is abstract;

   procedure Set_Color (Item  : in out Rho_Has_Color'Class;
                        Red   : Unit_Float;
                        Green : Unit_Float;
                        Blue  : Unit_Float;
                        Alpha : Unit_Float := 1.0);

   function Color (Item : Rho_Has_Color) return Rho_Color
                   is abstract;

   function Interpolate
     (Left, Right : Rho_Color;
      Factor      : Unit_Float)
      return Rho_Color
   is ((Right.Red * Factor + Left.Red * (1.0 - Factor),
        Right.Green * Factor + Left.Green * (1.0 - Factor),
        Right.Blue * Factor + Left.Blue * (1.0 - Factor),
        Right.Alpha * Factor + Left.Alpha * (1.0 - Factor)));

end Rho.Color;
