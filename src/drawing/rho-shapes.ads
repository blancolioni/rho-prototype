with Rho.Color;
with Rho.Entity;

package Rho.Shapes is

   function Cube
     (Size : Rho_Float := 1.0)
      return Rho.Entity.Rho_Entity;

   function Icosohedral_Sphere
     (Detail  : Natural)
      return Rho.Entity.Rho_Entity;

   function Icosohedral_Sphere
     (Detail  : Natural;
      Color  : not null access
        function (X, Y, Z : Signed_Unit_Float)
        return Rho.Color.Rho_Color)
      return Rho.Entity.Rho_Entity;

   function Quadric_Surface
     (Slices     : Positive;
      Stacks     : Positive;
      Z_Min      : Signed_Unit_Float := -1.0;
      Z_Max      : Signed_Unit_Float := 1.0;
      Radius_Fn  : not null access
        function (Z : Signed_Unit_Float) return Unit_Float;
      Z_Fn       : not null access
        function (Z : Signed_Unit_Float) return Signed_Unit_Float)
     return Rho.Entity.Rho_Entity;

   function Quadric_Sphere
     (Slices     : Positive;
      Stacks     : Positive)
      return Rho.Entity.Rho_Entity;

   function Quadric_Cylinder
     (Slices     : Positive;
      Stacks     : Positive)
      return Rho.Entity.Rho_Entity;

   function Quadric_Cone
     (Slices     : Positive;
      Stacks     : Positive)
      return Rho.Entity.Rho_Entity;

   function Quadric_Conical_Frustum
     (Slices       : Positive;
      Stacks       : Positive;
      Radius_Ratio : Unit_Float)
      return Rho.Entity.Rho_Entity;

   function Square
     (Size : Rho_Float)
      return Rho.Entity.Rho_Entity;

end Rho.Shapes;
