limited with Rho.Context;

with Rho.Color;
with Rho.Entity;

package Rho.Shapes is

   function Cube
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Size    : Rho_Float := 1.0)
      return Rho.Entity.Rho_Entity;

   function Icosohedral_Sphere
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Detail  : Natural)
      return Rho.Entity.Rho_Entity;

   function Icosohedral_Sphere
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Detail  : Natural;
      Color  : not null access
        function (X, Y, Z : Signed_Unit_Float)
        return Rho.Color.Rho_Color)
      return Rho.Entity.Rho_Entity;

   function Quadric_Surface
     (Context    : not null access Rho.Context.Rho_Context_Record'Class;
      Slices     : Positive;
      Stacks     : Positive;
      Z_Min      : Signed_Unit_Float := -1.0;
      Z_Max      : Signed_Unit_Float := 1.0;
      Radius_Fn  : not null access
        function (Z : Signed_Unit_Float) return Unit_Float;
      Z_Fn       : not null access
        function (Z : Signed_Unit_Float) return Signed_Unit_Float)
     return Rho.Entity.Rho_Entity;

   function Quadric_Sphere
     (Context    : not null access Rho.Context.Rho_Context_Record'Class;
      Slices     : Positive;
      Stacks     : Positive)
      return Rho.Entity.Rho_Entity;

   function Quadric_Cylinder
     (Context    : not null access Rho.Context.Rho_Context_Record'Class;
      Slices     : Positive;
      Stacks     : Positive)
      return Rho.Entity.Rho_Entity;

   function Quadric_Cone
     (Context    : not null access Rho.Context.Rho_Context_Record'Class;
      Slices     : Positive;
      Stacks     : Positive)
      return Rho.Entity.Rho_Entity;

   function Quadric_Conical_Frustum
     (Context      : not null access Rho.Context.Rho_Context_Record'Class;
      Slices       : Positive;
      Stacks       : Positive;
      Radius_Ratio : Unit_Float)
      return Rho.Entity.Rho_Entity;

   function Square
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Size    : Rho_Float)
      return Rho.Entity.Rho_Entity;

end Rho.Shapes;
