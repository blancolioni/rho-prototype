with Rho.Matrices;
with Rho.Moveable;

package Rho.Orientable is

   type Rho_Orientable is interface;

   procedure Set_Orientation (Orientable : in out Rho_Orientable;
                              Rotation   : Rho.Matrices.Matrix_3)
   is abstract;

   function Orientation (Orientable : in     Rho_Orientable)
                         return Rho.Matrices.Matrix_3
                         is abstract;

   procedure Set_Orientation (Orientable : in out Rho_Orientable'Class;
                              Angle      : Rho_Float;
                              X, Y, Z    : Rho_Float);

   procedure Set_Orientation (Orientable : in out Rho_Orientable'Class;
                              Angle      : Rho_Float;
                              Vector     : Rho.Matrices.Vector_3);

   procedure Set_Orientation_4
     (Orientable : in out Rho_Orientable'Class;
      Rotation   : Rho.Matrices.Matrix_4);

   procedure Get_Orientation (Orientable : in     Rho_Orientable'Class;
                              Angle      :    out Rho_Float;
                              X, Y, Z    :    out Rho_Float);

   procedure Rotate
     (Orientable : in out Rho_Orientable'Class;
      Angle      : Rho_Float;
      X, Y, Z    : Rho_Float);

   procedure Roll
     (Orientable : in out Rho_Orientable'Class;
      Angle      : Rho_Float);

   procedure Pitch
     (Orientable : in out Rho_Orientable'Class;
      Angle      : Rho_Float);

   procedure Yaw
     (Orientable : in out Rho_Orientable'Class;
      Angle      : Rho_Float);

   type Rho_Moveable_Orientable is interface
     and Rho_Orientable
     and Rho.Moveable.Rho_Moveable;

   procedure Look_At
     (Orientable : in out Rho_Moveable_Orientable'Class;
      Up         : Rho.Matrices.Vector_3;
      Point      : Rho.Matrices.Vector_3);

   procedure Look_At
     (Orientable : in out Rho_Moveable_Orientable'Class;
      Up         : in     Rho.Matrices.Vector_3;
      X, Y, Z    : in     Rho_Float);

   procedure Look_At
     (Orientable        : in out Rho_Moveable_Orientable'Class;
      Up_X, Up_Y, Up_Z  : in     Rho_Float;
      X, Y, Z           : in     Rho_Float);

   procedure Look_At
     (Orientable : in out Rho_Moveable_Orientable'Class;
      Point      : Rho.Matrices.Vector_3);

   procedure Look_At
     (Orientable : in out Rho_Moveable_Orientable'Class;
      X, Y, Z    : in     Rho_Float);

end Rho.Orientable;
