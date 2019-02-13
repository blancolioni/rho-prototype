with Rho.Matrices;

package Rho.Moveable is

   type Rho_Moveable is interface;

   procedure Set_Position (Moveable : in out Rho_Moveable;
                           Position : in Rho.Matrices.Vector)
   is abstract;

   function Position
     (Moveable : in     Rho_Moveable)
      return Rho.Matrices.Vector_4
      is abstract;

   function Position_3
     (Moveable : in     Rho_Moveable'Class)
      return Rho.Matrices.Vector_3
   is (Moveable.Position (1 .. 3));

   function X (Moveable : Rho_Moveable'Class) return Rho_Float;
   function Y (Moveable : Rho_Moveable'Class) return Rho_Float;
   function Z (Moveable : Rho_Moveable'Class) return Rho_Float;

   procedure Set_X (Moveable : in out Rho_Moveable'Class;
                    New_X    : Rho_Float);
   procedure Set_Y (Moveable : in out Rho_Moveable'Class;
                    New_Y    : Rho_Float);
   procedure Set_Z (Moveable : in out Rho_Moveable'Class;
                    New_Z    : Rho_Float);

   procedure Translate (Moveable   : in out Rho_Moveable'Class;
                        DV         : Rho.Matrices.Vector_3);

   procedure Translate (Moveable   : in out Rho_Moveable'Class;
                        DX, DY, DZ : in     Rho_Float);

   procedure Set_Position (Moveable      : in out Rho_Moveable'Class;
                           From_Moveable : Rho_Moveable'Class);

   procedure Set_Position (Moveable : in out Rho_Moveable'Class;
                           X, Y, Z  : Rho_Float);

end Rho.Moveable;
