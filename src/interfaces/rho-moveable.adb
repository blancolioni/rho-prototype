with Rho.Float_Arrays;

package body Rho.Moveable is

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position (Moveable : in out Rho_Moveable'Class;
                           X, Y, Z  : Rho_Float)
   is
   begin
      Moveable.Set_Position (Rho.Matrices.Vector_4'(X, Y, Z, 1.0));
   end Set_Position;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position (Moveable      : in out Rho_Moveable'Class;
                           From_Moveable : Rho_Moveable'Class)
   is
   begin
      Moveable.Set_Position (From_Moveable.Position);
   end Set_Position;

   -----------
   -- Set_X --
   -----------

   procedure Set_X (Moveable : in out Rho_Moveable'Class;
                    New_X    : Rho_Float)
   is
      Position : Rho.Matrices.Vector_4 := Moveable.Position;
   begin
      Position (1) := New_X;
      Moveable.Set_Position (Position);
   end Set_X;

   -----------
   -- Set_Y --
   -----------

   procedure Set_Y (Moveable : in out Rho_Moveable'Class;
                    New_Y    : Rho_Float)
   is
      Position : Rho.Matrices.Vector_4 := Moveable.Position;
   begin
      Position (2) := New_Y;
      Moveable.Set_Position (Position);
   end Set_Y;

   -----------
   -- Set_Z --
   -----------

   procedure Set_Z (Moveable : in out Rho_Moveable'Class;
                    New_Z    : Rho_Float)
   is
      Position : Rho.Matrices.Vector_4 := Moveable.Position;
   begin
      Position (3) := New_Z;
      Moveable.Set_Position (Position);
   end Set_Z;

   ---------------
   -- Translate --
   ---------------

   procedure Translate (Moveable   : in out Rho_Moveable'Class;
                        DX, DY, DZ : in     Rho_Float)
   is
   begin
      Moveable.Translate ((DX, DY, DZ));
   end Translate;

   ---------------
   -- Translate --
   ---------------

   procedure Translate (Moveable   : in out Rho_Moveable'Class;
                        DV         : Rho.Matrices.Vector_3)
   is
      use Rho.Float_Arrays;
   begin
      Moveable.Set_Position (Moveable.Position + (DV & 0.0));
   end Translate;

   -------
   -- X --
   -------

   function X (Moveable : Rho_Moveable'Class) return Rho_Float is
      V : constant Rho.Matrices.Vector_4 := Moveable.Position;
   begin
      return V (1);
   end X;

   -------
   -- Y --
   -------

   function Y (Moveable : Rho_Moveable'Class) return Rho_Float is
      V : constant Rho.Matrices.Vector_4 := Moveable.Position;
   begin
      return V (2);
   end Y;

   -------
   -- Z --
   -------

   function Z (Moveable : Rho_Moveable'Class) return Rho_Float is
      V : constant Rho.Matrices.Vector_4 := Moveable.Position;
   begin
      return V (3);
   end Z;

end Rho.Moveable;
