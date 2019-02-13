with Rho.Elementary_Functions;
with Rho.Float_Arrays;

package body Rho.Orientable is

   ---------------------
   -- Get_Orientation --
   ---------------------

   procedure Get_Orientation (Orientable : in     Rho_Orientable'Class;
                              Angle      :    out Rho_Float;
                              X, Y, Z    :    out Rho_Float)
   is
      use Rho.Matrices;
      use Rho.Elementary_Functions;
      M : constant Matrix_3 := Orientable.Orientation;
      V : constant Vector_3 := (-M (1, 3), -M (2, 3), -M (3, 3));
   begin
      X := V (1);
      Y := V (2);
      Z := V (3);
      Angle := Arccos ((M (1, 1) + M (2, 2) + M (3, 3) - 1.0) / 2.0, 360.0);
   end Get_Orientation;

   -------------
   -- Look_At --
   -------------

   procedure Look_At
     (Orientable : in out Rho_Moveable_Orientable'Class;
      Up         : in     Rho.Matrices.Vector_3;
      X, Y, Z    : in     Rho_Float)
   is
   begin
      Orientable.Look_At (Up, (X, Y, Z));
   end Look_At;

   -------------
   -- Look_At --
   -------------

   procedure Look_At
     (Orientable        : in out Rho_Moveable_Orientable'Class;
      Up_X, Up_Y, Up_Z  : in     Rho_Float;
      X, Y, Z           : in     Rho_Float)
   is
   begin
      Orientable.Look_At ((Up_X, Up_Y, Up_Z), (X, Y, Z));
   end Look_At;

   -------------
   -- Look_At --
   -------------

   procedure Look_At
     (Orientable : in out Rho_Moveable_Orientable'Class;
      Point      : Rho.Matrices.Vector_3)
   is
      use Rho.Float_Arrays;
   begin
      Orientable.Look_At (Orientable.Orientation * (0.0, 1.0, 0.0), Point);
   end Look_At;

   -------------
   -- Look_At --
   -------------

   procedure Look_At
     (Orientable        : in out Rho_Moveable_Orientable'Class;
      X, Y, Z           : in     Rho_Float)
   is
   begin
      Orientable.Look_At ((X, Y, Z));
   end Look_At;

   -------------
   -- Look_At --
   -------------

   procedure Look_At
     (Orientable : in out Rho_Moveable_Orientable'Class;
      Up         : Rho.Matrices.Vector_3;
      Point : Rho.Matrices.Vector_3)
   is
      use Rho.Matrices;
      use Rho.Float_Arrays;
      P : constant Vector_3 := Orientable.Position_3;
      Z : constant Vector_3 := Normalise (P - Point);
      X : constant Vector_3 := Normalise (Cross (Up, Z));
      Y : constant Vector_3 := Cross (Z, X);
      M : constant Matrix_3 :=
            ((X (1), Y (1), Z (1)),
             (X (2), Y (2), Z (2)),
             (X (3), Y (3), Z (3)));
   begin
      Orientable.Set_Orientation (M);
   end Look_At;

   -----------
   -- Pitch --
   -----------

   procedure Pitch (Orientable : in out Rho_Orientable'Class;
                    Angle      : Rho_Float)
   is
   begin
      Orientable.Rotate (Angle, 1.0, 0.0, 0.0);
   end Pitch;

   ----------
   -- Roll --
   ----------

   procedure Roll (Orientable : in out Rho_Orientable'Class;
                   Angle      : Rho_Float)
   is
   begin
      Orientable.Rotate (Angle, 0.0, 0.0, 1.0);
   end Roll;

   ------------
   -- Rotate --
   ------------

   procedure Rotate (Orientable : in out Rho_Orientable'Class;
                     Angle      : Rho_Float;
                     X, Y, Z    : Rho_Float)
   is
      use Rho.Float_Arrays;
   begin
      Orientable.Set_Orientation
        (Orientable.Orientation
         * Rho.Matrices.Rotation_Matrix (Angle, (X, Y, Z)));
   end Rotate;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation (Orientable : in out Rho_Orientable'Class;
                              Angle      : Rho_Float;
                              X, Y, Z    : Rho_Float)
   is
   begin
      Orientable.Set_Orientation
        (Rho.Matrices.Rotation_Matrix
           (Angle, (X, Y, Z)));
   end Set_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation (Orientable : in out Rho_Orientable'Class;
                              Angle      : Rho_Float;
                              Vector     : Rho.Matrices.Vector_3)
   is
   begin
      Orientable.Set_Orientation
        (Rho.Matrices.Rotation_Matrix
           (Angle, Vector));
   end Set_Orientation;

   -----------------------
   -- Set_Orientation_4 --
   -----------------------

   procedure Set_Orientation_4
     (Orientable : in out Rho_Orientable'Class;
      Rotation   : Rho.Matrices.Matrix_4)
   is
      M : constant Rho.Matrices.Matrix_3 :=
            ((Rotation (1, 1), Rotation (1, 2), Rotation (1, 3)),
             (Rotation (2, 1), Rotation (2, 2), Rotation (2, 3)),
             (Rotation (3, 1), Rotation (3, 2), Rotation (3, 3)));
   begin
      Orientable.Set_Orientation (M);
   end Set_Orientation_4;

   ---------
   -- Yaw --
   ---------

   procedure Yaw (Orientable : in out Rho_Orientable'Class;
                  Angle      : Rho_Float)
   is
   begin
      Orientable.Rotate (Angle, 0.0, 1.0, 0.0);
   end Yaw;

end Rho.Orientable;
