with Rho.Elementary_Functions;

package body Rho.Matrices is

   function Cross_Product_Matrix
     (V : Vector_3)
      return Matrix_3;

   function Tensor_Product
     (X, Y : Vector_3)
      return Matrix_3;

   -----------
   -- Cross --
   -----------

   function Cross (U, V : Vector_3) return Vector_3 is
   begin
      return (U (2) * V (3) - U (3) * V (2),
              U (3) * V (1) - U (1) * V (3),
              U (1) * V (2) - U (2) * V (1));
   end Cross;

   --------------------------
   -- Cross_Product_Matrix --
   --------------------------

   function Cross_Product_Matrix
     (V : Vector_3)
      return Matrix_3
   is
      Result : Matrix_3 := (others => (others => 0.0));
   begin
      Result (1, 2) := -V (3);
      Result (1, 3) := V (2);
      Result (2, 1) := V (3);
      Result (2, 3) := -V (1);
      Result (3, 1) := -V (2);
      Result (3, 2) := V (1);
      return Result;
   end Cross_Product_Matrix;

   -------------
   -- Current --
   -------------

   function Current
     (State : in out Rho_Matrix_Operation_Record;
      Mode  : Matrix_Mode_Type)
      return Matrix_4
   is
   begin
      return State.Current_Matrix (Mode);
   end Current;

   -------------------------
   -- Current_Matrix_Mode --
   -------------------------

   function Current_Matrix_Mode
     (State : Rho_Matrix_Operation_Record)
      return Matrix_Mode_Type
   is
   begin
      return State.Current_Mode;
   end Current_Matrix_Mode;

   -------------
   -- Frustum --
   -------------

   procedure Frustum
     (Target      : in out Rho_Matrix_Operation_Record;
      Left, Right : Rho_Float;
      Bottom, Top : Rho_Float;
      Near, Far   : Rho_Float)
   is
      A : constant Rho_Float := (Right + Left) / (Right - Left);
      B : constant Rho_Float := (Top + Bottom) / (Top - Bottom);
      C : constant Rho_Float := (Far + Near) / (Far - Near);
      D : constant Rho_Float := 2.0 * Far * Near / (Far - Near);
   begin
      Target.Multiply
        (((2.0 * Near / (Right - Left), 0.0, A, 0.0),
         (0.0, 2.0 * Near / (Top - Bottom), B, 0.0),
         (0.0, 0.0, C, D),
         (0.0, 0.0, -1.0, 0.0)));
   end Frustum;

   --------------------
   -- Frustum_Matrix --
   --------------------

   function Frustum_Matrix
     (Left, Right : Rho_Float;
      Bottom, Top : Rho_Float;
      Near, Far   : Rho_Float)
      return Matrix_4
   is
      A : constant Rho_Float := (Right + Left) / (Right - Left);
      B : constant Rho_Float := (Top + Bottom) / (Top - Bottom);
      C : constant Rho_Float := (Far + Near) / (Far - Near);
      D : constant Rho_Float := 2.0 * Far * Near / (Far - Near);
   begin
      return ((2.0 * Near / (Right - Left), 0.0, A, 0.0),
              (0.0, 2.0 * Near / (Top - Bottom), B, 0.0),
              (0.0, 0.0, C, D),
              (0.0, 0.0, -1.0, 0.0));
   end Frustum_Matrix;

   -------------------
   -- Load_Identity --
   -------------------

   procedure Load_Identity
     (State : in out Rho_Matrix_Operation_Record)
   is
   begin
      State.Current_Matrix (State.Current_Mode) := Identity;
   end Load_Identity;

   --------------------
   -- Look_At_Matrix --
   --------------------

   function Look_At_Matrix
     (From : Vector_3;
      Up   : Vector_3;
      To   : Vector_3)
      return Matrix_3
   is
      use Rho.Float_Arrays;
      Z : constant Vector_3 := Normalise (From - To);
      X : constant Vector_3 := Normalise (Cross (Up, Z));
      Y : constant Vector_3 := Cross (Z, X);
      M : constant Matrix_3 :=
            ((X (1), Y (1), Z (1)),
             (X (2), Y (2), Z (2)),
             (X (3), Y (3), Z (3)));
   begin
      return M;
   end Look_At_Matrix;

   -----------------
   -- Matrix_Mode --
   -----------------

   procedure Matrix_Mode
     (State : in out Rho_Matrix_Operation_Record;
      Mode  : Matrix_Mode_Type)
   is
   begin
      State.Current_Mode := Mode;
   end Matrix_Mode;

   --------------
   -- Multiply --
   --------------

   procedure Multiply
     (State : in out Rho_Matrix_Operation_Record;
      Matrix : in     Rho.Matrices.Matrix_4)
   is
      use Rho.Float_Arrays;
   begin
      State.Current_Matrix (State.Current_Mode) :=
        State.Current_Matrix (State.Current_Mode) * Matrix;
   end Multiply;

   ---------------
   -- Normalise --
   ---------------

   function Normalise (V : Vector) return Vector is
      use Rho.Float_Arrays;
   begin
      return V / abs V;
   end Normalise;

   -----------
   -- Ortho --
   -----------

   procedure Ortho
     (Target      : in out Rho_Matrix_Operation_Record;
      Left, Right : Rho_Float;
      Bottom, Top : Rho_Float;
      Near, Far   : Rho_Float)
   is
      TX : constant Rho_Float :=
             (Right + Left) / (Right - Left);
      TY : constant Rho_Float :=
             (Top + Bottom) / (Top - Bottom);
      TZ : constant Rho_Float :=
             (Far + Near) / (Far - Near);
   begin
      Target.Multiply
        (((2.0 / (Right - Left), 0.0, 0.0, -TX),
         (0.0, 2.0 / (Top - Bottom), 0.0, -TY),
         (0.0, 0.0, -2.0 / (Far - Near), -TZ),
         (0.0, 0.0, 0.0, 1.0)));
   end Ortho;

   -------------------------
   -- Orthographic_Matrix --
   -------------------------

   function Orthographic_Matrix
     (Left, Right  : Rho_Float;
      Bottom, Top  : Rho_Float;
      Near, Far    : Rho_Float)
      return Matrix_4
   is
      TX : constant Rho_Float :=
             (Right + Left) / (Right - Left);
      TY : constant Rho_Float :=
             (Top + Bottom) / (Top - Bottom);
      TZ : constant Rho_Float :=
             (Far + Near) / (Far - Near);
   begin
      return
        (((2.0 / (Right - Left), 0.0, 0.0, -TX),
         (0.0, 2.0 / (Top - Bottom), 0.0, -TY),
         (0.0, 0.0, -2.0 / (Far - Near), -TZ),
         (0.0, 0.0, 0.0, 1.0)));
   end Orthographic_Matrix;

   ------------------------
   -- Perspective_Matrix --
   ------------------------

   function Perspective_Matrix
     (Fovy         : Rho_Float;
      Aspect_Ratio : Rho_Float;
      Near, Far    : Rho_Float)
      return Matrix_4
   is
      use Rho.Elementary_Functions;
      F     : constant Rho_Float := Tan (Fovy / 2.0, 360.0);
      Y_Max : constant Rho_Float := Near * F;
      X_Max : constant Rho_Float := Y_Max * Aspect_Ratio;
   begin
      return Frustum_Matrix (-X_Max, X_Max, -Y_Max, Y_Max, Near, Far);
   end Perspective_Matrix;

   ----------------
   -- Pop_Matrix --
   ----------------

   procedure Pop_Matrix
     (State : in out Rho_Matrix_Operation_Record)
   is
   begin
      State.Current_Matrix (State.Current_Mode) :=
        State.Matrix_Stacks (State.Current_Mode).Last_Element;
      State.Matrix_Stacks (State.Current_Mode).Delete_Last;
   end Pop_Matrix;

   -----------------
   -- Push_Matrix --
   -----------------

   procedure Push_Matrix
     (State : in out Rho_Matrix_Operation_Record)
   is
   begin
      State.Matrix_Stacks (State.Current_Mode).Append
        (State.Current_Matrix (State.Current_Mode));
   end Push_Matrix;

   ------------
   -- Rotate --
   ------------

   procedure Rotate
     (State : in out Rho_Matrix_Operation_Record;
      Angle    : Rho_Float;
      X, Y, Z  : Rho_Float)
   is
   begin
      State.Rotate (Angle, (X, Y, Z));
   end Rotate;

   ------------
   -- Rotate --
   ------------

   procedure Rotate
     (State : in out Rho_Matrix_Operation_Record;
      Angle : Rho_Float;
      V     : Vector_3)
   is
   begin
      State.Rotate (Rotation_Matrix (Angle, V));
   end Rotate;

   ------------
   -- Rotate --
   ------------

   procedure Rotate
     (State : in out Rho_Matrix_Operation_Record;
      Matrix   : Matrix_3)
   is
      R : Matrix_4 := Rho.Float_Arrays.Unit_Matrix (4);
   begin
      for I in 1 .. 3 loop
         for J in 1 .. 3 loop
            R (I, J) := Matrix (I, J);
         end loop;
      end loop;
      State.Multiply (R);
   end Rotate;

   ---------------------
   -- Rotation_Matrix --
   ---------------------

   function Rotation_Matrix
     (Angle     : Rho_Float;
      Direction : Vector_3)
      return Matrix_3
   is
      use Rho.Elementary_Functions;
      use Rho.Float_Arrays;
      Cos_Theta : constant Rho_Float := Cos (Angle, 360.0);
      Sin_Theta : constant Rho_Float := Sin (Angle, 360.0);
      Cross     : constant Matrix_3 := Cross_Product_Matrix (Direction);
      Tensor    : constant Matrix_3 := Tensor_Product (Direction, Direction);
   begin
      return Unit_Matrix (3) * Cos_Theta
        + Sin_Theta * Cross
        + (1.0 - Cos_Theta) * Tensor;
   end Rotation_Matrix;

   -----------
   -- Scale --
   -----------

   procedure Scale
     (State : in out Rho_Matrix_Operation_Record;
      X, Y, Z : Rho_Float)
   is
   begin
      State.Scale ((X, Y, Z));
   end Scale;

   -----------
   -- Scale --
   -----------

   procedure Scale
     (State : in out Rho_Matrix_Operation_Record;
      Vector : Rho.Matrices.Vector_3)
   is
      M : Matrix_4 := Identity;
   begin
      for I in 1 .. 3 loop
         M (I, I) := Vector (I);
      end loop;
      State.Multiply (M);
   end Scale;

   -----------------
   -- Set_Current --
   -----------------

   procedure Set_Current
     (State       : in out Rho_Matrix_Operation_Record;
      Mode        : Matrix_Mode_Type;
      New_Current : Matrix_4)
   is
   begin
      State.Current_Matrix (Mode) := New_Current;
   end Set_Current;

   -----------------
   -- Set_Current --
   -----------------

   procedure Set_Current
     (State       : in out Rho_Matrix_Operation_Record;
      New_Current : Matrix_4)
   is
   begin
      State.Set_Current (State.Current_Matrix_Mode, New_Current);
   end Set_Current;

   --------------------
   -- Tensor_Product --
   --------------------

   function Tensor_Product
     (X, Y : Vector_3)
      return Matrix_3
   is
      Result : Matrix_3;
   begin
      for I in 1 .. 3 loop
         for J in 1 .. 3 loop
            Result (I, J) := X (I) * Y (J);
         end loop;
      end loop;
      return Result;
   end Tensor_Product;

   ---------------
   -- Translate --
   ---------------

   procedure Translate
     (State : in out Rho_Matrix_Operation_Record;
      X, Y, Z : Rho_Float)
   is
   begin
      State.Translate ((X, Y, Z));
   end Translate;

   ---------------
   -- Translate --
   ---------------

   procedure Translate
     (State : in out Rho_Matrix_Operation_Record;
      Vector : Vector_3)
   is
      M : Matrix_4 := Rho.Float_Arrays.Unit_Matrix (4);
   begin
      for I in 1 .. 3 loop
         M (I, 4) := Vector (I);
      end loop;
      State.Multiply (M);
   end Translate;

end Rho.Matrices;
