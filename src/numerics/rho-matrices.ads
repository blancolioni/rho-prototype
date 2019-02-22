private with Ada.Containers.Doubly_Linked_Lists;

with Rho.Float_Arrays;

package Rho.Matrices is

   subtype Vector is Rho.Float_Arrays.Real_Vector;
   subtype Vector_3 is Rho.Float_Arrays.Real_Vector (1 .. 3);
   subtype Vector_4 is Rho.Float_Arrays.Real_Vector (1 .. 4);

   subtype Matrix_3 is Rho.Float_Arrays.Real_Matrix (1 .. 3, 1 .. 3);

   subtype Matrix_4 is Rho.Float_Arrays.Real_Matrix (1 .. 4, 1 .. 4);

   function Identity return Matrix_4
   is (Rho.Float_Arrays.Unit_Matrix (4));

   function Rotation_Matrix
     (Angle     : Rho_Float;
      Direction : Vector_3)
      return Matrix_3;

   function Perspective_Matrix
     (Fovy         : Rho_Float;
      Aspect_Ratio : Rho_Float;
      Near, Far    : Rho_Float)
      return Matrix_4;

   function Frustum_Matrix
     (Left, Right : Rho_Float;
      Bottom, Top : Rho_Float;
      Near, Far   : Rho_Float)
      return Matrix_4;

   function Look_At_Matrix
     (From : Vector_3;
      Up   : Vector_3;
      To   : Vector_3)
      return Matrix_3;

   function Orthographic_Matrix
     (Left, Right  : Rho_Float;
      Bottom, Top  : Rho_Float;
      Near, Far    : Rho_Float)
      return Matrix_4;

   function Normalise (V : Vector) return Vector;
   function Cross (U, V : Vector_3) return Vector_3;

   type Rho_Matrix_Operation_Record is
     abstract tagged limited private;

   type Matrix_Mode_Type is
     (Model_View, Projection, Texture);

   procedure Matrix_Mode
     (State : in out Rho_Matrix_Operation_Record;
      Mode  : Matrix_Mode_Type);

   function Current_Matrix_Mode
     (State : Rho_Matrix_Operation_Record)
      return Matrix_Mode_Type;

   procedure Push_Matrix
     (State : in out Rho_Matrix_Operation_Record);

   procedure Pop_Matrix
     (State : in out Rho_Matrix_Operation_Record);

   procedure Load_Identity
     (State : in out Rho_Matrix_Operation_Record);

   procedure Multiply
     (State : in out Rho_Matrix_Operation_Record;
      Matrix : in     Rho.Matrices.Matrix_4);

   function Current
     (State : in out Rho_Matrix_Operation_Record;
      Mode  : Matrix_Mode_Type)
      return Matrix_4;

   procedure Set_Current
     (State       : in out Rho_Matrix_Operation_Record;
      Mode        : Matrix_Mode_Type;
      New_Current : Matrix_4);

   function Current
     (State : in out Rho_Matrix_Operation_Record)
      return Matrix_4
   is (State.Current (State.Current_Matrix_Mode));

   procedure Set_Current
     (State       : in out Rho_Matrix_Operation_Record;
      New_Current : Matrix_4);

   procedure Rotate
     (State : in out Rho_Matrix_Operation_Record;
      Angle    : Rho_Float;
      X, Y, Z  : Rho_Float);

   procedure Rotate
     (State : in out Rho_Matrix_Operation_Record;
      Angle  : Rho_Float;
      V      : Rho.Matrices.Vector_3);

   procedure Rotate
     (State : in out Rho_Matrix_Operation_Record;
      Matrix   : Rho.Matrices.Matrix_3);

   procedure Translate
     (State : in out Rho_Matrix_Operation_Record;
      Vector : Rho.Matrices.Vector_3);

   procedure Translate
     (State : in out Rho_Matrix_Operation_Record;
      X, Y, Z  : Rho_Float);

   procedure Scale
     (State : in out Rho_Matrix_Operation_Record;
      Vector : Rho.Matrices.Vector_3);

   procedure Scale
     (State : in out Rho_Matrix_Operation_Record;
      X, Y, Z  : Rho_Float);

private

   package List_Of_Matrices is
     new Ada.Containers.Doubly_Linked_Lists
       (Matrix_4, Rho.Float_Arrays."=");

   type Current_Matrix_Array is
     array (Matrix_Mode_Type) of aliased Matrix_4;

   type Matrix_Stack_Array is
     array (Matrix_Mode_Type) of List_Of_Matrices.List;

   type Rho_Matrix_Operation_Record is
     abstract tagged limited
      record
         Current_Matrix : Current_Matrix_Array :=
                            (others => Rho.Float_Arrays.Unit_Matrix (4));
         Matrix_Stacks  : Matrix_Stack_Array;
         Current_Mode   : Matrix_Mode_Type := Model_View;
      end record;

end Rho.Matrices;
