private with Ada.Strings.Unbounded;

with Rho.Color;
with Rho.Matrices;

package Rho.Value is

   type Value_Type is
     (String_Value,
      Identifier_Value,
      Float_Value,
      Vector_2_Value,
      Vector_3_Value,
      Vector_4_Value,
      Matrix_Value,
      Color_Value);

   type Rho_Value_Record (Of_Type : Value_Type) is tagged private;

   type Rho_Value is access all Rho_Value_Record'Class;

   procedure Rho_New
     (Value   : in out Rho_Value;
      Of_Type : Value_Type);

   function Shader_Type_Name
     (Value : Rho_Value_Record'Class)
      return String;

   function Shader_Constant
     (Value : Rho_Value_Record'Class)
      return String;

   procedure Set_Uniform_Value
     (Value    : Rho_Value_Record'Class;
      Location : Uniform_Location_Type);

   function Default_Value
     (Of_Type : Value_Type)
      return Rho_Value;

   function Float_Value
     (Value : Rho_Float)
      return Rho_Value;

   function Vector_Value
     (Value : Rho.Matrices.Vector)
      return Rho_Value;

   function Identifier_Value
     (Name : String)
      return Rho_Value;

   function Color_Value
     (Color : Rho.Color.Rho_Color)
      return Rho_Value;

   function Color_Value
     (Red, Green, Blue : Unit_Float;
      Alpha            : Unit_Float := 1.0)
      return Rho_Value;

private

   type Rho_Value_Record (Of_Type : Value_Type) is tagged
      record
         case Of_Type is
            when String_Value =>
               String_Val : Ada.Strings.Unbounded.Unbounded_String;
            when Identifier_Value =>
               Identifier_Val : Ada.Strings.Unbounded.Unbounded_String;
            when Float_Value =>
               Float_Val  : Rho_Float;
            when Vector_2_Value =>
               Vector_2_Val : Rho.Matrices.Vector (1 .. 2);
            when Vector_3_Value =>
               Vector_3_Val : Rho.Matrices.Vector (1 .. 3);
            when Vector_4_Value =>
               Vector_4_Val : Rho.Matrices.Vector (1 .. 4);
            when Matrix_Value =>
               Matrix_Val   : Rho.Matrices.Matrix_4;
            when Color_Value =>
               Color_Val    : Rho.Color.Rho_Color;
         end case;
      end record;

end Rho.Value;
