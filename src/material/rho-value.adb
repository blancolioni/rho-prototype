with Rho.Float_Arrays;
with Rho.Float_Images;

with GL, GL_Types;

package body Rho.Value is

   -----------------
   -- Color_Value --
   -----------------

   function Color_Value
     (Color : Rho.Color.Rho_Color)
      return Rho_Value
   is
   begin
      return new Rho_Value_Record'
        (Color_Value, Color_Val => Color);
   end Color_Value;

   -----------------
   -- Color_Value --
   -----------------

   function Color_Value
     (Red, Green, Blue : Unit_Float;
      Alpha            : Unit_Float := 1.0)
      return Rho_Value
   is
   begin
      return Color_Value ((Red, Green, Blue, Alpha));
   end Color_Value;

   -------------------
   -- Default_Value --
   -------------------

   function Default_Value
     (Of_Type : Value_Type)
      return Rho_Value
   is
   begin
      return Value : constant Rho_Value := new Rho_Value_Record (Of_Type) do
         case Of_Type is
            when String_Value =>
               Value.String_Val := Ada.Strings.Unbounded.Null_Unbounded_String;
            when Identifier_Value =>
               Value.String_Val := Ada.Strings.Unbounded.Null_Unbounded_String;
            when Float_Value =>
               Value.Float_Val := 0.0;
            when Vector_2_Value =>
               Value.Vector_2_Val := (others => 0.0);
            when Vector_3_Value =>
               Value.Vector_3_Val := (others => 0.0);
            when Vector_4_Value =>
               Value.Vector_4_Val := (others => 0.0);
            when Matrix_Value =>
               Value.Matrix_Val :=
                 Rho.Float_Arrays.Unit_Matrix (4);
            when Color_Value =>
               Value.Color_Val := (0.0, 0.0, 0.0, 1.0);
         end case;
      end return;
   end Default_Value;

   -----------------
   -- Float_Value --
   -----------------

   function Float_Value
     (Value : Rho_Float)
      return Rho_Value
   is
   begin
      return new Rho_Value_Record'
        (Float_Value, Value);
   end Float_Value;

   ----------------------
   -- Identifier_Value --
   ----------------------

   function Identifier_Value
     (Name : String)
      return Rho_Value
   is
   begin
      return new Rho_Value_Record'
        (Identifier_Value, Ada.Strings.Unbounded.To_Unbounded_String (Name));
   end Identifier_Value;

   ------------
   -- Rho_New --
   ------------

   procedure Rho_New
     (Value   : in out Rho_Value;
      Of_Type : Value_Type)
   is
   begin
      Value := Default_Value (Of_Type);
   end Rho_New;

   -----------------------
   -- Set_Uniform_Value --
   -----------------------

   procedure Set_Uniform_Value
     (Value    : Rho_Value_Record'Class;
      Location : Uniform_Location_Type)
   is
      GL_Loc : constant GL_Types.Uint :=
                 GL_Types.Uint (Location);
   begin
      case Value.Of_Type is
         when String_Value =>
            raise Constraint_Error with
              "cannot set string uniform";
         when Identifier_Value =>
            raise Constraint_Error with
              "cannot set identifier uniform";
         when Float_Value =>
            GL.Uniform (GL_Loc, (1 => GL_Types.GLfloat (Value.Float_Val)));
         when Vector_2_Value =>
            GL.Uniform (GL_Loc,
                        (GL_Types.GLfloat (Value.Vector_2_Val (1)),
                         GL_Types.GLfloat (Value.Vector_2_Val (2))));
         when Vector_3_Value =>
            GL.Uniform (GL_Loc,
                        (GL_Types.GLfloat (Value.Vector_3_Val (1)),
                         GL_Types.GLfloat (Value.Vector_3_Val (2)),
                         GL_Types.GLfloat (Value.Vector_3_Val (3))));
         when Vector_4_Value =>
            GL.Uniform (GL_Loc,
                        (GL_Types.GLfloat (Value.Vector_4_Val (1)),
                         GL_Types.GLfloat (Value.Vector_4_Val (2)),
                         GL_Types.GLfloat (Value.Vector_4_Val (3)),
                         GL_Types.GLfloat (Value.Vector_4_Val (3))));
         when Matrix_Value =>
            null;
         when Color_Value =>
            GL.Uniform (GL_Loc,
                        (GL_Types.GLfloat (Value.Color_Val.Red),
                         GL_Types.GLfloat (Value.Color_Val.Green),
                         GL_Types.GLfloat (Value.Color_Val.Blue),
                         GL_Types.GLfloat (Value.Color_Val.Alpha)));
      end case;
   end Set_Uniform_Value;

   ---------------------
   -- Shader_Constant --
   ---------------------

   function Shader_Constant
     (Value : Rho_Value_Record'Class)
      return String
   is
      use Rho.Float_Images;
   begin
      case Value.Of_Type is
         when String_Value =>
            return "error - string type cannot be used in shader";
         when Identifier_Value =>
            return Ada.Strings.Unbounded.To_String (Value.Identifier_Val);
         when Float_Value =>
            return Image (Value.Float_Val);
         when Vector_2_Value =>
            return "vec2("
              & Image (Value.Vector_2_Val (1))
              & ","
              & Image (Value.Vector_2_Val (2))
              & ")";
         when Vector_3_Value =>
            return "vec3("
              & Image (Value.Vector_3_Val (1))
              & ","
              & Image (Value.Vector_3_Val (2))
              & ","
              & Image (Value.Vector_3_Val (3))
              & ")";
         when Vector_4_Value =>
            return "vec4("
              & Image (Value.Vector_4_Val (1))
              & ","
              & Image (Value.Vector_4_Val (2))
              & ","
              & Image (Value.Vector_4_Val (3))
              & ","
              & Image (Value.Vector_4_Val (4))
              & ")";
         when Matrix_Value =>
            return "mat4";
         when Color_Value =>
            return "vec4("
              & Image (Value.Color_Val.Red)
              & ","
              & Image (Value.Color_Val.Green)
              & ","
              & Image (Value.Color_Val.Blue)
              & ","
              & Image (Value.Color_Val.Alpha)
              & ")";
      end case;
   end Shader_Constant;

   ----------------------
   -- Shader_Type_Name --
   ----------------------

   function Shader_Type_Name
     (Value : Rho_Value_Record'Class)
      return String
   is
   begin
      case Value.Of_Type is
         when String_Value =>
            return "error - string type cannot be used in shader";
         when Identifier_Value =>
            return "error - cannot decude identifier type";
         when Float_Value =>
            return "float";
         when Vector_2_Value =>
            return "vec2";
         when Vector_3_Value =>
            return "vec3";
         when Vector_4_Value =>
            return "vec4";
         when Matrix_Value =>
            return "mat4";
         when Color_Value =>
            return "vec4";
      end case;
   end Shader_Type_Name;

   ------------------
   -- Vector_Value --
   ------------------

   function Vector_Value
     (Value : Rho.Matrices.Vector)
      return Rho_Value
   is
   begin
      if Value'Length = 1 then
         return Float_Value (Value (Value'First));
      elsif Value'Length = 2 then
         return new Rho_Value_Record'(Vector_2_Value, Value);
      elsif Value'Length = 3 then
         return new Rho_Value_Record'(Vector_3_Value, Value);
      elsif Value'Length = 4 then
         return new Rho_Value_Record'(Vector_4_Value, Value);
      else
         raise Constraint_Error with "invalid vector length:"
           & Natural'Image (Value'Length);
      end if;
   end Vector_Value;

end Rho.Value;
