with Rho.Float_Arrays;
with Rho.Float_Buffer;
with Rho.Value;

package Rho.Shaders.Values is

   type Rho_Uniform_Value_Record is new Rho_Shader_Value_Record with private;

   type Rho_Uniform_Value is access all Rho_Uniform_Value_Record'Class;

   type Uniform_Integer_Array is
     array (Positive range <>) of Integer;

   type Uniform_Float_Array is
     array (Positive range <>) of Rho_Float;

   type Uniform_Vector_3_Array is
     array (Positive range <>) of Rho.Matrices.Vector_3;

   procedure Set_Value
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Rho.Value.Rho_Value);

   procedure Set_Value
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Integer);

   procedure Set_Value
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Rho_Float);

   procedure Set_Value
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Rho.Matrices.Matrix_4);

   procedure Set_Value
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Rho.Float_Arrays.Real_Vector);

   procedure Set_Array
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Uniform_Float_Array);

   procedure Set_Array
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Uniform_Vector_3_Array);

   procedure Set_Array
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Uniform_Integer_Array);

   function New_Uniform_Value
     (Shader : Rho_Shader;
      Id     : Rho_Uniform_Id;
      Name   : String)
      return Rho_Uniform_Value;

   type Rho_Attribute_Value_Record is new Rho_Shader_Value_Record with private;

   type Rho_Attribute_Value is access all Rho_Attribute_Value_Record'Class;

   procedure Bind_Vertex_Buffer
     (Attribute      : Rho_Attribute_Value_Record;
      Buffer         : Rho.Float_Buffer.Rho_Float_Buffer;
      Start          : Positive;
      Component_Size : Positive);

   function New_Attribute_Value
     (Shader : Rho_Shader;
      Id     : Rho_Attribute_Id;
      Name   : String)
      return Rho_Attribute_Value;

private

   type Rho_Uniform_Value_Record is
     new Rho_Shader_Value_Record with null record;

   type Rho_Attribute_Value_Record is
     new Rho_Shader_Value_Record with null record;

end Rho.Shaders.Values;
