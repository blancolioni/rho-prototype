with Rho.Context;
with Rho.Rendering;

package body Rho.Shaders.Values is

   ------------------------
   -- Bind_Vertex_Buffer --
   ------------------------

   procedure Bind_Vertex_Buffer
     (Attribute      : Rho_Attribute_Value_Record;
      Buffer         : Rho.Float_Buffer.Rho_Float_Buffer;
      Start          : Positive;
      Component_Size : Positive)
   is
   begin
      Attribute.Context.Renderer.Bind_Vertex_Buffer
        (Attribute.Attribute_Location, Buffer, Start, Component_Size);
   end Bind_Vertex_Buffer;

   ---------------
   -- Set_Array --
   ---------------

   procedure Set_Array
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Uniform_Float_Array)
   is
      Vector : Rho.Float_Arrays.Real_Vector (Value'Range);
   begin
      for I in Vector'Range loop
         Vector (I) := Value (I);
      end loop;
      Uniform.Set_Value (Vector);
   end Set_Array;

   ---------------
   -- Set_Array --
   ---------------

   procedure Set_Array
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Uniform_Vector_3_Array)
   is
      Vector : Rho.Float_Arrays.Real_Vector (1 .. Value'Length * 3);
   begin
      for I in Vector'Range loop
         Vector (I) := Rho_Float (Value (I / 3 + 1) (I mod 3 + 1));
      end loop;
      Uniform.Context.Renderer.Set_Uniform_Vector_Array
        (Uniform.Uniform_Location, 3, Vector);
   end Set_Array;

   ---------------
   -- Set_Array --
   ---------------

   procedure Set_Array
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Uniform_Integer_Array)
   is
      Vector : Rho.Rendering.Integer_Array (Value'Range);
   begin
      for I in Vector'Range loop
         Vector (I) := Value (I);
      end loop;
      Uniform.Context.Renderer.Set_Uniform_Value
        (Uniform.Uniform_Location, Vector);
   end Set_Array;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Rho.Value.Rho_Value)
   is
   begin
      Value.Set_Uniform_Value (Uniform.Uniform_Location);
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Uniform      : Rho_Uniform_Value_Record;
      Value        : Integer)
   is
   begin
      Uniform.Context.Renderer.Set_Uniform_Value
        (Uniform.Uniform_Location, Value);
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Uniform      : Rho_Uniform_Value_Record;
      Value        : Rho.Matrices.Matrix_4)
   is
   begin
      Uniform.Context.Renderer.Set_Uniform_Value
        (Uniform.Uniform_Location, Value);
--
--        GL_Mat : GL_Types.Float_Matrix_4x4;
--     begin
--
--        for I in Value'Range (1) loop
--           for J in Value'Range (2) loop
--              GL_Mat (I + (J - 1) * 4) := GLfloat (Value (I, J));
--           end loop;
--        end loop;
--
--        GL.Uniform_Matrix
--          (Location  => Uint (Uniform.Location),
--           Count     => 1,
--           Transpose => GL_FALSE,
--           Matrix    => GL_Mat);
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Rho.Float_Arrays.Real_Vector)
   is
   begin
      Uniform.Context.Renderer.Set_Uniform_Value
        (Uniform.Uniform_Location, Value);
--        GL_Value : Array_Of_Float (Value'Range);
--     begin
--        for I in Value'Range loop
--           GL_Value (I) := GLfloat (Value (I));
--        end loop;
--        GL.Uniform
--          (Location => Uint (Uniform.Location),
--           Value    => GL_Value);
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Rho_Float)
   is
   begin
      Uniform.Context.Renderer.Set_Uniform_Value
        (Uniform.Uniform_Location, Value);
   end Set_Value;

end Rho.Shaders.Values;
