with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

with Interfaces.C.Strings;

with GL_Constants;
with GL;

package body Rho.Shader is

   use GL, GL_Types, GL_Constants;

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   --------------
   -- Activate --
   --------------

   procedure Activate (Program : Rho_Shader_Record'Class) is
   begin
      Use_Program (Program.Id);
   end Activate;

   ---------
   -- Add --
   ---------

   procedure Add
     (Shader        : in out Rho_Shader_Record'Class;
      Vertex_Shader : Rho_Vertex_Shader)
   is
   begin
      Attach_Shader (Shader.Id, Vertex_Shader.Id);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Shader          : in out Rho_Shader_Record'Class;
      Fragment_Shader : Rho_Fragment_Shader)
   is
   begin
      Attach_Shader (Shader.Id, Fragment_Shader.Id);
   end Add;

   ---------------------
   -- Attribute_Value --
   ---------------------

   function Attribute_Value
     (Program : not null access Rho_Shader_Record'Class;
      Name    : String)
      return Rho_Attribute_Value
   is
      Loc    : constant Int :=
                 GL.Get_Attribute_Location
                   (Program.Id, Name);
      Error  : constant GLenum := GL.Get_Error;
      Result : Rho_Attribute_Value;
   begin
      if Error /= 0 then
         raise Program_Error with Error'Img;
      end if;
      if Loc = -1 then
         if False then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "warning: location for attribute " & Name & " in program "
               & Program.Id'Img & " not found");
         end if;
         return null;
      end if;

      Result :=
        new Rho_Attribute_Value_Record'
          (Rho.Object.Rho_Object_Record with
             Attribute, Rho_Shader (Program), Uniform_Location_Type (Loc));
      Result.Set_Name (Name);
      return Result;
   end Attribute_Value;

   ---------------------
   -- Attribute_Value --
   ---------------------

   function Attribute_Value
     (Program  : not null access Rho_Shader_Record'Class;
      Name     : String;
      Location : Natural)
      return Rho_Attribute_Value
   is
      Result : constant Rho_Attribute_Value :=
                 new Rho_Attribute_Value_Record'
                   (Rho.Object.Rho_Object_Record with
                    Attribute, Rho_Shader (Program),
                    Uniform_Location_Type (Location));
   begin
      Result.Set_Name (Name);
      return Result;
   end Attribute_Value;

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
      GL.Enable_Vertex_Attribute_Array (GL_Types.Uint (Attribute.Location));
      GL.Vertex_Attribute_Pointer
        (Index        => GL_Types.Uint (Attribute.Location),
         Size         => GL_Types.Int (Component_Size),
         Element_Type => GL_Constants.GL_FLOAT,
         Normalized   => GL_Constants.GL_FALSE,
         Stride       => 0,
         Pointer      => Buffer.To_Offset (Start));
   end Bind_Vertex_Buffer;

   -------------
   -- Compile --
   -------------

   procedure Compile (Shader : in out Rho_Shader_Record'Class) is
   begin

      Link_Program (Shader.Id);

      declare
         Result     : aliased Int := 0;
         Log_Length : aliased Int;
      begin
         Get_Program (Shader.Id, GL_LINK_STATUS, Result'Access);
         Shader.Error := Result = 0;

         if Shader.Error then
            Get_Program (Shader.Id, GL_INFO_LOG_LENGTH, Log_Length'Access);
            declare
               Log : constant Interfaces.C.Strings.char_array_access :=
                       new Interfaces.C.char_array
                         (1 .. Interfaces.C.size_t (Log_Length));
            begin
               Get_Shader_Info_Log (Shader.Id, Sizei (Log_Length), null,
                                    Interfaces.C.Strings.To_Chars_Ptr
                                      (Log));
               Shader.Log :=
                 Ada.Strings.Unbounded.To_Unbounded_String
                   (Interfaces.C.To_Ada (Log.all));
               Ada.Text_IO.Put_Line
                 (Ada.Strings.Unbounded.To_String (Shader.Log));
            end;
            Ada.Text_IO.Put_Line ("Compilation failed");
         end if;
      end;

   end Compile;

   ------------
   -- Create --
   ------------

   function Create return Rho_Shader is
      Program : constant Rho_Shader := new Rho_Shader_Record;
   begin
      Program.Id := Create_Program;
      return Program;
   end Create;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate (Program : Rho_Shader_Record'Class) is
      pragma Unreferenced (Program);
   begin
      null;
   end Deactivate;

   procedure Destroy (Shader : in out Rho_Shader) is null;
--     begin
--        Null
--     end Destroy;

   ------------
   -- Exists --
   ------------

   function Exists (Shader_Value : Rho_Shader_Value_Record) return Boolean is
   begin
      return Shader_Value.Location /= -1;
   end Exists;

   --------
   -- Id --
   --------

   function Id (Program : Rho_Shader_Record'Class) return Program_Id is
   begin
      return Program_Id (Program.Id);
   end Id;

   --------------
   -- Location --
   --------------

   function Location
     (Shader_Value : Rho_Shader_Value_Record'Class)
      return Natural
   is
   begin
      return Natural (Shader_Value.Location);
   end Location;

   -------------------
   -- Rho_Initialize --
   -------------------

   procedure Rho_Initialize (Shader : in out Rho_Shader_Record'Class) is
   begin
      Shader.Id := Create_Program;
   end Rho_Initialize;

   ------------
   -- Rho_New --
   ------------

   procedure Rho_New (Shader : in out Rho_Shader) is
   begin
      Shader := Create;
   end Rho_New;

   ---------------
   -- Set_Array --
   ---------------

   procedure Set_Array
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Uniform_Float_Array)
   is
      Arg : Array_Of_Float (Value'Range);
   begin
      for Index in Arg'Range loop
         Arg (Index) := GLfloat (Value (Index));
      end loop;
      GL.Uniform_Float_Array (Uint (Uniform.Location), Arg);

   end Set_Array;

   ---------------
   -- Set_Array --
   ---------------

   procedure Set_Array
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Uniform_Vector_3_Array)
   is
      Arg   : Array_Of_Float (1 .. Value'Length * 3);
      Index : Natural := 0;
   begin
      for V of Value loop
         for X of V loop
            Index := Index + 1;
            Arg (Index) := GLfloat (X);
         end loop;
      end loop;
      GL.Uniform_Float_Vector_3_Array (Uint (Uniform.Location), Arg);
   end Set_Array;

   ---------------
   -- Set_Array --
   ---------------

   procedure Set_Array
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Uniform_Integer_Array)
   is
      Arg : Array_Of_Int (Value'Range);
   begin
      for Index in Arg'Range loop
         Arg (Index) := Int (Value (Index));
      end loop;
      GL.Uniform_Int_Array (Uint (Uniform.Location), Arg);
   end Set_Array;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Rho.Value.Rho_Value)
   is
   begin
      Value.Set_Uniform_Value (Uniform.Location);
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Uniform      : Rho_Uniform_Value_Record;
      Value        : Integer)
   is
   begin
      GL.Uniform (Uint (Uniform.Location), Int (Value));
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Uniform      : Rho_Uniform_Value_Record;
      Value        : Rho.Matrices.Matrix_4)
   is
      GL_Mat : GL_Types.Float_Matrix_4x4;
   begin

      for I in Value'Range (1) loop
         for J in Value'Range (2) loop
            GL_Mat (I + (J - 1) * 4) := GLfloat (Value (I, J));
         end loop;
      end loop;

      GL.Uniform_Matrix
        (Location  => Uint (Uniform.Location),
         Count     => 1,
         Transpose => GL_FALSE,
         Matrix    => GL_Mat);
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Rho.Float_Arrays.Real_Vector)
   is
      GL_Value : Array_Of_Float (Value'Range);
   begin
      for I in Value'Range loop
         GL_Value (I) := GLfloat (Value (I));
      end loop;
      GL.Uniform
        (Location => Uint (Uniform.Location),
         Value    => GL_Value);
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Uniform : Rho_Uniform_Value_Record;
      Value   : Rho_Float)
   is
   begin
      GL.Uniform (Uint (Uniform.Location), GLfloat (Value));
   end Set_Value;

   ------------
   -- Shader --
   ------------

   function Shader
     (Shader_Value : Rho_Shader_Value_Record'Class)
      return Rho_Shader
   is
   begin
      return Shader_Value.Shader;
   end Shader;

   --------------------------
   -- Uniform_Matrix_Value --
   --------------------------

   function Uniform_Matrix_Value
     (Program : not null access Rho_Shader_Record'Class;
      Matrix  : Rho.Matrices.Matrix_Mode_Type)
      return Rho_Uniform_Value
   is
   begin
      if Program.Matrices (Matrix) = null then
         declare
            Name : constant String :=
                     (case Matrix is
                         when Rho.Matrices.Projection => "camera",
                         when Rho.Matrices.Model_View => "model",
                         when Rho.Matrices.Texture    => "texture");
         begin
            Program.Matrices (Matrix) :=
              Program.Uniform_Value (Name);
         end;
      end if;

      return Program.Matrices (Matrix);
   end Uniform_Matrix_Value;

   -------------------
   -- Uniform_Value --
   -------------------

   function Uniform_Value
     (Program : not null access Rho_Shader_Record'Class;
      Name    : String)
      return Rho_Uniform_Value
   is
      Loc    : constant Uint :=
                 GL.Get_Uniform_Location
                   (Program.Id, Interfaces.C.Strings.New_String (Name));
      Error  : constant GLenum := GL.Get_Error;
      Result : Rho_Uniform_Value;
   begin
      if Error /= 0 then
         raise Program_Error with Error'Img;
      end if;
      if Loc = -1 then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "warning: location for " & Name & " in program "
            & Program.Id'Img & " not found");
      end if;

      Result :=
        new Rho_Uniform_Value_Record'
          (Rho.Object.Rho_Object_Record with
             Uniform, Rho_Shader (Program), Uniform_Location_Type (Loc));
      Result.Set_Name (Name);
      return Result;
   end Uniform_Value;

end Rho.Shader;
