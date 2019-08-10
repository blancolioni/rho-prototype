private with Ada.Strings.Unbounded;

private with GL_Types;

with Rho.Float_Arrays;
with Rho.Float_Buffer;
with Rho.Matrices;
with Rho.Object;
with Rho.Value;

package Rho.Shader is

   type Uniform_Integer_Array is
     array (Positive range <>) of Integer;

   type Uniform_Float_Array is
     array (Positive range <>) of Rho_Float;

   type Uniform_Vector_3_Array is
     array (Positive range <>) of Rho.Matrices.Vector_3;

   Rho_Shader_Class_Name : constant String := "shader";

   type Rho_Vertex_Shader is private;

   type Rho_Fragment_Shader is private;

   type Rho_Shader_Record is
     new Rho.Object.Rho_Resource_Record
   with private;

   type Rho_Shader is access all Rho_Shader_Record'Class;

   procedure Rho_New (Shader : in out Rho_Shader);
   procedure Rho_Initialize (Shader : in out Rho_Shader_Record'Class);
   procedure Destroy (Shader : in out Rho_Shader);

   type Rho_Shader_Array is array (Positive range <>) of Rho_Shader;

   overriding function Class_Name
     (Shader : Rho_Shader_Record)
      return String
   is (Rho_Shader_Class_Name);

   procedure Add
     (Shader : in out Rho_Shader_Record'Class;
      Vertex_Shader : Rho_Vertex_Shader);

   procedure Add
     (Shader        : in out Rho_Shader_Record'Class;
      Fragment_Shader : Rho_Fragment_Shader);

   procedure Compile (Shader : in out Rho_Shader_Record'Class);

   procedure Activate (Program : Rho_Shader_Record'Class);
   procedure Deactivate (Program : Rho_Shader_Record'Class);

   type Rho_Shader_Value_Record is
     new Rho.Object.Rho_Object_Record with private;

   function Location
     (Shader_Value : Rho_Shader_Value_Record'Class)
      return Natural;

   function Exists
     (Shader_Value : Rho_Shader_Value_Record)
      return Boolean;

   function Shader
     (Shader_Value : Rho_Shader_Value_Record'Class)
      return Rho_Shader;

   type Rho_Uniform_Value_Record is new Rho_Shader_Value_Record with private;

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

   type Rho_Uniform_Value is access all Rho_Uniform_Value_Record'Class;

   function Uniform_Matrix_Value
     (Program : not null access Rho_Shader_Record'Class;
      Matrix  : Rho.Matrices.Matrix_Mode_Type)
      return Rho_Uniform_Value;

   function Uniform_Value
     (Program : not null access Rho_Shader_Record'Class;
      Name    : String)
      return Rho_Uniform_Value;

   type Rho_Attribute_Value_Record is new Rho_Shader_Value_Record with private;

   type Rho_Attribute_Value is access all Rho_Attribute_Value_Record'Class;

   function Attribute_Value
     (Program : not null access Rho_Shader_Record'Class;
      Name    : String)
      return Rho_Attribute_Value;

   function Attribute_Value
     (Program  : not null access Rho_Shader_Record'Class;
      Name     : String;
      Location : Natural)
      return Rho_Attribute_Value;

   procedure Bind_Vertex_Buffer
     (Attribute      : Rho_Attribute_Value_Record;
      Buffer         : Rho.Float_Buffer.Rho_Float_Buffer;
      Start          : Positive;
      Component_Size : Positive);

   type Program_Id is new Natural;

   function Id (Program : Rho_Shader_Record'Class) return Program_Id;

   function Create return Rho_Shader;

   type Rho_Shader_Interface is interface;

   function Has_Shader (Item : Rho_Shader_Interface) return Boolean
                        is abstract;

   function Shader (Item : in out Rho_Shader_Interface) return Rho_Shader
                    is abstract;

   procedure Set_Shader
     (Item   : in out Rho_Shader_Interface;
      Shader : Rho_Shader)
   is abstract;

private

   type Shader_Value_Class is (Uniform, Attribute);

   type Rho_Shader_Value_Record is new Rho.Object.Rho_Object_Record with
      record
         Class    : Shader_Value_Class;
         Shader   : Rho_Shader;
         Location : Uniform_Location_Type;
      end record;

   type Rho_Uniform_Value_Record is
     new Rho_Shader_Value_Record with null record;

   type Rho_Attribute_Value_Record is
     new Rho_Shader_Value_Record with null record;

   type Matrix_Uniform_Values is
     array (Rho.Matrices.Matrix_Mode_Type) of Rho_Uniform_Value;

   type Rho_Sub_Shader_Type is (Vertex, Fragment);

   type Rho_Sub_Shader_Record is tagged
      record
         Id    : GL_Types.Uint;
         Log   : Ada.Strings.Unbounded.Unbounded_String;
         Error : Boolean;
      end record;

   type Rho_Sub_Shader is access all Rho_Sub_Shader_Record'Class;

   type Rho_Vertex_Shader is new Rho_Sub_Shader;
   type Rho_Fragment_Shader is new Rho_Sub_Shader;

   type Rho_Shader_Record is
     new Rho.Object.Rho_Resource_Record with
      record
         Id       : GL_Types.Uint;
         Log      : Ada.Strings.Unbounded.Unbounded_String;
         Matrices : Matrix_Uniform_Values;
         Error    : Boolean;
      end record;

end Rho.Shader;
