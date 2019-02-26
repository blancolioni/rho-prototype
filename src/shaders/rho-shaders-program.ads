private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

--  with Rho.Rendering;
with Rho.Shaders.Shader;
with Rho.Shaders.Values;

package Rho.Shaders.Program is

   Rho_Program_Class_Name : constant String := "shader";

   type Rho_Program_Record is
     new Rho.Object.Rho_Resource_Record
     and Rho_Shader_Interface
   with private;

   type Rho_Program is access all Rho_Program_Record'Class;

   procedure Rho_New
     (Shader   : in out Rho_Program;
      Context  : not null access Rho.Context.Rho_Context_Record'Class);

   procedure Rho_Initialize
     (Shader   : in out Rho_Program_Record'Class;
      Context  : not null access Rho.Context.Rho_Context_Record'Class);

   procedure Destroy
     (Shader   : in out Rho_Program);

   type Rho_Program_Array is array (Positive range <>) of Rho_Program;

   overriding function Class_Name
     (Shader : Rho_Program_Record)
      return String
   is (Rho_Program_Class_Name);

   procedure Add
     (Program : in out Rho_Program_Record'Class;
      Shader  : Rho.Shaders.Shader.Rho_Shader);

   procedure Compile (Shader : in out Rho_Program_Record'Class);

   function Id (Program : Rho_Program_Record'Class) return Rho_Program_Id;

   function Attribute_Value
     (Program : not null access Rho_Program_Record'Class;
      Name    : String)
      return Rho.Shaders.Values.Rho_Attribute_Value;

   function Uniform_Value
     (Program : not null access Rho_Program_Record'Class;
      Name    : String)
      return Rho.Shaders.Values.Rho_Uniform_Value;

   function Uniform_Matrix_Value
     (Program : in out Rho_Program_Record'Class;
      Matrix  : Rho.Matrices.Matrix_Mode_Type)
      return Rho.Shaders.Values.Rho_Uniform_Value;

   function Create
     (Context : not null access Rho.Context.Rho_Context_Record'Class)
      return Rho_Program;

   function Create
     (Context  : not null access Rho.Context.Rho_Context_Record'Class;
      Shaders  : Rho.Shaders.Shader_Array)
      return Rho_Program;

private

   type Matrix_Uniform_Values is
     array (Rho.Matrices.Matrix_Mode_Type) of Values.Rho_Uniform_Value;

   package Shader_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Rho.Shaders.Shader.Rho_Shader,
        Rho.Shaders.Shader."=");

   type Rho_Program_Record is
     new Rho.Object.Rho_Resource_Record
     and Rho_Shader_Interface with
      record
         Id       : Rho_Program_Id;
         Log      : Ada.Strings.Unbounded.Unbounded_String;
         Matrices : Matrix_Uniform_Values;
         Error    : Boolean;
         Context  : access Rho.Context.Rho_Context_Record'Class;
         Shaders  : Shader_Lists.List;
      end record;

   overriding procedure Activate (Shader : in out Rho_Program_Record);
   overriding procedure Deactivate (Shader : in out Rho_Program_Record);
   overriding function Context
     (Program : Rho_Program_Record)
      return not null access Rho.Context.Rho_Context_Record'Class
   is (Program.Context);

end Rho.Shaders.Program;
