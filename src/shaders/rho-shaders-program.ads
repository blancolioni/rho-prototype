private with Ada.Strings.Unbounded;

with Rho.Rendering;
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
      Renderer : Rho.Rendering.Rho_Renderer);

   procedure Rho_Initialize
     (Shader   : in out Rho_Program_Record'Class;
      Renderer : Rho.Rendering.Rho_Renderer);

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
     (Program : Rho_Program_Record'Class;
      Name    : String)
      return Values.Rho_Attribute_Value;

   function Uniform_Value
     (Program : Rho_Program_Record'Class;
      Name    : String)
      return Rho.Shaders.Values.Rho_Uniform_Value;

   function Uniform_Matrix_Value
     (Program : Rho_Program_Record'Class;
      Matrix  : Rho.Matrices.Matrix_Mode_Type)
      return Rho.Shaders.Values.Rho_Uniform_Value;

   function Create
     (Renderer : not null access Rho.Rendering.Rho_Renderer_Record'Class)
      return Rho_Program;

   function Create
     (Renderer : not null access Rho.Rendering.Rho_Renderer_Record'Class;
      Shaders  : Rho.Shaders.Shader_Array)
      return Rho_Program;

private

   type Matrix_Uniform_Values is
     array (Rho.Matrices.Matrix_Mode_Type) of Values.Rho_Uniform_Value;

   type Rho_Program_Record is
     new Rho.Object.Rho_Resource_Record
     and Rho_Shader_Interface with
      record
         Id       : Rho_Program_Id;
         Log      : Ada.Strings.Unbounded.Unbounded_String;
         Matrices : Matrix_Uniform_Values;
         Error    : Boolean;
         Renderer : Rho.Rendering.Rho_Renderer;
      end record;

   overriding procedure Activate (Shader : in out Rho_Program_Record);
   overriding procedure Deactivate (Shader : in out Rho_Program_Record);

end Rho.Shaders.Program;
