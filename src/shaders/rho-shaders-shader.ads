private with Ada.Strings.Unbounded;

with Rho.Rendering;

package Rho.Shaders.Shader is

   type Rho_Shader_Record is tagged private;

   type Rho_Shader is access all Rho_Shader_Record'Class;

   function Id (Shader : Rho_Shader_Record'Class) return Rho_Shader_Id;

   function Create_Shader
     (Renderer : Rho.Rendering.Rho_Renderer;
      Shader   : Rho_Shader_Type;
      Source   : String)
      return Rho_Shader;

private

   type Rho_Shader_Record is tagged
      record
         Shader : Rho_Shader_Type;
         Id     : Rho_Shader_Id    := 0;
         Log    : Ada.Strings.Unbounded.Unbounded_String;
         Error  : Boolean := True;
      end record;

   function Id (Shader : Rho_Shader_Record'Class) return Rho_Shader_Id
   is (Shader.Id);

end Rho.Shaders.Shader;
