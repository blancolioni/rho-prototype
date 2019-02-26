package body Rho.Shaders.Shader is

   -------------------
   -- Create_Shader --
   -------------------

   function Create_Shader
     (Renderer : Rho.Rendering.Rho_Renderer;
      Shader   : Rho_Shader_Type;
      Source   : String)
      return Rho_Shader
   is
   begin
      return Result : constant Rho_Shader := new Rho_Shader_Record do
         Result.Id := Renderer.Load_Shader (Shader, Source);
         Result.Shader := Shader;
      end return;
   end Create_Shader;

end Rho.Shaders.Shader;
