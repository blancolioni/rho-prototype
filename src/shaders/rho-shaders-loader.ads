with Rho.Rendering;
with Rho.Shaders.Shader;

package Rho.Shaders.Loader is

   function Load
     (Renderer : Rho.Rendering.Rho_Renderer;
      Path     : String)
      return Rho.Shaders.Shader.Rho_Shader;

   function Load
     (Renderer : Rho.Rendering.Rho_Renderer;
      Shader   : Rho_Shader_Type;
      Path     : String)
      return Rho.Shaders.Shader.Rho_Shader;

   function Create_From_Source
     (Renderer    : Rho.Rendering.Rho_Renderer;
      Shader      : Rho_Shader_Type;
      Source_Text : String)
      return Rho.Shaders.Shader.Rho_Shader;

   function Load
     (Renderer             : Rho.Rendering.Rho_Renderer;
      Vertex_Shader_Path  : String;
      Fragment_Shader_Path : String)
      return Rho.Shaders.Rho_Shader;

   function Load_Standard_Shader
     (Renderer : Rho.Rendering.Rho_Renderer;
      Name     : String)
      return Rho_Shader;

end Rho.Shaders.Loader;
