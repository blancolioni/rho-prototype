with Rho.Context;
with Rho.Shaders.Shader;

package Rho.Shaders.Loader is

   function Load
     (Context  : not null access Rho.Context.Rho_Context_Record'Class;
      Path     : String)
      return Rho.Shaders.Shader.Rho_Shader;

   function Load
     (Context  : not null access Rho.Context.Rho_Context_Record'Class;
      Shader   : Rho_Shader_Type;
      Path     : String)
      return Rho.Shaders.Shader.Rho_Shader;

   function Create_From_Source
     (Context     : not null access Rho.Context.Rho_Context_Record'Class;
      Shader      : Rho_Shader_Type;
      Source_Text : String)
      return Rho.Shaders.Shader.Rho_Shader;

   function Load
     (Context             : not null access
        Rho.Context.Rho_Context_Record'Class;
      Vertex_Shader_Path  : String;
      Fragment_Shader_Path : String)
      return Rho.Shaders.Rho_Shader;

   function Load_Standard_Shader
     (Context  : not null access Rho.Context.Rho_Context_Record'Class;
      Name     : String)
      return Rho_Shader;

end Rho.Shaders.Loader;
