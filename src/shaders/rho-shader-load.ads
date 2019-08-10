package Rho.Shader.Load is

   procedure Add_Search_Path
     (Path : String);

   function Load
     (Name : String;
      Ext  : String := "vert")
      return Rho_Vertex_Shader;

   function Create_From_Source
     (Source_Text : String)
      return Rho_Vertex_Shader;

   function Load
     (Name : String;
      Ext  : String := "frag")
      return Rho_Fragment_Shader;

   function Create_From_Source
     (Source_Text : String)
      return Rho_Fragment_Shader;

   function Load
     (Vertex_Shader_Name, Fragment_Shader_Name : String)
      return Rho_Shader;

   function Load_Standard_Shader (Name : String) return Rho_Shader;

end Rho.Shader.Load;
