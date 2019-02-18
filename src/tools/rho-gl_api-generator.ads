private package Rho.GL_API.Generator is

   procedure Generate_GL_Constants
     (Document    : GL_API_Document'Class;
      Binding     : API_Binding_Language;
      Feature_Key : String);

   procedure Generate_GL_Types
     (Document    : GL_API_Document'Class;
      Binding     : API_Binding_Language;
      Feature_Key : String);

   procedure Generate_GL_Commands
     (Document      : GL_API_Document'Class;
      Generate_Body : Boolean;
      Binding       : API_Binding_Language;
      Feature_Key   : String);

end Rho.GL_API.Generator;
