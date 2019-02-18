with Rho.GL_API;

procedure Rho.Binding_Generator is
   Doc : Rho.GL_API.GL_API_Document;
begin
   Doc.Load_API ("spec/gl.xml");
   Doc.Generate
     (Binding => Rho.GL_API.Gnoga,
      API     => "gles2",
      Name    => "GL_ES_VERSION_2_0",
      Number  => "2.0",
      Path    => "../webgl/ext");
end Rho.Binding_Generator;
