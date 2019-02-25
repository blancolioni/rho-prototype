with Rho.GL_API;

procedure Rho.Binding_Generator is
   Doc : Rho.GL_API.GL_API_Document;
begin
   Doc.Load_API ("spec/gl.xml", "spec/configuration.txt");
   Doc.Generate
     (Binding => Rho.GL_API.Gnoga,
      API     => "gles2",
      Name    => "GL_ES_VERSION_2_0",
      Number  => "2.0",
      Path    =>
        "E:\home\fraser\Documents\kiln\rho-trunk\src\rendering\webgl");
end Rho.Binding_Generator;
