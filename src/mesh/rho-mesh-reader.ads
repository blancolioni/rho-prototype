package Rho.Mesh.Reader is

   function Read_Dat_File
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Path    : String)
      return Rho_Mesh;

   function Read_Mesh_XML_File
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Path    : String)
      return Rho_Mesh;

   function Load
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Path    : String)
      return Rho_Mesh;

end Rho.Mesh.Reader;
