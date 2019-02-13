package Rho.Mesh.Reader is

   function Read_Dat_File
     (Path : String)
      return Rho_Mesh;

   function Read_Mesh_XML_File
     (Path : String)
      return Rho_Mesh;

   function Load (Path : String) return Rho_Mesh;

end Rho.Mesh.Reader;
