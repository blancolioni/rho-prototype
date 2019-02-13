package Rho.Paths is

   Config_Path : constant String :=
     "C:\Users\fwilson\Documents\kiln\rho-trunk\config";

   function Config_File
     (File_Path : String)
     return String
   is (Config_Path & "/" & File_Path);

end Rho.Paths;
