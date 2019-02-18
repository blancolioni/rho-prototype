package Rho.GL_API.Registry is

   type Registry_Loader is
     new Tag_Loader with private;

private

   type Registry_Loader is
     new Tag_Loader with null record;

   overriding procedure On_Attribute
     (Loader   : in out Registry_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String)
   is null;

   overriding function Child_Loader
     (Loader : Registry_Loader;
      Name   : String)
      return Tag_Loader'Class;

end Rho.GL_API.Registry;
