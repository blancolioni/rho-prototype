package Rho.GL_API.Types is

   type Types_Loader is
     new Tag_Loader with private;

private

   type Types_Loader is
     new Tag_Loader with null record;

   overriding function Child_Loader
     (Loader : Types_Loader;
      Name   : String)
      return Tag_Loader'Class;

end Rho.GL_API.Types;
