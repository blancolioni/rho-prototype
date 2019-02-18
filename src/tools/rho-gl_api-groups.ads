package Rho.GL_API.Groups is

   type Groups_Loader is
     new Tag_Loader with private;

private

   type Groups_Loader is
     new Tag_Loader with null record;

   overriding function Child_Loader
     (Loader : Groups_Loader;
      Name   : String)
      return Tag_Loader'Class;

end Rho.GL_API.Groups;
