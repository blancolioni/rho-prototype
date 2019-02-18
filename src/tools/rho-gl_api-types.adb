package body Rho.GL_API.Types is

   type Type_Loader is
     new Tag_Loader with null record;

   ------------------
   -- Child_Loader --
   ------------------

   overriding function Child_Loader
     (Loader : Types_Loader;
      Name   : String)
      return Tag_Loader'Class
   is
      pragma Unreferenced (Loader);
   begin
      if Name = "type" then
         return Result : Type_Loader;
      else
         return Result : Null_Loader;
      end if;
   end Child_Loader;

end Rho.GL_API.Types;
