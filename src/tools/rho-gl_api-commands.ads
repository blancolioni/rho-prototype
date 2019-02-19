package Rho.GL_API.Commands is

   type Commands_Loader is
     new Tag_Loader with private;

private

   type Commands_Loader is
     new Tag_Loader with
      record
         Group_Name : Ada.Strings.Unbounded.Unbounded_String;
         Bit_Mask   : Boolean := False;
      end record;

   overriding procedure On_Attribute
     (Loader   : in out Commands_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String);

   overriding function Child_Loader
     (Loader : Commands_Loader;
      Name   : String)
      return Tag_Loader'Class;

   overriding procedure On_End
     (Loader   : in out Commands_Loader;
      Document : in out GL_API_Document'Class);

end Rho.GL_API.Commands;
