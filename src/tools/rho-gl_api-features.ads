package Rho.GL_API.Features is

   type Feature_Loader is
     new Tag_Loader with private;

private

   type Feature_Loader is
     new Tag_Loader with
      record
         API    : Ada.Strings.Unbounded.Unbounded_String;
         Name   : Ada.Strings.Unbounded.Unbounded_String;
         Number : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding procedure On_Attribute
     (Loader   : in out Feature_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String);

   overriding procedure On_End
     (Loader   : in out Feature_Loader;
      Document : in out GL_API_Document'Class);

   overriding function Child_Loader
     (Loader : Feature_Loader;
      Name   : String)
      return Tag_Loader'Class;

end Rho.GL_API.Features;
