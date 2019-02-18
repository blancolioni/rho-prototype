package Rho.GL_API.Enums is

   type Enums_Loader is
     new Tag_Loader with private;

private

   type Enums_Loader is
     new Tag_Loader with
      record
         Group_Name : Ada.Strings.Unbounded.Unbounded_String;
         Bit_Mask   : Boolean := False;
      end record;

   overriding procedure On_Attribute
     (Loader   : in out Enums_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String);

   overriding procedure On_End
     (Loader   : in out Enums_Loader;
      Document : in out GL_API_Document'Class);

   overriding function Child_Loader
     (Loader : Enums_Loader;
      Name   : String)
      return Tag_Loader'Class;

end Rho.GL_API.Enums;
