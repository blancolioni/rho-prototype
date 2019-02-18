package body Rho.GL_API.Features is

   type Require_Loader is
     new Tag_Loader with null record;

   overriding function Child_Loader
     (Loader : Require_Loader;
      Name   : String)
      return Tag_Loader'Class;

   type Enum_Loader is
     new Tag_Loader with null record;

   overriding procedure On_Attribute
     (Loader   : in out Enum_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String);

   type Command_Loader is
     new Tag_Loader with null record;

   overriding procedure On_Attribute
     (Loader   : in out Command_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String);

   ------------------
   -- Child_Loader --
   ------------------

   overriding function Child_Loader
     (Loader : Feature_Loader;
      Name   : String)
      return Tag_Loader'Class
   is
      pragma Unreferenced (Loader);
   begin
      if Name = "require" then
         return Loader : Require_Loader;
      else
         return Default : Null_Loader;
      end if;
   end Child_Loader;

   ------------------
   -- Child_Loader --
   ------------------

   overriding function Child_Loader
     (Loader : Require_Loader;
      Name   : String)
      return Tag_Loader'Class
   is
      pragma Unreferenced (Loader);
   begin
      if Name = "enum" then
         return Enum : Enum_Loader;
      elsif Name = "command" then
         return Command : Command_Loader;
      else
         return Default : Null_Loader;
      end if;
   end Child_Loader;

   ------------------
   -- On_Attribute --
   ------------------

   overriding procedure On_Attribute
     (Loader   : in out Feature_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String)
   is
      pragma Unreferenced (Document);
   begin
      if Name = "api" then
         Loader.API := +Value;
      elsif Name = "name" then
         Loader.Name := +Value;
      elsif Name = "number" then
         Loader.Number := +Value;
      end if;
   end On_Attribute;

   ------------------
   -- On_Attribute --
   ------------------

   overriding procedure On_Attribute
     (Loader   : in out Enum_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String)
   is
      pragma Unreferenced (Loader);
   begin
      if Name = "name" then
         Document.Current_Feature.Enum_List.Append (Value);
         Document.Current_Feature.Enum_Set.Insert (Value);
      end if;
   end On_Attribute;

   ------------------
   -- On_Attribute --
   ------------------

   overriding procedure On_Attribute
     (Loader   : in out Command_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String)
   is
      pragma Unreferenced (Loader);
   begin
      if Name = "name" then
         Document.Current_Feature.Command_List.Append (Value);
         Document.Current_Feature.Command_Set.Insert (Value);
      end if;
   end On_Attribute;

   ------------
   -- On_End --
   ------------

   overriding procedure On_End
     (Loader   : in out Feature_Loader;
      Document : in out GL_API_Document'Class)
   is
   begin
      Document.Feature_Map.Insert
        (Key      => Feature_Key (-Loader.API, -Loader.Name, -Loader.Number),
         New_Item => Document.Current_Feature);
      Document.Current_Feature := (others => <>);
   end On_End;

end Rho.GL_API.Features;
