
package body Rho.GL_API.Groups is

   type Group_Loader is new Tag_Loader with
      record
         Group_Name : Ada.Strings.Unbounded.Unbounded_String;
         Bit_Mask   : Boolean := False;
      end record;

   overriding procedure On_Attribute
     (Loader   : in out Group_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String);

   overriding procedure On_End
     (Loader   : in out Group_Loader;
      Document : in out GL_API_Document'Class);

   overriding function Child_Loader
     (Loader : Group_Loader;
      Name   : String)
      return Tag_Loader'Class;

   type Group_Enum_Loader is new Tag_Loader with null record;

   overriding procedure On_Attribute
     (Loader   : in out Group_Enum_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String);

   ------------------
   -- Child_Loader --
   ------------------

   overriding function Child_Loader
     (Loader : Groups_Loader;
      Name   : String)
      return Tag_Loader'Class
   is
      pragma Unreferenced (Loader);
   begin
      if Name = "group" then
         return Result : Group_Loader;
      else
         return Result : Null_Loader;
      end if;
   end Child_Loader;

   ------------------
   -- Child_Loader --
   ------------------

   overriding function Child_Loader
     (Loader : Group_Loader;
      Name   : String)
      return Tag_Loader'Class
   is
      pragma Unreferenced (Loader);
   begin
      if Name = "enum" then
         return Result : Group_Enum_Loader;
      else
         return Result : Null_Loader;
      end if;
   end Child_Loader;

   ------------------
   -- On_Attribute --
   ------------------

   overriding procedure On_Attribute
     (Loader   : in out Group_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String)
   is
   begin
      if Name = "name" then
         Loader.Group_Name :=
           Ada.Strings.Unbounded.To_Unbounded_String (Value);
         Document.Current_Ids.Clear;
      end if;
   end On_Attribute;

   ------------------
   -- On_Attribute --
   ------------------

   overriding procedure On_Attribute
     (Loader   : in out Group_Enum_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String)
   is
      pragma Unreferenced (Loader);
   begin
      if Name = "name" then
         Document.Current_Ids.Append (Value);
      end if;
   end On_Attribute;

   ------------
   -- On_End --
   ------------

   overriding procedure On_End
     (Loader   : in out Group_Loader;
      Document : in out GL_API_Document'Class)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Loader.Group_Name /= "Boolean" then
         Document.Group_List.Append
           (Group_Record'
              (Name     => Loader.Group_Name,
               Bit_Mask => Loader.Bit_Mask,
               Ids      => Document.Current_Ids));
         Document.Group_Map.Insert
           (Ada.Strings.Unbounded.To_String (Loader.Group_Name),
            Document.Group_List.Last);
      end if;
   end On_End;

end Rho.GL_API.Groups;
