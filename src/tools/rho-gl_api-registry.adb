with Ada.Text_IO;

with Rho.GL_API.Commands;
with Rho.GL_API.Enums;
with Rho.GL_API.Features;
with Rho.GL_API.Groups;
with Rho.GL_API.Types;

package body Rho.GL_API.Registry is

   type Comment_Loader is
     new Tag_Loader with null record;

   overriding procedure On_Inline_Text
     (Loader   : in out Comment_Loader;
      Document : in out GL_API_Document'Class;
      Text     : String);

   ------------------
   -- Child_Loader --
   ------------------

   overriding function Child_Loader
     (Loader : Registry_Loader;
      Name   : String)
      return Tag_Loader'Class
   is
      pragma Unreferenced (Loader);
   begin
      if Name = "comment" then
         return Result : Comment_Loader;
      elsif Name = "types" then
         return Result : Types.Types_Loader;
      elsif Name = "groups" then
         return Result : Groups.Groups_Loader;
      elsif Name = "enums" then
         return Result : Enums.Enums_Loader;
      elsif Name = "commands" then
         return Result : Commands.Commands_Loader;
      elsif Name = "feature" then
         return Result : Features.Feature_Loader;
      else
         return Result : Null_Loader;
      end if;
   end Child_Loader;

   --------------------
   -- On_Inline_Text --
   --------------------

   overriding procedure On_Inline_Text
     (Loader   : in out Comment_Loader;
      Document : in out GL_API_Document'Class;
      Text     : String)
   is
      pragma Unreferenced (Loader, Document);
   begin
      Ada.Text_IO.Put_Line (Text);
   end On_Inline_Text;

end Rho.GL_API.Registry;
