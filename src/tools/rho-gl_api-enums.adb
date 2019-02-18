with Ada.Text_IO;

package body Rho.GL_API.Enums is

   type Enum_Loader is new Tag_Loader with
      record
         Name  : Ada.Strings.Unbounded.Unbounded_String;
         Value : Constant_Value := 0;
      end record;

   overriding procedure On_Attribute
     (Loader   : in out Enum_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String);

   overriding procedure On_End
     (Loader   : in out Enum_Loader;
      Document : in out GL_API_Document'Class);

   ------------------
   -- Child_Loader --
   ------------------

   overriding function Child_Loader
     (Loader : Enums_Loader;
      Name   : String)
      return Tag_Loader'Class
   is
      pragma Unreferenced (Loader);
   begin
      if Name = "enum" then
         return Result : constant Enum_Loader := (others => <>);
      else
         return Result : Null_Loader;
      end if;
   end Child_Loader;

   ------------------
   -- On_Attribute --
   ------------------

   overriding procedure On_Attribute
     (Loader   : in out Enums_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String)
   is
      pragma Unreferenced (Loader, Document);
   begin
      if Name = "namespace" then
         null;
      elsif Name = "group" then
         Loader.Group_Name :=
           Ada.Strings.Unbounded.To_Unbounded_String (Value);
      elsif Name = "type" then
         Loader.Bit_Mask := Value = "bitmask";
      elsif Name = "comment" then
         null;
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
      pragma Unreferenced (Document);
   begin
      if Name = "name" then
         Loader.Name := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      elsif Name = "value" then
         begin
            if Value'Length > 2
              and then Value (Value'First .. Value'First + 1) = "0x"
            then
               Loader.Value :=
                 Constant_Value'Value
                   ("16#" & Value (Value'First + 2 .. Value'Last) & "#");
            elsif Value'Length > 1
              and then Value (Value'First) = '-'
            then
               Loader.Value :=
                 Constant_Value'Last
                   - Constant_Value'Value
                 (Value (Value'First + 1 .. Value'Last))
                   + 1;
            else
               Loader.Value := Constant_Value'Value (Value);
            end if;
         exception
            when Constraint_Error =>
               Ada.Text_IO.Put_Line
                 ("cannot convert " & Value & " to an integer");
               raise;
         end;
      end if;
   end On_Attribute;

   ------------
   -- On_End --
   ------------

   overriding procedure On_End
     (Loader   : in out Enums_Loader;
      Document : in out GL_API_Document'Class)
   is
      Group : constant String :=
                Ada.Strings.Unbounded.To_String (Loader.Group_Name);
   begin
      if Group /= "" then
         if Document.Group_Map.Contains (Group) then
            Document.Group_List (Document.Group_Map.Element (Group)).Bit_Mask
              := Loader.Bit_Mask;
         else
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "warning: group " & Group & " not found");
         end if;
      end if;
   end On_End;

   ------------
   -- On_End --
   ------------

   overriding procedure On_End
     (Loader   : in out Enum_Loader;
      Document : in out GL_API_Document'Class)
   is
      Key : constant String :=
              Ada.Strings.Unbounded.To_String (Loader.Name);
   begin
      if Document.Constant_Map.Contains (Key) then
         declare
            Current_Value : constant Constant_Value :=
                              Document.Constant_Map.Element (Key);
         begin
            if Current_Value = Loader.Value then
               Ada.Text_IO.Put_Line
                 ("warning: " & Key & " defined twice with the same value");
            else
               Ada.Text_IO.Put_Line
                 ("warning: " & Key & " redefined with a different value");
            end if;
         end;

      else
         Document.Constant_Map.Insert
           (Ada.Strings.Unbounded.To_String (Loader.Name), Loader.Value);
         Document.Constant_List.Append
           ((Loader.Name, Loader.Value));
      end if;
   end On_End;

end Rho.GL_API.Enums;
