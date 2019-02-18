with Ada.Text_IO;

with Rho.GL_API.Registry;
with Rho.GL_API.Generator;

package body Rho.GL_API is

   type Top_Level_Loader is
     new Tag_Loader with null record;

   overriding procedure On_Attribute
     (Loader   : in out Top_Level_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String);

   overriding function Child_Loader
     (Loader : Top_Level_Loader;
      Name   : String)
      return Tag_Loader'Class;

   function Base_File_Name
     (Binding  : API_Binding_Language)
      return String
   is (case Binding is
          when C     => "gl",
          when Gnoga => "gnoga-gui-element-canvas-context_webgl");

   function Base_Ada_Name
     (Binding  : API_Binding_Language)
      return String
   is (case Binding is
          when C     => "GL",
          when Gnoga => "Gnoga.Gui.Element.Canvas.Context_WebGL");

   ------------------
   -- Child_Loader --
   ------------------

   function Child_Loader
     (Loader : Tag_Loader;
      Name   : String)
      return Tag_Loader'Class
   is
      pragma Unreferenced (Loader, Name);
   begin
      return Result : Null_Loader;
   end Child_Loader;

   ------------------
   -- Child_Loader --
   ------------------

   overriding function Child_Loader
     (Loader : Top_Level_Loader;
      Name   : String)
      return Tag_Loader'Class
   is
      pragma Unreferenced (Loader);
   begin
      if Name = "registry" then
         return Result : Registry.Registry_Loader;
      else
         return Result : Null_Loader;
      end if;
   end Child_Loader;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (Document : GL_API_Document'Class;
      Binding  : API_Binding_Language;
      API      : String;
      Name     : String;
      Number   : String;
      Path     : String)
   is
      use Ada.Text_IO;
      File    : File_Type;
      Key     : constant String := Feature_Key (API, Name, Number);
   begin

      Create (File, Out_File,
              Path & "/" & Base_File_Name (Binding) & ".ads");
      Set_Output (File);
      Put_Line ("with System;");
      New_Line;
      Put_Line
        ("package "
         & Base_Ada_Name (Binding) & " is");

      New_Line;

      Put_Line ("   pragma Style_Checks (Off);");
      New_Line;

      if Binding = C then
         Generator.Generate_GL_Constants (Document, Binding, Key);
         New_Line;
      end if;

      Generator.Generate_GL_Types (Document, Binding, Key);
      New_Line;

      Generator.Generate_GL_Commands (Document, False, Binding, Key);

      Put_Line ("end " & Base_Ada_Name (Binding) & ";");
      Set_Output (Standard_Output);
      Close (File);

      Create (File, Out_File,
              Path & "/" & Base_File_Name (Binding) & ".adb");
      Set_Output (File);
      Put_Line ("with Gnoga.Server.Connection;");
      New_Line;
      Put_Line
        ("package body "
         & Base_Ada_Name (Binding) & " is");

      New_Line;
      Put_Line ("   pragma Style_Checks (Off);");

      Generator.Generate_GL_Commands (Document, True, Binding, Key);

      Put_Line ("end " & Base_Ada_Name (Binding) & ";");

      Set_Output (Standard_Output);
      Close (File);
      New_Line;

   end Generate;

   --------------
   -- Load_API --
   --------------

   procedure Load_API
     (Document : in out GL_API_Document'Class;
      Path     : String)
   is
      Top : Top_Level_Loader;
   begin
      Document.Loaders.Append (Top);
      Document.Read (Path);
   end Load_API;

   ------------------
   -- On_Attribute --
   ------------------

   overriding procedure On_Attribute
     (Loader   : in out Top_Level_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String)
   is
      pragma Unreferenced (Loader, Document);
   begin
      Ada.Text_IO.Put_Line
        ("top-level: attribute: " & Name & "=" & Value);
   end On_Attribute;

   ------------------
   -- On_Attribute --
   ------------------

   overriding procedure On_Attribute
     (Document        : in out GL_API_Document;
      Attribute_Name  : String;
      Attribute_Value : String)
   is
   begin
      Document.Loaders (Document.Loaders.First).On_Attribute
        (Document, Attribute_Name, Attribute_Value);
   end On_Attribute;

   ------------------
   -- On_Close_Tag --
   ------------------

   overriding procedure On_Close_Tag
     (Document : in out GL_API_Document;
      Tag_Name : String)
   is
      pragma Unreferenced (Tag_Name);
   begin
      Document.Loaders (Document.Loaders.First).On_End (Document);
      Document.Loaders.Delete_First;
   end On_Close_Tag;

   --------------------
   -- On_Inline_Text --
   --------------------

   overriding procedure On_Inline_Text
     (Document : in out GL_API_Document;
      Text     : in     String)
   is
   begin
      Document.Loaders (Document.Loaders.First)
        .On_Inline_Text (Document, Text);
   end On_Inline_Text;

   -----------------
   -- On_Open_Tag --
   -----------------

   overriding procedure On_Open_Tag
     (Document : in out GL_API_Document;
      Tag_Name : String)
   is
      New_Loader : constant Tag_Loader'Class :=
                     Document.Loaders.First_Element.Child_Loader
                       (Tag_Name);
   begin
      Document.Loaders.Insert (Document.Loaders.First, New_Loader);
   end On_Open_Tag;

end Rho.GL_API;
