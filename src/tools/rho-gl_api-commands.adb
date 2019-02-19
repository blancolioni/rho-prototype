with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Rho.GL_API.Commands is

   type Command_Loader is
     new Tag_Loader with null record;

   overriding procedure On_End
     (Loader   : in out Command_Loader;
      Document : in out GL_API_Document'Class);

   overriding function Child_Loader
     (Loader : Command_Loader;
      Name   : String)
      return Tag_Loader'Class;

   type Proto_Loader is
     new Tag_Loader with null record;

   overriding function Child_Loader
     (Loader : Proto_Loader;
      Name   : String)
      return Tag_Loader'Class;

   overriding procedure On_Attribute
     (Loader   : in out Proto_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String);

   type Proto_Name_Loader is
     new Tag_Loader with null record;

   overriding procedure On_Inline_Text
     (Loader   : in out Proto_Name_Loader;
      Document : in out GL_API_Document'Class;
      Text     : String);

   type Proto_Type_Loader is
     new Tag_Loader with null record;

   overriding procedure On_Inline_Text
     (Loader   : in out Proto_Type_Loader;
      Document : in out GL_API_Document'Class;
      Text     : String);

   type Param_Loader is new Tag_Loader with null record;

   overriding procedure On_Attribute
     (Loader   : in out Param_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String);

   overriding procedure On_End
     (Loader   : in out Param_Loader;
      Document : in out GL_API_Document'Class);

   overriding function Child_Loader
     (Loader : Param_Loader;
      Name   : String)
      return Tag_Loader'Class;

   overriding procedure On_Inline_Text
     (Loader   : in out Param_Loader;
      Document : in out GL_API_Document'Class;
      Text     : String);

   type Ptype_Loader is
     new Tag_Loader with null record;

   overriding procedure On_Inline_Text
     (Loader   : in out Ptype_Loader;
      Document : in out GL_API_Document'Class;
      Text     : String);

   type Pname_Loader is
     new Tag_Loader with null record;

   overriding procedure On_Inline_Text
     (Loader   : in out Pname_Loader;
      Document : in out GL_API_Document'Class;
      Text     : String);

   ------------------
   -- Child_Loader --
   ------------------

   overriding function Child_Loader
     (Loader : Commands_Loader;
      Name   : String)
      return Tag_Loader'Class
   is
      pragma Unreferenced (Loader);
   begin
      if Name = "command" then
         return Loader : Command_Loader;
      else
         return Default : Null_Loader;
      end if;
   end Child_Loader;

   ------------------
   -- Child_Loader --
   ------------------

   overriding function Child_Loader
     (Loader : Command_Loader;
      Name   : String)
      return Tag_Loader'Class
   is
      pragma Unreferenced (Loader);
   begin
      if Name = "proto" then
         return Loader : Proto_Loader;
      elsif Name = "param" then
         return Loader : Param_Loader;
      else
         return Default : Null_Loader;
      end if;
   end Child_Loader;

   ------------------
   -- Child_Loader --
   ------------------

   overriding function Child_Loader
     (Loader : Proto_Loader;
      Name   : String)
      return Tag_Loader'Class
   is
      pragma Unreferenced (Loader);
   begin
      if Name = "name" then
         return Loader : Proto_Name_Loader;
      elsif Name = "ptype" then
         return Loader : Proto_Type_Loader;
      else
         return Default : Null_Loader;
      end if;
   end Child_Loader;

   ------------------
   -- Child_Loader --
   ------------------

   overriding function Child_Loader
     (Loader : Param_Loader;
      Name   : String)
      return Tag_Loader'Class
   is
      pragma Unreferenced (Loader);
   begin
      if Name = "ptype" then
         return Loader : Ptype_Loader;
      elsif Name = "name" then
         return Loader : Pname_Loader;
      else
         return Default : Null_Loader;
      end if;
   end Child_Loader;

   ------------------
   -- On_Attribute --
   ------------------

   overriding procedure On_Attribute
     (Loader   : in out Commands_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String)
   is
      pragma Unreferenced (Loader, Document);
   begin
      if Name = "namespace" then
         if Value /= "GL" then
            Ada.Text_IO.Put_Line ("namespace: " & Value);
         end if;
      end if;
   end On_Attribute;

   ------------------
   -- On_Attribute --
   ------------------

   overriding procedure On_Attribute
     (Loader   : in out Proto_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String)
   is
      pragma Unreferenced (Loader);
   begin
      if Name = "group" then
         Document.Current_Command.Return_Group := +Value;
      end if;
   end On_Attribute;

   ------------------
   -- On_Attribute --
   ------------------

   overriding procedure On_Attribute
     (Loader   : in out Param_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String)
   is
      pragma Unreferenced (Loader);
   begin
      if Name = "group" then
         Document.Current_Parameter.Group_Name := +Value;
      end if;
   end On_Attribute;

   ------------
   -- On_End --
   ------------

   overriding procedure On_End
     (Loader   : in out Commands_Loader;
      Document : in out GL_API_Document'Class)
   is
      pragma Unreferenced (Loader);
   begin
      for Binding of Document.Bindings loop
         declare
            Command : Command_Record :=
                        Command_Record'
                          (Name           => +Binding.Config_Name,
                           Return_Type    =>
                             +(Binding.Get ("return-type", "")),
                           Return_Group   =>
                             +(Binding.Get ("return-group", "")),
                           Parameters     => <>,
                           GLX_Type       => <>,
                           Opcode         => 0,
                           API_Override   => <>,
                           Creates_Object => Binding.Get ("creates-object"));
         begin
            for Language in API_Binding_Language loop
               if Binding.Child ("api").Contains
                 (API_Binding_Language'Image (Language))
               then
                  Command.API_Override (Language) := True;
               end if;
            end loop;

            for Parameter_Config of Binding.Child ("parameters") loop
               Command.Parameters.Append
                 (Parameter_Record'
                    (Group_Name       =>
                         +(Parameter_Config.Get ("group", "")),
                     Type_Name        =>
                       +(Parameter_Config.Get ("type", "")),
                     Parameter_Name   =>
                       +(Parameter_Config.Get ("name", "")),
                     Byte_Offset      =>
                       Parameter_Config.Get ("byte-offset"),
                     Float_Data_Array =>
                       Parameter_Config.Get ("float-data-array"),
                     String_Array     =>
                       Parameter_Config.Get ("string-array"),
                     Writeable_Array  =>
                       Parameter_Config.Get ("writeable-array"),
                     Object_Reference =>
                       Parameter_Config.Get ("object-reference")));
            end loop;
            Document.Command_List.Append (Command);

         end;
      end loop;
   end On_End;

   ------------
   -- On_End --
   ------------

   overriding procedure On_End
     (Loader   : in out Command_Loader;
      Document : in out GL_API_Document'Class)
   is
      pragma Unreferenced (Loader);
      Command_Name : constant String := -Document.Current_Command.Name;
   begin
      if Document.Overrides.Contains (Command_Name) then
         declare
            Override : constant Tropos.Configuration :=
                         Document.Overrides.Child (Command_Name);
            Command : Command_Record :=
                         Command_Record'
                           (Name           => +Command_Name,
                            Return_Type    =>
                              +(Override.Get ("return-type", "")),
                            Return_Group   =>
                              +(Override.Get ("return-group", "")),
                            Parameters     => <>,
                            GLX_Type       => <>,
                            Opcode         => 0,
                            API_Override   => <>,
                            Creates_Object =>
                              Override.Get ("creates-object")
                              or else Command_Name = "glCreateShader"
                                or else Command_Name = "glCreateProgram");
         begin
            for Parameter_Config of Override.Child ("parameters") loop
               Command.Parameters.Append
                 (Parameter_Record'
                    (Group_Name      =>
                         +(Parameter_Config.Get ("group", "")),
                     Type_Name       =>
                       +(Parameter_Config.Get ("type", "")),
                     Parameter_Name  =>
                       +(Parameter_Config.Get ("name", "")),
                     Byte_Offset     =>
                       Parameter_Config.Get ("byte-offset"),
                     Float_Data_Array =>
                       Parameter_Config.Get ("float-data-array"),
                     String_Array     =>
                       Parameter_Config.Get ("string-array"),
                     Writeable_Array =>
                       Parameter_Config.Get ("writeable-array"),
                     Object_Reference =>
                       Parameter_Config.Get ("object-reference")));
            end loop;
            Document.Command_List.Append (Command);
         end;
      else
         Document.Command_List.Append (Document.Current_Command);
      end if;

      Document.Current_Command := (others => <>);
   end On_End;

   ------------
   -- On_End --
   ------------

   overriding procedure On_End
     (Loader   : in out Param_Loader;
      Document : in out GL_API_Document'Class)
   is
      pragma Unreferenced (Loader);
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Document.Current_Parameter.Group_Name /= "BufferSize" then
         Document.Current_Command.Parameters.Append
           (Document.Current_Parameter);
      end if;
      Document.Current_Parameter := (others => <>);
   end On_End;

   --------------------
   -- On_Inline_Text --
   --------------------

   overriding procedure On_Inline_Text
     (Loader   : in out Proto_Name_Loader;
      Document : in out GL_API_Document'Class;
      Text     : String)
   is
      pragma Unreferenced (Loader);
   begin
      Document.Current_Command.Name := +Text;
      if Text = "glCreateProgram" or else Text = "glCreateShader" then
         Document.Current_Command.Creates_Object := True;
      end if;
   end On_Inline_Text;

   --------------------
   -- On_Inline_Text --
   --------------------

   overriding procedure On_Inline_Text
     (Loader   : in out Param_Loader;
      Document : in out GL_API_Document'Class;
      Text     : String)
   is
      pragma Unreferenced (Loader);
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Text = "const void *" then
         Document.Current_Parameter.Float_Data_Array := True;
      elsif Text = "void *" or else Text = "void **" then
         Document.Current_Parameter.Writeable_Array := True;
      elsif Ada.Strings.Fixed.Trim (Text, Ada.Strings.Both) = "*"
        and then Document.Current_Parameter.Type_Name = "GLchar"
      then
         Document.Current_Parameter.Type_Name := +"String";
      end if;
   end On_Inline_Text;

   --------------------
   -- On_Inline_Text --
   --------------------

   overriding procedure On_Inline_Text
     (Loader   : in out Proto_Type_Loader;
      Document : in out GL_API_Document'Class;
      Text     : String)
   is
      pragma Unreferenced (Loader);
   begin
      Document.Current_Command.Return_Type := +Text;
   end On_Inline_Text;

   --------------------
   -- On_Inline_Text --
   --------------------

   overriding procedure On_Inline_Text
     (Loader   : in out Ptype_Loader;
      Document : in out GL_API_Document'Class;
      Text     : String)
   is
      pragma Unreferenced (Loader);
   begin
      Document.Current_Parameter.Type_Name := +Text;
   end On_Inline_Text;

   --------------------
   -- On_Inline_Text --
   --------------------

   overriding procedure On_Inline_Text
     (Loader   : in out Pname_Loader;
      Document : in out GL_API_Document'Class;
      Text     : String)
   is
      pragma Unreferenced (Loader);
   begin
      if Text = "type" then
         Document.Current_Parameter.Parameter_Name := +"Item_Type";
      elsif Text = "range" then
         Document.Current_Parameter.Parameter_Name := +"Item_Range";
      else
         Document.Current_Parameter.Parameter_Name := +Text;
         Document.Current_Parameter.Object_Reference :=
           Text = "program" or else Text = "shader";
      end if;
   end On_Inline_Text;

end Rho.GL_API.Commands;
