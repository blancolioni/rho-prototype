with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Rho.GL_API.Commands is

   procedure Override_Current_Command
     (Document : in out GL_API_Document'Class;
      Config   : Tropos.Configuration);

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
                          (Name            => +Binding.Config_Name,
                           Ada_Name        =>
                             +(Binding.Get ("name", "")),
                           Return_Type     =>
                             +(Binding.Get ("return-type", "")),
                           Return_Group    =>
                             +(Binding.Get ("return-group", "")),
                           Parameters      => <>,
                           GLX_Type        => <>,
                           Opcode          => 0,
                           API_Override    => <>,
                           Have_Data_Array => <>,
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
                     Parameter_Value  =>
                       +(Parameter_Config.Get ("value", "")),
                     Implied          =>
                       Parameter_Config.Get ("implied"),
                     Byte_Offset      =>
                       Parameter_Config.Get ("byte-offset"),
                     Data_Array =>
                       Parameter_Config.Get ("data-array"),
                     Image_Data       =>
                       Parameter_Config.Get ("image-data"),
                     Html_Element_Id  =>
                       Parameter_Config.Get ("html-element-id"),
                     String_Array     =>
                       Parameter_Config.Get ("string-array"),
                     Writeable_Array  =>
                       Parameter_Config.Get ("writeable-array"),
                     Object_Reference =>
                       Parameter_Config.Get ("object-reference")));
               Command.Have_Data_Array :=
                 Command.Have_Data_Array or else
                 Command.Parameters.Last_Element.Data_Array;
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
         begin
            Override_Current_Command (Document, Override);
         end;
      end if;

      Document.Command_List.Append (Document.Current_Command);
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
         Document.Current_Parameter.Data_Array := True;
         Document.Current_Command.Have_Data_Array := True;
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

   ------------------------------
   -- Override_Current_Command --
   ------------------------------

   procedure Override_Current_Command
     (Document : in out GL_API_Document'Class;
      Config   : Tropos.Configuration)
   is
      Command : Command_Record renames Document.Current_Command;
   begin
      if Config.Get ("replace") then
         Command :=
           (Name            => Document.Current_Command.Name,
            Ada_Name        => <>,
            Return_Type     =>
              +(Config.Get ("return-type", "")),
            Return_Group    =>
              +(Config.Get ("return-group", "")),
            Parameters      => <>,
            GLX_Type        => <>,
            Opcode          => 0,
            API_Override    => <>,
            Have_Data_Array => <>,
            Creates_Object  =>
              Config.Get ("creates-object"));

         for Parameter_Config of Config.Child ("parameters") loop
            Command.Parameters.Append
              (Parameter_Record'
                 (Group_Name       =>
                      +(Parameter_Config.Get ("group", "")),
                  Type_Name        =>
                    +(Parameter_Config.Get ("type", "")),
                  Parameter_Name   =>
                    +(Parameter_Config.Get ("name", "")),
                  Parameter_Value  =>
                    +(Parameter_Config.Get ("value", "")),
                  Implied          =>
                    Parameter_Config.Get ("implied"),
                  Byte_Offset      =>
                    Parameter_Config.Get ("byte-offset"),
                  Data_Array       =>
                    Parameter_Config.Get ("data-array"),
                  Image_Data       =>
                    Parameter_Config.Get ("image-data"),
                  Html_Element_Id  =>
                    Parameter_Config.Get ("html-element-id"),
                  String_Array     =>
                    Parameter_Config.Get ("string-array"),
                  Writeable_Array  =>
                    Parameter_Config.Get ("writeable-array"),
                  Object_Reference =>
                    Parameter_Config.Get ("object-reference")));
            Command.Have_Data_Array :=
              Command.Have_Data_Array or else
              Command.Parameters.Last_Element.Data_Array;
         end loop;
      else
         if Config.Contains ("name") then
            Command.Ada_Name := +(Config.Get ("name"));
         end if;
         if Config.Contains ("return-type") then
            Command.Return_Type := +(Config.Get ("return-type"));
         end if;
         if Config.Contains ("return-group") then
            Command.Return_Group := +(Config.Get ("return-group"));
         end if;
         if Config.Contains ("creates-object") then
            Command.Creates_Object := Config.Get ("creates-object");
         end if;

         declare
            Parameters_Config : constant Tropos.Configuration :=
                                  Config.Child ("parameters");
            package Config_Child_Maps is
              new WL.String_Maps (Tropos.Configuration, Tropos."=");
            Config_Child : Config_Child_Maps.Map;
         begin
            for Param_Config of Parameters_Config loop
               Config_Child.Insert (Param_Config.Get ("name"),
                                    Param_Config);
            end loop;

            for Parameter of Command.Parameters loop
               declare
                  Parameter_Name : constant String :=
                                     -Parameter.Parameter_Name;
               begin
                  if Config_Child.Contains (Parameter_Name) then
                     declare
                        Item : constant Tropos.Configuration :=
                                 Config_Child.Element (Parameter_Name);
                     begin
                        if Item.Contains ("group") then
                           Parameter.Group_Name :=
                             +(Item.Get ("group"));
                        end if;
                        if Item.Contains ("type") then
                           Parameter.Type_Name :=
                             +(Item.Get ("type"));
                        end if;
                        if Item.Contains ("value") then
                           Parameter.Parameter_Value :=
                             +(Item.Get ("value"));
                        end if;
                        if Item.Contains ("implied") then
                           Parameter.Implied := Item.Get ("implied");
                        end if;
                        if Item.Contains ("byte-offset") then
                           Parameter.Byte_Offset := Item.Get ("byte-offset");
                        end if;
                        if Item.Contains ("data-array") then
                           Parameter.Data_Array := Item.Get ("data-array");
                        end if;
                        if Item.Contains ("image-data") then
                           Parameter.Image_Data := Item.Get ("image-data");
                        end if;
                        if Item.Contains ("string-array") then
                           Parameter.String_Array := Item.Get ("string-array");
                        end if;
                        if Item.Contains ("writeable-array") then
                           Parameter.Writeable_Array :=
                             Item.Get ("writeable-array");
                        end if;
                        if Item.Contains ("object-reference") then
                           Parameter.Object_Reference :=
                             Item.Get ("object-reference");
                        end if;
                     end;
                  end if;
               end;
               Command.Have_Data_Array :=
                 Command.Have_Data_Array or else Parameter.Data_Array;
            end loop;
         end;
      end if;
   end Override_Current_Command;

end Rho.GL_API.Commands;
