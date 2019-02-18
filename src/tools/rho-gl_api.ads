private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;
private with WL.String_Maps;
private with WL.String_Sets;

with Partoe;

package Rho.GL_API is

   type API_Binding_Language is (C, Gnoga);

   type GL_API_Document is
     new Partoe.Partoe_Document with private;

   procedure Load_API
     (Document : in out GL_API_Document'Class;
      Path     : String);

   procedure Generate
     (Document : GL_API_Document'Class;
      Binding  : API_Binding_Language;
      API      : String;
      Name     : String;
      Number   : String;
      Path     : String);

   type Tag_Loader is abstract tagged private;

   procedure On_Attribute
     (Loader   : in out Tag_Loader;
      Document : in out GL_API_Document'Class;
      Name     : String;
      Value    : String)
   is null;

   procedure On_Inline_Text
     (Loader   : in out Tag_Loader;
      Document : in out GL_API_Document'Class;
      Text     : String)
   is null;

   procedure On_End
     (Loader   : in out Tag_Loader;
      Document : in out GL_API_Document'Class)
   is null;

   function Child_Loader
     (Loader : Tag_Loader;
      Name   : String)
      return Tag_Loader'Class;

private

   type Tag_Loader is abstract tagged null record;

   type Constant_Value is mod 2 ** 64;

   type GL_Constant_Name is
      record
         Name  : Ada.Strings.Unbounded.Unbounded_String;
         Value : Constant_Value;
      end record;

   package GL_Constant_Maps is
     new WL.String_Maps (Constant_Value);

   package GL_Constant_Lists is
     new Ada.Containers.Doubly_Linked_Lists (GL_Constant_Name);

   package Identifier_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Group_Record is
      record
         Bit_Mask : Boolean;
         Name     : Ada.Strings.Unbounded.Unbounded_String;
         Ids      : Identifier_Lists.List;
      end record;

   package Group_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Group_Record);

   package Group_Maps is
     new WL.String_Maps (Group_Lists.Cursor, Group_Lists."=");

   type Parameter_Record is
      record
         Group_Name      : Ada.Strings.Unbounded.Unbounded_String;
         Type_Name       : Ada.Strings.Unbounded.Unbounded_String;
         Parameter_Name  : Ada.Strings.Unbounded.Unbounded_String;
         Data_Array      : Boolean := False;
         Writeable_Array : Boolean := False;
      end record;

   package Parameter_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Parameter_Record);

   type Command_Record is
      record
         Name         : Ada.Strings.Unbounded.Unbounded_String;
         Return_Type  : Ada.Strings.Unbounded.Unbounded_String;
         Return_Group : Ada.Strings.Unbounded.Unbounded_String;
         Parameters   : Parameter_Lists.List;
         GLX_Type     : Ada.Strings.Unbounded.Unbounded_String;
         Opcode       : Natural := 0;
      end record;

   package Command_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Command_Record);

   type Require_Record is
      record
         Enum_List    : Identifier_Lists.List;
         Enum_Set     : WL.String_Sets.Set;
         Command_List : Identifier_Lists.List;
         Command_Set  : WL.String_Sets.Set;
      end record;

   package Feature_Maps is
     new WL.String_Maps (Require_Record);

   package Loader_Stacks is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Tag_Loader'Class);

   type GL_API_Document is
     new Partoe.Partoe_Document with
      record
         Loaders           : Loader_Stacks.List;
         Constant_Map      : GL_Constant_Maps.Map;
         Constant_List     : GL_Constant_Lists.List;
         Group_Map         : Group_Maps.Map;
         Group_List        : Group_Lists.List;
         Command_List      : Command_Lists.List;
         Feature_Map       : Feature_Maps.Map;
         Current_Group     : Ada.Strings.Unbounded.Unbounded_String;
         Current_Ids       : Identifier_Lists.List;
         Current_Command   : Command_Record;
         Current_Parameter : Parameter_Record;
         Current_Feature   : Require_Record;
      end record;

   overriding procedure On_Open_Tag
     (Document : in out GL_API_Document;
      Tag_Name : String);

   overriding procedure On_Close_Tag
     (Document : in out GL_API_Document;
      Tag_Name : String);

   overriding procedure On_Attribute
     (Document        : in out GL_API_Document;
      Attribute_Name  : String;
      Attribute_Value : String);

   overriding procedure On_Inline_Text
     (Document : in out GL_API_Document;
      Text     : in     String);

   type Null_Loader is
     new Tag_Loader with null record;

   function Feature_Key
     (API    : String;
      Name   : String;
      Number : String)
      return String
   is (API & "--" & Name & "--" & Number);

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

end Rho.GL_API;
