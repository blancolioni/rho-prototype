with Ada.Characters.Handling;
with Ada.Text_IO;

package body Rho.GL_API.Generator is

   function To_Ada_Command_Name
     (Command_Name : String)
      return String;

   function To_Ada_Parameter_Name
     (Parameter_Name : String)
      return String;

   function To_WebGL_Command_Name
     (Command_Name : String)
      return String;

   --------------------------
   -- Generate_GL_Commands --
   --------------------------

   procedure Generate_GL_Commands
     (Document      : GL_API_Document'Class;
      Generate_Body : Boolean;
      Binding       : API_Binding_Language;
      Feature_Key   : String)
   is
      use Ada.Text_IO;
      Feature : Require_Record renames Document.Feature_Map (Feature_Key);
   begin

      if not Generate_Body then
         if Binding = Gnoga then
            Put_Line
              ("   type Context_WebGL_Type is"
               & " new Context_Type with private;");
            Put_Line
              ("   type Context_WebGL_Access is"
               & " access all Context_WebGL_Type;");
            Put_Line
              ("   type Pointer_To_Context_WebGL_Class is"
               & " access all Context_WebGL_Type'Class;");

            New_Line;

            Put_Line ("   procedure Get_Drawing_Context_WebGL");
            Put_Line ("     (Context   : in out Context_WebGL_Type;");
            Put_Line ("      Canvas  : in out Canvas_Type'Class);");
            New_Line;
         end if;
      end if;

      if Generate_Body then
         Put_Line ("   function GLEnum_Property");
         Put_Line ("     (Context : Context_WebGL_Type'Class;");
         Put_Line ("      Name    : String)");
         Put_Line ("     return String");
         Put_Line ("   is (Context.Property "
                   & "(Name (Name'First + 3 .. Name'Last)));");

         New_Line;
         Put_Line ("   -------------------------------");
         Put_Line ("   -- Get_Drawing_Context_WebGL --");
         Put_Line ("   -------------------------------");
         New_Line;
         Put_Line ("   procedure Get_Drawing_Context_WebGL");
         Put_Line ("     (Context : in out Context_WebGL_Type;");
         Put_Line ("      Canvas  : in out Canvas_Type'Class)");
         Put_Line ("   is");
         Put_Line ("      GID : constant String := "
                   & "Gnoga.Server.Connection.New_GID;");
         Put_Line ("   begin");
         Put_Line ("      Context.Context_ID := "
                   & "Ada.Strings.Unbounded.To_Unbounded_String (GID);");
         Put_Line ("      Context.Connection_ID := Canvas.Connection_ID;");
         New_Line;
         Put_Line ("      Gnoga.Server.Connection.Execute_Script");
         Put_Line ("        (Context.Connection_ID,");
         Put_Line ("         ""gnoga['"" & GID & ""'] = "" &");
         Put_Line ("           Canvas.jQuery &");
         Put_Line ("           "".get(0).getContext('webgl');"");");
         Put_Line ("   end Get_Drawing_Context_WebGL;");
         New_Line;
      end if;

      for Command of Document.Command_List loop

         declare
            use type Ada.Strings.Unbounded.Unbounded_String;
            Name : constant String := -Command.Name;
            Ada_Name : constant String :=
                         (if Document.Group_Map.Contains
                            (Name (Name'First + 2 .. Name'Last))
                          then "Set_" & To_Ada_Command_Name (Name)
                          else To_Ada_Command_Name (Name));
            Return_Type : constant String :=
                            (if Command.Return_Group = ""
                             then -Command.Return_Type
                             else -Command.Return_Group);
            Is_Procedure : constant Boolean :=
                             Return_Type = "";
            First_Parameter : Boolean := True;
            Have_Data_Array : Boolean := False;
            Data_Array_Param : Parameter_Record;
            Have_Group_Mask  : Boolean := False;
            Group_Parameter  : Parameter_Record;
            Group_Mask       : Group_Record;
         begin
            if Feature.Command_Set.Contains (Name)
              and then Is_Procedure
              and then Ada_Name /= "Read_Pixels"
              and then Ada_Name /= "Get_Vertex_Attrib_Pointerv"
            then

               if Generate_Body then
                  declare
                     S : constant String (1 .. Name'Length + 6) :=
                           (others => '-');
                  begin
                     Put_Line ("   " & S);
                     Put_Line ("   -- " & Ada_Name & " --");
                     Put_Line ("   " & S);
                     New_Line;
                  end;
               end if;

               if Is_Procedure then
                  Put ("   procedure ");
               else
                  Put ("   function ");
               end if;

               Put (Ada_Name);

               if Binding = Gnoga then
                  New_Line;
                  Put ("     (Context : in out Context_WebGL_Type'Class");
                  First_Parameter := False;
               end if;
               for Parameter of Command.Parameters loop
                  declare
                     Parameter_Name  : constant String :=
                                         -Parameter.Parameter_Name;
                     Parameter_Type  : constant String :=
                                         -Parameter.Type_Name;
                     Parameter_Group : constant String :=
                                         -Parameter.Group_Name;
                  begin

                     if Parameter.Data_Array then
                        Have_Data_Array := True;
                        Data_Array_Param := Parameter;
                     end if;

                     if First_Parameter then
                        New_Line;
                        Put ("     (");
                        First_Parameter := False;
                     else
                        Put_Line (";");
                        Put ("      ");
                     end if;
                     Put
                       (Ada.Characters.Handling.To_Upper
                          (Parameter_Name (Parameter_Name'First)));
                     Put (Parameter_Name (Parameter_Name'First + 1
                          .. Parameter_Name'Last));
                     Put (" : ");
                     if Parameter.Data_Array then
                        Put ("Float_Array");
                     elsif Parameter.Writeable_Array then
                        Put ("GLvoidptr");
                     elsif Parameter_Group = ""
                       or else not Document.Group_Map.Contains
                         (Parameter_Group)
                     then
                        Put (Parameter_Type);
                     else
                        declare
                           Group : Group_Record renames
                                     Document.Group_List
                                       (Document.Group_Map.Element
                                          (Parameter_Group));
                        begin
                           Put (To_Ada_Command_Name (Parameter_Group));
                           if Group.Bit_Mask then
                              Put ("_Array");
                              Have_Group_Mask := True;
                              Group_Parameter := Parameter;
                              Group_Mask := Group;
                           end if;
                        end;
                     end if;
                  end;
               end loop;

               if not First_Parameter then
                  Put (")");
               end if;

               if not Is_Procedure then
                  New_Line;
                  Put ("     return ");
                  Put (To_Ada_Command_Name (Return_Type));
               end if;

               if Generate_Body then
                  if First_Parameter then
                     Put_Line (" is");
                  else
                     New_Line;
                     Put_Line ("   is");
                  end if;

                  if Have_Data_Array or else Have_Group_Mask then
                     Put_Line
                       ("      use Ada.Strings.Unbounded;");
                  end if;

                  if Have_Data_Array then
                     Put_Line
                       ("      Data_Image : Unbounded_String;");
                  end if;

                  if Have_Group_Mask then
                     Put_Line
                       ("      Mask_Image : Unbounded_String;");
                  end if;

                  Put_Line ("   begin");
                  if Have_Data_Array then
                     Put_Line
                       ("      for X of " & (-Data_Array_Param.Parameter_Name)
                        & " loop");
                     Put_Line
                       ("         if Data_Image /= """" then");
                     Put_Line
                       ("            Data_Image := Data_Image & "","";");
                     Put_Line
                       ("         end if;");
                     Put_Line
                       ("         Data_Image := Data_Image & X'Image;");
                     Put_Line
                       ("      end loop;");
                  end if;

                  if Have_Group_Mask then
                     Put_Line
                       ("      for X of " & (-Group_Parameter.Parameter_Name)
                        & " loop");
                     Put_Line
                       ("         if Mask_Image /= """" then");
                     Put_Line
                       ("            Mask_Image := Mask_Image & ""|"";");
                     Put_Line
                       ("         end if;");
                     Put_Line
                       ("         Mask_Image := Mask_Image & "
                        & "Context.GLEnum_Property (X'Image);");
                     Put_Line
                       ("      end loop;");
                  end if;

                  Put_Line ("      "
                            & (if Is_Procedure then "" else "return ")
                            & "Context.Execute");
                  Put_Line
                    ("        (""" & To_WebGL_Command_Name (Name)
                     & "(""");
                  First_Parameter := True;
                  for Parameter of Command.Parameters loop
                     declare
                        Parameter_Name  : constant String :=
                                            -Parameter.Parameter_Name;
                        Parameter_Type  : constant String :=
                                            -Parameter.Type_Name;
                        Parameter_Group : constant String :=
                                            -Parameter.Group_Name;
                     begin
                        if First_Parameter then
                           First_Parameter := False;
                        else
                           Put_Line (" & "",""");
                        end if;
                        Put ("         & ");

                        if Parameter.Data_Array then
                           Put
                             ("""new Float32Array(["" & "
                              & "To_String (Data_Image) & ""])""");
                        elsif Have_Group_Mask
                          and then Parameter.Parameter_Name
                            = Group_Parameter.Parameter_Name
                        then
                           Put ("To_String (Mask_Image)");
                        else
                           if Parameter_Group /= ""
                             and then Parameter_Group /= "ColorF"
                             and then Parameter_Type /= "GLfloat"
                           then
                              Put ("Context.GLEnum_Property (");
                           end if;
                           Put (To_Ada_Parameter_Name (Parameter_Name));
                           Put ("'Image");
                           if Parameter_Group /= ""
                             and then Parameter_Group /= "ColorF"
                             and then Parameter_Type /= "GLfloat"
                           then
                              Put (")");
                           end if;
                        end if;
                     end;
                  end loop;
                  Put_Line (" & "")"");");
                  Put_Line ("   end " & Ada_Name & ";");
               else
                  Put_Line (";");
               end if;
               New_Line;
            end if;
         end;

      end loop;

      if not Generate_Body then
         if Binding = Gnoga then
            Put_Line ("private");
            New_Line;

            Put_Line
              ("   type Context_WebGL_Type is"
               & " new Context_Type with null record;");
            New_Line;
         end if;
      end if;

   end Generate_GL_Commands;

   ---------------------------
   -- Generate_GL_Constants --
   ---------------------------

   procedure Generate_GL_Constants
     (Document    : GL_API_Document'Class;
      Binding     : API_Binding_Language;
      Feature_Key : String)
   is
      use Ada.Text_IO;
      Feature : Require_Record renames Document.Feature_Map (Feature_Key);
      pragma Unreferenced (Binding);
   begin
      Put_Line ("   pragma Style_Checks (Off);");
      New_Line;

      for Id of Document.Constant_List loop
         if Feature.Enum_Set.Contains (-Id.Name) then
            Put_Line
              ("   "
               & Ada.Strings.Unbounded.To_String (Id.Name)
               & " : constant :="
               & Id.Value'Image
               & ";");
         end if;
      end loop;

   end Generate_GL_Constants;

   -----------------------
   -- Generate_GL_Types --
   -----------------------

   procedure Generate_GL_Types
     (Document    : GL_API_Document'Class;
      Binding     : API_Binding_Language;
      Feature_Key : String)
   is
      use Ada.Text_IO;

      Feature : Require_Record renames Document.Feature_Map (Feature_Key);

      function Less (Left, Right : GL_Constant_Name) return Boolean
      is (Left.Value < Right.Value);

      package Constant_Name_Sorting is
        new GL_Constant_Lists.Generic_Sorting (Less);

   begin
      Put_Line ("   type GLuchar is mod 2 ** 8;");
      Put_Line ("   type GLchar is range -128 .. 127;");
      Put_Line ("   type GLint is range -2 ** 31 .. 2 ** 31 - 1;");
      Put_Line ("   type GLuint is mod 2 **32;");
      Put_Line ("   type GLenum is new GLint;");
      Put_Line ("   type GLfloat is new Float;");
      Put_Line ("   type GLsizei is new GLuint;");
      Put_Line ("   type GLintptr is new GLuint;");
      Put_Line ("   type GLvoidptr is new System.Address;");
      New_Line;
      Put_Line ("   type Float_Array is "
                & "array (Positive range <>) of GLfloat;");
      Put_Line ("   type CheckedInt32 is "
                & "array (Positive range <>) of GLint;");
      Put_Line ("   type ColorF is new GLfloat;");
      Put_Line ("   type Texture is new GLuint;");
      Put_Line ("   type BufferOffset is access GLint;");

      for Group of Document.Group_List loop

         declare
            First : Boolean := True;
            Skip  : Boolean := False;
            Ids   : GL_Constant_Lists.List;
            Prev  : Constant_Value := 0;
            Name  : constant String := -Group.Name;
            Ada_Name : constant String := To_Ada_Command_Name (Name);
         begin

            for Id of Group.Ids loop
               if Feature.Enum_Set.Contains (Id) then
                  Ids.Append
                    (GL_Constant_Name'
                       (Ada.Strings.Unbounded.To_Unbounded_String (Id),
                        (if Document.Constant_Map.Contains (Id)
                         then Document.Constant_Map.Element (Id)
                         else 0)));
               end if;
            end loop;

            if not Ids.Is_Empty then

               Constant_Name_Sorting.Sort (Ids);

               New_Line;
               Put ("   type "
                    & Ada_Name
                    & " is");

               for Id of Ids loop
                  if First then
                     New_Line;
                     Put ("     (");
                  elsif Id.Value > Prev then
                     Put_Line (",");
                     Put ("      ");
                  else
                     Skip := True;
                  end if;

                  if Skip then
                     Skip := False;
                  else
                     Put (-Id.Name);
                     Prev := Id.Value;
                  end if;

                  First := False;
               end loop;
               Put_Line (");");

               if Group.Bit_Mask then
                  Put_Line ("   type "
                            & Ada_Name
                            & "_Array is array (Positive range <>) of "
                            & Ada_Name
                            & ";");
               end if;

               if Binding = C then
                  First := True;
                  Skip := False;

                  New_Line;

                  Put ("   for "
                       & Ada_Name
                       & " use");

                  for Id of Ids loop
                     if First then
                        New_Line;
                        Put ("     (");
                     elsif Id.Value > Prev then
                        Put_Line (",");
                        Put ("      ");
                     else
                        Skip := True;
                     end if;

                     if Skip then
                        Skip := False;
                     else
                        Put (-Id.Name);
                        Put (" =>");
                        Put (Constant_Value'Image (Id.Value));
                     end if;

                     Prev := Id.Value;

                     First := False;
                  end loop;
                  Put_Line (");");
               end if;
            end if;
         end;
      end loop;
   end Generate_GL_Types;

   -------------------------
   -- To_Ada_Command_Name --
   -------------------------

   function To_Ada_Command_Name
     (Command_Name : String)
      return String
   is
      use Ada.Characters.Handling;
      Skipping    : Boolean := True;
      Upper       : Boolean := False;
      Number      : Boolean := False;
      Result      : String (1 .. Command_Name'Length * 2);
      Length      : Natural := 0;

      procedure Add (Ch : Character);

      ---------
      -- Add --
      ---------

      procedure Add (Ch : Character) is
      begin
         Length := Length + 1;
         Result (Length) := Ch;
      end Add;

   begin
      for Ch of Command_Name loop
         if (not Number and then not Upper and then Is_Upper (Ch))
           or else Is_Digit (Ch)
         then
            Number := Is_Digit (Ch);
            Upper := Is_Upper (Ch);
            if Skipping then
               Add (Ch);
               Skipping := False;
            else
               Add ('_');
               Add (Ch);
            end if;
         elsif not Skipping then
            Add (Ch);
            Number := Is_Digit (Ch);
            Upper := Is_Upper (Ch);
         end if;
      end loop;
      return Result (1 .. Length);
   end To_Ada_Command_Name;

   ---------------------------
   -- To_Ada_Parameter_Name --
   ---------------------------

   function To_Ada_Parameter_Name
     (Parameter_Name : String)
      return String
   is
      First : constant Character := Parameter_Name (Parameter_Name'First);
      Rest  : constant String :=
                Parameter_Name
                  (Parameter_Name'First + 1 .. Parameter_Name'Last);
   begin
      return Ada.Characters.Handling.To_Upper (First) & Rest;
   end To_Ada_Parameter_Name;

   ---------------------------
   -- To_WebGL_Command_Name --
   ---------------------------

   function To_WebGL_Command_Name
     (Command_Name : String)
      return String
   is
   begin
      if Command_Name
        (Command_Name'First .. Command_Name'First + 1)
        = "gl"
      then
         return Ada.Characters.Handling.To_Lower
           (Command_Name (Command_Name'First + 2))
           & Command_Name
           (Command_Name'First + 3 .. Command_Name'Last);
      else
         return Command_Name;
      end if;
   end To_WebGL_Command_Name;

end Rho.GL_API.Generator;
