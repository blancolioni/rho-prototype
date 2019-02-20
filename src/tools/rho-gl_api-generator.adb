with Ada.Characters.Handling;
with Ada.Text_IO;

package body Rho.GL_API.Generator is

   Check_Errors : constant Boolean := False;

   function Less (Left, Right : GL_Constant_Name) return Boolean
   is (Left.Value < Right.Value);

   package Constant_Name_Sorting is
     new GL_Constant_Lists.Generic_Sorting (Less);

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

            Put_Line ("   procedure Perspective");
            Put_Line
              ("     (Context       : in out Context_WebGL_Type'Class;");
            Put_Line ("      Matrix        : out Matrix_4;");
            Put_Line ("      Field_Of_View : GLfloat;");
            Put_Line ("      Aspect_Ratio  : GLfloat;");
            Put_Line ("      Near, Far     : GLfloat);");
            New_Line;

            Put_Line ("   procedure Translate");
            Put_Line ("     (Context : in out Context_WebGL_Type'Class;");
            Put_Line ("      Matrix  : in out Matrix_4;");
            Put_Line ("      X, Y, Z : GLfloat);");
            New_Line;

            Put_Line ("   procedure Uniform_Matrix");
            Put_Line ("     (Context  : in out Context_WebGL_Type'Class;");
            Put_Line ("      Location : GLuint;");
            Put_Line ("      Matrix   : Matrix_4);");
            New_Line;

         end if;
      end if;

      if Generate_Body then
         Put_Line ("   function GLEnum_Property");
         Put_Line ("     (Context : Context_WebGL_Type'Class;");
         Put_Line ("      Name    : String)");
         Put_Line ("     return String");
         Put_Line ("   is (if Name = ""TRUE"" then ""true""");
         Put_Line ("       elsif Name = ""FALSE"" then ""false""");
         Put_Line ("       else Context.Property "
                   & "(Name (Name'First + 3 .. Name'Last)));");

         New_Line;
         Put_Line ("   procedure Get_Drawing_Context_WebGL");
         Put_Line ("     (Context : in out Context_WebGL_Type;");
         Put_Line ("      Canvas  : in out Canvas_Type'Class)");
         Put_Line ("   renames Support.Get_Drawing_Context_WebGL;");
         New_Line;

         Put_Line ("   procedure Perspective");
         Put_Line ("     (Context       : in out Context_WebGL_Type'Class;");
         Put_Line ("      Matrix        : out Matrix_4;");
         Put_Line ("      Field_Of_View : GLfloat;");
         Put_Line ("      Aspect_Ratio  : GLfloat;");
         Put_Line ("      Near, Far     : GLfloat)");
         Put_Line ("   renames Support.Perspective;");
         New_Line;

         Put_Line ("   procedure Translate");
         Put_Line ("     (Context : in out Context_WebGL_Type'Class;");
         Put_Line ("      Matrix  : in out Matrix_4;");
         Put_Line ("      X, Y, Z : GLfloat)");
         Put_Line ("   renames Support.Translate;");
         New_Line;

         Put_Line ("   procedure Uniform_Matrix");
         Put_Line ("     (Context  : in out Context_WebGL_Type'Class;");
         Put_Line ("      Location : GLuint;");
         Put_Line ("      Matrix   : Matrix_4)");
         Put_Line ("   renames Support.Uniform_Matrix;");
         New_Line;

      end if;

      if Generate_Body then
         for Group of Document.Group_List loop

            declare
               First    : Boolean := True;
               Skip     : Boolean := False;
               Ids      : GL_Constant_Lists.List;
               Prev     : Constant_Value := 0;
               Name     : constant String := -Group.Name;
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

                  Put ("   " & Ada_Name & "_Values : constant array ("
                       & Ada_Name & ") of GLuint := ");

                  for Id of Ids loop
                     if First then
                        Put_Line ("(");
                     elsif Id.Value > Prev
                       and then Id.Value < 2 ** 32
                     then
                        Put_Line (",");
                     else
                        Skip := True;
                     end if;

                     if Skip then
                        Skip := False;
                     else
                        Put ("      ");
                        Put (-Id.Name);
                        Put (" =>");
                        Put (Id.Value'Image);
                        Prev := Id.Value;
                     end if;

                     First := False;
                  end loop;
                  Put_Line (");");

               end if;
            end;
         end loop;

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
            Have_Float_Data_Array  : Boolean := False;
            Float_Data_Array_Param : Parameter_Record;
            Have_Group_Mask  : Boolean := False;
            Group_Parameter  : Parameter_Record;
            Group_Mask       : Group_Record;
         begin
            if (Feature.Command_Set.Contains (Name)
                or else Command.API_Override (Binding))
              and then (Is_Procedure
                        or else Return_Type = "GLint"
                        or else Return_Type = "GLuint"
                        or else Return_Type = "Boolean")
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

                     if Parameter.Float_Data_Array then
                        Have_Float_Data_Array := True;
                        Float_Data_Array_Param := Parameter;
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
                     if Parameter.Float_Data_Array then
                        Put ("Float_Array");
                     elsif Parameter.Byte_Offset then
                        Put ("Natural");
                     elsif Parameter.Writeable_Array then
                        Put ("GLvoidptr");
                     elsif Parameter_Group = "Boolean" then
                        Put (Parameter_Group);
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

                  if Have_Float_Data_Array then
                     Put_Line
                       ("      use Ada.Strings.Unbounded;");
                  end if;

                  if Check_Errors then
                     Put_Line
                       ("      Error : Natural;");
                  end if;

                  if Have_Float_Data_Array then
                     Put_Line
                       ("      Data_Image : Unbounded_String;");
                  end if;

                  if Have_Group_Mask then
                     Put_Line
                       ("      "
                        & To_Ada_Parameter_Name
                          (-Group_Parameter.Parameter_Name)
                        & "_Uint : GLuint := 0;");
                  end if;

                  Put_Line ("   begin");
                  if Have_Float_Data_Array then
                     Put_Line
                       ("      for X of "
                        & (-Float_Data_Array_Param.Parameter_Name)
                        & " loop");
                     Put_Line
                       ("         if Data_Image /= """" then");
                     Put_Line
                       ("            Data_Image := Data_Image & "","";");
                     Put_Line
                       ("         end if;");
                     Put_Line
                       ("         Data_Image := Data_Image & "
                        & "Support.Image (X);");
                     Put_Line
                       ("      end loop;");
                  end if;

                  if Have_Group_Mask then
                     declare
                        Mask_Name     : constant String := -Group_Mask.Name;
                        Ada_Mask_Name : constant String :=
                                          To_Ada_Command_Name (Mask_Name);
                        Mask_Acc_Name : constant String :=
                                          To_Ada_Parameter_Name
                                            (-Group_Parameter.Parameter_Name)
                                          & "_Uint";
                     begin
                        Put_Line
                          ("      for X of "
                           & To_Ada_Parameter_Name
                             (-Group_Parameter.Parameter_Name)
                           & " loop");
                        Put_Line
                          ("         " & Mask_Acc_Name & " := "
                           & Mask_Acc_Name & " or "
                           & Ada_Mask_Name & "_Values (X);");
                        Put_Line
                          ("      end loop;");
                     end;
                  end if;

                  Put ("      ");
                  if not Is_Procedure then
                     Put_Line ("declare");
                     Put_Line ("         Result : constant "
                               & Return_Type & " := ");
                     Put ("           ");

                     if Command.Creates_Object then
                        Put ("Support.Indexed_Javascript_Object (Context,");
                     else
                        Put (Return_Type & "'Value (Context.Execute (");
                     end if;
                  else
                     Put ("Context.Execute (");
                  end if;
                  New_Line;
                  Put_Line
                    ("        """ & To_WebGL_Command_Name (Name)
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
                        Get_GLEnum_Property : constant Boolean :=
                                                Parameter_Group /= ""
                                       and then Parameter_Group /= "ColorF"
                                       and then Parameter_Type /= "GLfloat"
                                       and then Parameter_Type /= "GLint"
                                       and then Parameter_Type /= "GLintptr"
                                       and then Parameter_Type /= "GLuint";
                        String_Constant     : constant Boolean :=
                                             Parameter_Type = "String";
                        Float_Argument      : constant Boolean :=
                                                Parameter_Type = "GLfloat";
                        Have_Group          : constant Boolean :=
                                                Parameter_Group /= ""
                                       and then Parameter_Group /= "Texture"
                                       and then Document.Group_Map.Contains
                                                          (Parameter_Group);
                     begin
                        if First_Parameter then
                           First_Parameter := False;
                        else
                           Put_Line (" & "",""");
                        end if;
                        Put ("         & ");

                        if Parameter.Float_Data_Array then
                           Put
                             ("""new Float32Array(["" & "
                              & "To_String (Data_Image) & ""])""");
                        elsif Have_Group_Mask
                          and then Parameter.Parameter_Name
                            = Group_Parameter.Parameter_Name
                        then
                           Put (To_Ada_Parameter_Name
                                (-Group_Parameter.Parameter_Name)
                                & "_Uint'Image");
                        else
                           if Have_Group then
                              Put (To_Ada_Command_Name (Parameter_Group)
                                   & "_Values (");
                           elsif Get_GLEnum_Property then
                              Put ("GLEnum_Property (Context, ");
                           elsif String_Constant then
                              Put ("""'"" & Escape_Quotes (");
                           elsif Parameter.Object_Reference then
                              Put ("Support.Indexed_Object_Reference "
                                   & "(Context, ");
                           elsif Float_Argument then
                              Put ("Support.Image (");
                           end if;

                           Put (To_Ada_Parameter_Name (Parameter_Name));

                           if String_Constant then
                              Put (") & ""'""");
                           elsif Parameter.Object_Reference
                             or else Float_Argument
                           then
                              Put (")");
                           elsif Have_Group then
                              Put (")'Image");
                           else
                              Put ("'Image");
                              if Get_GLEnum_Property then
                                 Put (")");
                              end if;
                           end if;
                        end if;
                     end;
                  end loop;
                  Put ("        & "")"")");
                  if not Is_Procedure
                    and then not Command.Creates_Object
                  then
                     Put (")");
                  end if;
                  Put_Line (";");
                  if not Is_Procedure then
                     Put_Line ("      begin");
                  end if;

                  if Check_Errors then
                     Put_Line ("      Error := Natural'Value "
                               & "(Context.Execute ("
                               & """getError()""));");
                     Put_Line ("      if Error /= 0 then");
                     Put_Line ("         raise Program_Error with """
                               & Name & ": error"" & Error'Image;");
                     Put_Line ("      end if;");
                  end if;

                  if not Is_Procedure then
                     Put_Line ("      return Result;");
                     Put_Line ("   end;");
                  end if;

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
            Put_Line ("   package Web_Object_Id_Vectors is");
            Put_Line ("     new Ada.Containers.Indefinite_Vectors");
            Put_Line ("       (Positive, String);");
            New_Line;
            Put_Line
              ("   type Context_WebGL_Type is"
               & " new Context_Type with");
            Put_Line
              ("      record");
            Put_Line
              ("         Web_Object_Ids : Web_Object_Id_Vectors.Vector;");
            Put_Line
              ("         View_Width     : Natural;");
            Put_Line
              ("         View_Height    : Natural;");
            Put_Line
              ("      end record;");
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
     (Document      : GL_API_Document'Class;
      Binding       : API_Binding_Language;
      Feature_Key   : String)
   is
      use Ada.Text_IO;

      Feature : Require_Record renames Document.Feature_Map (Feature_Key);

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

      Put_Line ("   package Matrices is");
      Put_Line ("     new Ada.Numerics.Generic_Real_Arrays (GLfloat);");
      New_Line;

      Put_Line ("   subtype Matrix_4 is");
      Put_Line ("     Matrices.Real_Matrix (1 .. 4, 1 .. 4);");
      New_Line;

      Put_Line ("   subtype Vector_4 is");
      Put_Line ("     Matrices.Real_Vector (1 .. 4);");
      New_Line;

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
      if Result (Length) = 'f' then
         Length := Length - 1;
      end if;
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
      First : Positive := Command_Name'First;
      Last  : Positive := Command_Name'Last;
   begin
      if Command_Name (First .. First + 1) = "gl" then
         First := First + 2;
      end if;
      if Command_Name (Last) = 'f' then
         Last := Last - 1;
      end if;

      return Ada.Characters.Handling.To_Lower (Command_Name (First))
        & Command_Name (First + 1 .. Last);
   end To_WebGL_Command_Name;

end Rho.GL_API.Generator;
