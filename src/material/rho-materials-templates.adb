with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

package body Rho.Materials.Templates is

   package body Material_Template_Caches is

      function To_Key (Template : Rho_Material_Template'Class) return String;

      ---------
      -- Get --
      ---------

      function Get (From     : Cache;
                    Template : Rho_Material_Template'Class)
                    return Element_Access
      is
         Key : constant String := To_Key (Template);
      begin
         if From.Map.Contains (Key) then
            return From.Map.Element (Key);
         else
            return null;
         end if;
      end Get;

      ------------
      -- Insert --
      ------------

      procedure Insert (To       : in out Cache;
                        Template : Rho_Material_Template'Class;
                        Value    : Element_Access)
      is
         Key : constant String := To_Key (Template);
      begin
         To.Map.Insert (Key, Value);
      end Insert;

      ------------
      -- To_Key --
      ------------

      function To_Key (Template : Rho_Material_Template'Class) return String is
         use Ada.Strings.Unbounded;
         use Value_Maps;
         Key : Unbounded_String;
         Position : Cursor := Template.Values.First;
      begin
         while Has_Element (Position) loop
            if Key /= Null_Unbounded_String then
               Key := Key & ";";
            end if;
            Key := Key & Value_Maps.Key (Position) & "="
              & Value_Maps.Element (Position);
            Next (Position);
         end loop;
         return To_String (Key);
      end To_Key;

   end Material_Template_Caches;

   ----------------
   -- Apply_Line --
   ----------------

   function Apply_Line
     (Template    : Rho_Material_Template;
      Shader_Line : String)
      return String
   is
      Value_Start : Natural := 0;
      Line : constant String := Shader_Line & ' ';
   begin
      for I in Line'Range loop
         if Line (I) = '$' then
            if Value_Start /= 0 then
               Value_Start := 0;
            else
               Value_Start := I;
            end if;
         elsif not Ada.Characters.Handling.Is_Alphanumeric (Line (I))
           and then Line (I) /= '-'
           and then Line (I) /= '_'
           and then Value_Start > 0
         then
            declare
               Key : constant String :=
                       Line (Value_Start + 1 .. I - 1);
               New_Text : constant String :=
                            (if Template.Values.Contains (Key)
                             then Template.Values (Key)
                             else "");
            begin
               return Line (Line'First .. Value_Start - 1)
                 & New_Text
                 & Template.Apply_Line (Line (I .. Line'Last));
            end;
         end if;
      end loop;

      return Shader_Line;

   end Apply_Line;

   -------------
   -- Execute --
   -------------

   function Execute
     (Template     : Rho_Material_Template'Class;
      Source_Path  : String;
      Write_Result : Boolean := False)
      return String
   is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      File : File_Type;
      Result : Unbounded_String;
      Line_Number : Positive := 1;
   begin
      Open (File, In_File, Source_Path);
      while not End_Of_File (File) loop
         declare
            Old_Line : constant String := Get_Line (File);
         begin
            if Old_Line'Length > 2
              and then Old_Line (1 .. 2) = "@$"
            then
               declare
                  Indexed_Name : constant String :=
                                   Ada.Strings.Fixed.Trim
                                     (Old_Line (3 .. Old_Line'Last),
                                      Ada.Strings.Both);
                  Index        : Positive := 1;
               begin
                  loop
                     declare
                        Key : constant String :=
                                Indexed_Name & Integer'Image (-Index);
                     begin
                        exit when not Template.Values.Contains (Key);
                        if Write_Result then
                           Ada.Integer_Text_IO.Put (Line_Number, 4);
                           Ada.Text_IO.Put_Line
                             (": " & Template.Values (Key));
                        end if;
                        Line_Number := Line_Number + 1;

                        Result :=
                          Result & Template.Values (Key)
                          & Character'Val (10);
                        Index := Index + 1;
                     end;
                  end loop;
               end;
            else
               declare
                  Filtered : constant String :=
                               Template.Filter_Line (Old_Line);
                  Applied  : constant String :=
                               Template.Apply_Line (Filtered);
               begin
                  if Applied /= "" then
                     if Write_Result then
                        Ada.Integer_Text_IO.Put (Line_Number, 4);
                        Ada.Text_IO.Put_Line (": " & Applied);
                     end if;
                     Line_Number := Line_Number + 1;
                     Result := Result & Applied & Character'Val (10);
                  end if;
               end;
            end if;
         end;
      end loop;
      Close (File);
      return To_String (Result);
   exception
      when Name_Error =>
         raise Program_Error with
           "unable to locate template source: "
           & Source_Path;
   end Execute;

   -----------------
   -- Filter_Line --
   -----------------

   function Filter_Line
     (Template : Rho_Material_Template;
      Line     : String)
      return String
   is
      Start : Positive := 2;
   begin
      if Line'Length = 0 or else Line (Line'First) /= '[' then
         return Line;
      else
         for I in Line'Range loop
            if Line (I) = ' ' or else Line (I) = ']' then
               if not Template.Values.Contains
                 (Line (Start .. I - 1))
               then
                  return "";
               elsif Line (I) = ']' then
                  return Template.Apply_Line
                    (Line (I + 1 .. Line'Last));
               else
                  Start := I + 1;
               end if;
            end if;
         end loop;
         return Line;
      end if;
   end Filter_Line;

   --------------
   -- Set_Flag --
   --------------

   procedure Set_Flag
     (Template : in out Rho_Material_Template'Class;
      Name     : String;
      Value    : Boolean)
   is
   begin
      if Value then
         Template.Set_Value (Name, Name);
      else
         Template.Set_Value ("no-" & Name, Name);
      end if;
   end Set_Flag;

   -----------------------
   -- Set_Indexed_Value --
   -----------------------

   procedure Set_Indexed_Value
     (Template : in out Rho_Material_Template'Class;
      Name     : String;
      Index    : Positive;
      Value    : String)
   is
   begin
      Template.Set_Value
        (Name  => Name & Integer'Image (-Index),
         Value => Value);
   end Set_Indexed_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Template : in out Rho_Material_Template'Class;
      Name     : String;
      Value    : String)
   is
   begin
      if Template.Values.Contains (Name) then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "warning: template value "
            & Name
            & " overridden");
         Template.Values.Replace (Name, Value);
      else
         Template.Values.Insert (Name, Value);
      end if;
   end Set_Value;

end Rho.Materials.Templates;
