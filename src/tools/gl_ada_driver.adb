with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash_Case_Insensitive;
with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Text_IO;
with Tropos.Reader;
with Rho.Paths;

procedure GL_Ada_Driver is
   use Ada.Text_IO;

   Debug    : constant Boolean := False;
   Checking : constant Boolean := True;

   Output_Directory : constant String := "src/rendering/opengl/";

   package Type_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => String,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   Type_Map : Type_Maps.Map;

   GL_Config : Tropos.Configuration :=
                 Tropos.Reader.Read_Config
                   (Rho.Paths.Config_Path & "/gl/gl_4.3.txt");
   Spec_File, Body_File : File_Type;

   function Ada_Name
     (Config : Tropos.Configuration)
      return String;

   function Ada_Type
     (Name : String)
      return String;

   function C_Name
     (Config : Tropos.Configuration)
      return String;

   procedure Write_Spec
     (Config : Tropos.Configuration;
      File   : File_Type;
      For_Type : Boolean);

   function To_Upper (Ch : Character) return Character
                      renames Ada.Characters.Handling.To_Upper;

   --------------
   -- Ada_Name --
   --------------

   function Ada_Name
     (Config : Tropos.Configuration)
      return String
   is
      Result : String := Config.Config_Name;
      First  : Boolean := True;
   begin
      for I in Result'Range loop
         if First then
            Result (I) := To_Upper (Result (I));
            First := False;
         elsif Result (I) = '_' then
            First := True;
         end if;
      end loop;
      return Result;
   end Ada_Name;

   --------------
   -- Ada_Type --
   --------------

   function Ada_Type
     (Name : String)
      return String
   is
      use Type_Maps;
      Position : constant Cursor := Type_Map.Find (Name);
   begin
      if Has_Element (Position) then
         return Element (Position);
      else
         return Name;
      end if;
   end Ada_Type;

   ------------
   -- C_Name --
   ------------

   function C_Name
     (Config : Tropos.Configuration)
      return String
   is
   begin
      if Config.Contains ("c") then
         return Config.Get ("c");
      else
         declare
            Result : String := Config.Config_Name;
            Length : Natural := 0;
            First  : Boolean := True;
         begin
            for I in Result'Range loop
               if Result (I) = '_' then
                  First := True;
               else
                  Length := Length + 1;
                  if First then
                     Result (Length) := To_Upper (Result (I));
                     First := False;
                  else
                     Result (Length) := Result (I);
                  end if;
               end if;
            end loop;
            return "gl" & Result (1 .. Length);
         end;
      end if;
   end C_Name;

   ----------------
   -- Write_Spec --
   ----------------

   procedure Write_Spec
     (Config   : Tropos.Configuration;
      File     : File_Type;
      For_Type : Boolean)
   is
      Is_Function      : constant Boolean :=
                           Config.Contains ("return");
      First_Argument   : Boolean := True;
      Longest_Arg_Name : Natural := 0;
      Indent_Length    : constant Positive :=
                           (if For_Type then 8 else 3);
      Indent           : constant String (1 .. Indent_Length) :=
                           (others => ' ');
   begin
      Put (File,
           Indent
           & (if Is_Function then "function" else "procedure")
           & (if For_Type then "" else " " & Ada_Name (Config)));
      for Arg_Config of Config loop
         if Arg_Config.Config_Name /= "return"
           and then Arg_Config.Config_Name /= "c"
         then
            declare
               Arg_Name : constant String := Ada_Name (Arg_Config);
            begin
               if Arg_Name'Length > Longest_Arg_Name then
                  Longest_Arg_Name := Arg_Name'Length;
               end if;
            end;
         end if;
      end loop;

      for Arg_Config of Config loop
         if Arg_Config.Config_Name /= "return"
           and then Arg_Config.Config_Name /= "c"
         then
            if First_Argument then
               New_Line (File);
               Put (File, Indent & "  (");
               First_Argument := False;
            else
               Put_Line (File, ";");
               Put (File, Indent & "   ");
            end if;
            declare
               Name : constant String := Ada_Name (Arg_Config);
               Spaces : constant String
                 (1 .. Longest_Arg_Name - Name'Length) :=
                      (others => ' ');
            begin
               Put (File, Name);
               Put (File, Spaces);
            end;
            Put (File, " : ");
            Put (File, Ada_Type (Arg_Config.Value));
         end if;
      end loop;

      if not First_Argument then
         Put (File, ")");
      end if;
      if Is_Function then
         New_Line (File);
         Put (File, Indent & "  return " & Ada_Type (Config.Get ("return")));
      end if;
   end Write_Spec;

begin

   Type_Map.Insert ("access_float", "access GLfloat");
   Type_Map.Insert ("access_int", "access Int");
   Type_Map.Insert ("access_sizei", "access Sizei");
   Type_Map.Insert ("access_string",
                    "access Interfaces.C.Strings.chars_ptr");
   Type_Map.Insert ("access_uint", "access Uint");
   Type_Map.Insert ("address", "System.Address");
   Type_Map.Insert ("bit_mask", "Bit_Mask");
   Type_Map.Insert ("boolean", "GLboolean");
   Type_Map.Insert ("enum", "GLenum");
   Type_Map.Insert ("float", "GLfloat");
   Type_Map.Insert ("double", "GLdouble");
   Type_Map.Insert ("int", "Int");
   Type_Map.Insert ("sizei", "Sizei");
   Type_Map.Insert ("sizeiptr", "Sizeiptr");
   Type_Map.Insert ("storage_offset",
                    "System.Storage_Elements.Storage_Offset");
   Type_Map.Insert ("string", "Interfaces.C.Strings.chars_ptr");
   Type_Map.Insert ("uint", "Uint");

   Create (Spec_File, Out_File, Output_Directory & "gl.ads");
   Create (Body_File, Out_File, Output_Directory & "gl.adb");

   Put_Line (Spec_File, "with System.Storage_Elements;");
   Put_Line (Spec_File, "with Interfaces.C.Strings;");
   Put_Line (Spec_File, "with GL_Types; use GL_Types;");
   New_Line (Spec_File);
   Put_Line (Spec_File, "package GL is");
   New_Line (Spec_File);

   if Debug then
      Put_Line (Body_File, "with Ada.Text_IO;");
      New_Line (Body_File);
   end if;

   Put_Line (Body_File, "package body GL is");
   New_Line (Body_File);

   for Config of GL_Config loop

      declare
         Name   : constant String := Ada_Name (Config);
         Dashes : constant String (1 .. Name'Length + 6) := (others => '-');
      begin
         Put_Line (Body_File, "   " & Dashes);
         Put_Line (Body_File, "   -- " & Name & " --");
         Put_Line (Body_File, "   " & Dashes);
         New_Line (Body_File);
      end;

      Write_Spec (Config, Spec_File, For_Type => False);
      Put_Line (Spec_File, ";");
      New_Line (Spec_File);

      Write_Spec (Config, Body_File, For_Type => False);
      New_Line (Body_File);
      Put_Line (Body_File, "   is");
      declare
         C_Proc : constant String := C_Name (Config);
         First_Argument : Boolean := True;
      begin
         Put_Line (Body_File, "      type Lib_" & C_Proc & " is access");
         Write_Spec (Config, Body_File, For_Type => True);
         Put_Line (Body_File, ";");
         Put_Line (Body_File,
                   "      pragma Convention (C, Lib_" & C_Proc & ");");
         Put_Line (Body_File, "      " & C_Proc & " : Lib_" & C_Proc & ";");
         Put_Line (Body_File,
                   "      pragma Import (C, " & C_Proc & ",");
         Put_Line (Body_File,
                   "                     ""_ptrc_" & C_Proc & """);");
         Put_Line (Body_File, "   begin");
         if Debug then
            Put_Line (Body_File,
                      "      Ada.Text_IO.Put_Line (""" & C_Proc & """);");
         end if;
         Put (Body_File, "      ");
         if Config.Contains ("return") then
            Put (Body_File, "return ");
         end if;
         Put (Body_File, C_Proc);

         for Arg_Config of Config loop
            if Arg_Config.Config_Name /= "return"
              and then Arg_Config.Config_Name /= "c"
            then
               if First_Argument then
                  New_Line (Body_File);
                  Put (Body_File, "        (");
                  First_Argument := False;
               else
                  Put_Line (Body_File, ",");
                  Put (Body_File, "         ");
               end if;
               Put (Body_File, Ada_Name (Arg_Config));
            end if;
         end loop;

         if not First_Argument then
            Put (Body_File, ")");
         else
            Put (Body_File, ".all");
         end if;
         Put_Line (Body_File, ";");

         if Checking and then not Config.Contains ("return") then
            Put_Line (Body_File, "      if Get_Error /= 0 then");
            Put_Line (Body_File, "         raise Program_Error;");
            Put_Line (Body_File, "      end if;");
         end if;
      end;

      Put_Line (Body_File, "   end " & Ada_Name (Config) & ";");
      New_Line (Body_File);

   end loop;

   Put_Line (Spec_File, "end GL;");
   Put_Line (Body_File, "end GL;");

   Close (Body_File);
   Close (Spec_File);

end GL_Ada_Driver;
