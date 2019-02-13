with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Interfaces.C.Strings;

with GL;
with GL_Constants;
with GL_Types;

with Rho.Paths;

package body Rho.Shader.Load is

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   Search_Paths : String_Lists.List;

   function Create_From_Source
     (Source_Text : String;
      Shader_Type : Rho_Sub_Shader_Type)
      return Rho_Sub_Shader;

   function Load
     (Path        : String;
      Shader_Type : Rho_Sub_Shader_Type)
      return Rho_Sub_Shader;

   function Find_File
     (File_Name : String;
      Extension : String)
      return String;

   ---------------------
   -- Add_Search_Path --
   ---------------------

   procedure Add_Search_Path
     (Path : String)
   is
   begin
      if Path (Path'Last) = '/' then
         Search_Paths.Append (Path);
      else
         Search_Paths.Append (Path & '/');
      end if;
   end Add_Search_Path;

   ------------------------
   -- Create_From_Source --
   ------------------------

   function Create_From_Source
     (Source_Text : String;
      Shader_Type : Rho_Sub_Shader_Type)
      return Rho_Sub_Shader
   is
      use GL, GL_Constants, GL_Types;
      Shader : constant Rho_Sub_Shader := new Rho_Sub_Shader_Record;
   begin

      Shader.Id :=
        Create_Shader ((case Shader_Type is
                          when Vertex   => GL_VERTEX_SHADER,
                          when Fragment => GL_FRAGMENT_SHADER));

      Shader_Source
        (Shader => Shader.Id,
         Count  => 1,
         Source => Source_Text);

      Compile_Shader (Shader.Id);

      declare
         Result     : constant Int := Get_Compile_Status (Shader.Id);
         Log_Length : aliased Int;
      begin
--           Get_Shader (Shader.Id, GL_COMPILE_STATUS, Result'Access);
         Shader.Error := Result = 0;

         if Shader.Error then
            Get_Shader (Shader.Id, GL_INFO_LOG_LENGTH, Log_Length'Access);
            declare
               Log : constant Interfaces.C.Strings.char_array_access :=
                       new Interfaces.C.char_array
                         (1 .. Interfaces.C.size_t (Log_Length));
            begin
               Get_Shader_Info_Log (Shader.Id, Sizei (Log_Length), null,
                                    Interfaces.C.Strings.To_Chars_Ptr
                                      (Log));
               Shader.Log :=
                 Ada.Strings.Unbounded.To_Unbounded_String
                   (Interfaces.C.To_Ada (Log.all));
               Ada.Text_IO.Put_Line
                 (Ada.Strings.Unbounded.To_String (Shader.Log));
            end;
            Ada.Text_IO.Put_Line ("Load failed");
         end if;
      end;

      return Shader;
   end Create_From_Source;

   ------------------------
   -- Create_From_Source --
   ------------------------

   function Create_From_Source
     (Source_Text : String)
      return Rho_Vertex_Shader
   is
   begin
      return Rho_Vertex_Shader (Create_From_Source (Source_Text, Vertex));
   end Create_From_Source;

   ------------------------
   -- Create_From_Source --
   ------------------------

   function Create_From_Source
     (Source_Text : String)
      return Rho_Fragment_Shader
   is
   begin
      return Rho_Fragment_Shader (Create_From_Source (Source_Text, Fragment));
   end Create_From_Source;

   ---------------
   -- Find_File --
   ---------------

   function Find_File
     (File_Name : String;
      Extension : String)
      return String
   is
   begin
      if Ada.Directories.Exists (File_Name) then
         return File_Name;
      elsif Ada.Directories.Exists (File_Name & "." & Extension) then
         return File_Name & "." & Extension;
      else
         for Path of Search_Paths loop
            if Ada.Directories.Exists (Path & File_Name) then
               return Path & File_Name;
            elsif Ada.Directories.Exists
              (Path & File_Name & "." & Extension)
            then
               return Path & File_Name & "." & Extension;
            end if;
         end loop;

         if Ada.Directories.Exists
           (Rho.Paths.Config_File ("shaders/" & File_Name))
         then
            return Rho.Paths.Config_File ("shaders/" & File_Name);
         elsif Ada.Directories.Exists
           (Rho.Paths.Config_File ("shaders/" & File_Name & "." & Extension))
         then
            return Rho.Paths.Config_File
              ("shaders/" & File_Name & "." & Extension);
         else
            raise Constraint_Error with
              "File not found: " & File_Name;
         end if;
      end if;
   end Find_File;

   ----------
   -- Load --
   ----------

   function Load
     (Path        : String;
      Shader_Type : Rho_Sub_Shader_Type)
      return Rho_Sub_Shader
   is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      Source : String_Lists.List;
   begin

--        Ada.Text_IO.Put_Line
--          ("shader: " & Path);

      declare
         File   : File_Type;
      begin
         Open (File, In_File, Path);
         while not End_Of_File (File) loop
            Source.Append (Get_Line (File));
         end loop;
         Close (File);
      end;

      declare
         Buffer : Unbounded_String;
      begin
         for S of Source loop
            Buffer := Buffer & S & Character'Val (10);
         end loop;

         return Create_From_Source (To_String (Buffer), Shader_Type);
      end;

   end Load;

   ----------
   -- Load --
   ----------

   function Load
     (Name : String;
      Ext  : String := "vert")
      return Rho_Vertex_Shader
   is
      Path   : constant String := Find_File (Name, Ext);
      Result : constant Rho_Sub_Shader :=
                 Load (Path, Vertex);
   begin
      return Rho_Vertex_Shader (Result);
   end Load;

   ----------
   -- Load --
   ----------

   function Load
     (Name : String;
      Ext  : String := "frag")
      return Rho_Fragment_Shader
   is
      Path   : constant String := Find_File (Name, Ext);
      Result : constant Rho_Sub_Shader :=
                 Load (Path, Fragment);
   begin
      return Rho_Fragment_Shader (Result);
   end Load;

   ----------
   -- Load --
   ----------

   function Load
     (Vertex_Shader_Name, Fragment_Shader_Name : String)
      return Rho_Shader
   is
      Vertex_Shader   : constant Rho_Vertex_Shader :=
                          Load (Vertex_Shader_Name);
      Fragment_Shader : constant Rho_Fragment_Shader :=
                          Load (Fragment_Shader_Name);
   begin
      return Result : constant Rho_Shader := Create do
         Result.Add (Vertex_Shader);
         Result.Add (Fragment_Shader);
         Result.Compile;
      end return;
   end Load;

   --------------------------
   -- Load_Standard_Shader --
   --------------------------

   function Load_Standard_Shader (Name : String) return Rho_Shader is
      Path : constant String := Rho.Paths.Config_Path & "/shaders/";
   begin
      return Load
        (Path & Name & ".vert", Path & Name & ".frag");
   end Load_Standard_Shader;

end Rho.Shader.Load;
