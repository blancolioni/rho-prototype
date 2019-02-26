with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Rho.Paths;

with Rho.Shaders.Program;

package body Rho.Shaders.Loader is

   ------------------------
   -- Create_From_Source --
   ------------------------

   function Create_From_Source
     (Context     : not null access Rho.Context.Rho_Context_Record'Class;
      Shader      : Rho_Shader_Type;
      Source_Text : String)
      return Rho.Shaders.Shader.Rho_Shader
   is
   begin
      return Rho.Shaders.Shader.Create_Shader
        (Context.Renderer, Shader, Source_Text);
   end Create_From_Source;

   ----------
   -- Load --
   ----------

   function Load
     (Context     : not null access Rho.Context.Rho_Context_Record'Class;
      Path        : String)
      return Rho.Shaders.Shader.Rho_Shader
   is
      Extension : constant String :=
                    Ada.Directories.Extension (Path);
   begin
      if Extension = "frag" then
         return Load (Context, Fragment_Shader, Path);
      elsif Extension = "vert" then
         return Load (Context, Vertex_Shader, Path);
      else
         raise Constraint_Error with
           "cannot determine shader type from extension: ." & Extension;
      end if;
   end Load;

   ----------
   -- Load --
   ----------

   function Load
     (Context              : not null access
        Rho.Context.Rho_Context_Record'Class;
      Vertex_Shader_Path   : String;
      Fragment_Shader_Path : String)
      return Rho.Shaders.Rho_Shader
   is
      Vertex_Shader : constant Rho.Shaders.Shader.Rho_Shader :=
                        Load (Context, Rho.Shaders.Vertex_Shader,
                              Vertex_Shader_Path);
      Fragment_Shader : constant Rho.Shaders.Shader.Rho_Shader :=
                          Load (Context, Rho.Shaders.Fragment_Shader,
                                Fragment_Shader_Path);
   begin
      return Rho.Shaders.Rho_Shader
        (Rho.Shaders.Program.Create
           (Context  => Context,
            Shaders  => (Vertex_Shader.Id, Fragment_Shader.Id)));
   end Load;

   ----------
   -- Load --
   ----------

   function Load
     (Context     : not null access Rho.Context.Rho_Context_Record'Class;
      Shader      : Rho_Shader_Type;
      Path     : String)
      return Rho.Shaders.Shader.Rho_Shader
   is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;

      File   : File_Type;
      Buffer : Unbounded_String;
   begin
      Open (File, In_File, Path);

      while not End_Of_File (File) loop
         Buffer := Buffer & Get_Line (File);
      end loop;

      Close (File);

      return Create_From_Source
        (Context     => Context,
         Shader      => Shader,
         Source_Text => To_String (Buffer));
   end Load;

   --------------------------
   -- Load_Standard_Shader --
   --------------------------

   function Load_Standard_Shader
     (Context     : not null access Rho.Context.Rho_Context_Record'Class;
      Name        : String)
      return Rho_Shader
   is
      Vertex   : constant Rho.Shaders.Shader.Rho_Shader :=
                   Load (Context,
                         Rho.Paths.Config_File ("shaders/" & Name & ".vert"));
      Fragment : constant Rho.Shaders.Shader.Rho_Shader :=
                   Load (Context,
                         Rho.Paths.Config_File ("shaders/" & Name & ".frag"));
   begin
      return Rho_Shader
        (Rho.Shaders.Program.Create (Context, (Vertex.Id, Fragment.Id)));
   end Load_Standard_Shader;

end Rho.Shaders.Loader;
