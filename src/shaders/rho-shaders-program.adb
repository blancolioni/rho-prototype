with Rho.Context;
with Rho.Rendering;

package body Rho.Shaders.Program is

   --------------
   -- Activate --
   --------------

   overriding procedure Activate (Shader : in out Rho_Program_Record) is
   begin
      Shader.Context.Renderer.Use_Shader (Shader.Id);
   end Activate;

   ---------
   -- Add --
   ---------

   procedure Add
     (Program : in out Rho_Program_Record'Class;
      Shader  : Rho.Shaders.Shader.Rho_Shader)
   is
   begin
      Program.Shaders.Append (Shader);
   end Add;

   ---------------------
   -- Attribute_Value --
   ---------------------

   function Attribute_Value
     (Program : not null access Rho_Program_Record'Class;
      Name    : String)
      return Rho.Shaders.Values.Rho_Attribute_Value
   is
      Renderer : constant Rho.Rendering.Rho_Renderer :=
                   Program.Context.Renderer;
      Loc      : constant Rho_Attribute_Id :=
                   Renderer.Get_Attribute_Location
                     (Program.Id, Name);
   begin
      return Rho.Shaders.Values.New_Attribute_Value
        (Program, Loc, Name);
   end Attribute_Value;

   -------------
   -- Compile --
   -------------

   procedure Compile (Shader : in out Rho_Program_Record'Class) is
      Shdrs : Shader_Array (1 .. Natural (Shader.Shaders.Length));
      Count : Natural := 0;
   begin
      for Shdr of Shdrs loop
         Count := Count + 1;
         Shdrs (Count) := Shdr;
      end loop;

      Shader.Id := Shader.Context.Renderer.Create_Program (Shdrs);
   end Compile;

   ------------
   -- Create --
   ------------

   function Create
     (Context  : not null access Rho.Context.Rho_Context_Record'Class;
      Shaders  : Rho.Shaders.Shader_Array)
      return Rho_Program
   is
      Program : constant Rho_Program := Create (Context);
   begin
      Program.Context := Context;
      Program.Id := Context.Renderer.Create_Program (Shaders);
      return Program;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Context : not null access Rho.Context.Rho_Context_Record'Class)
      return Rho_Program
   is
   begin
      return Program : constant Rho_Program := new Rho_Program_Record do
         Rho_Initialize (Program.all, Context);
      end return;
   end Create;

   ----------------
   -- Deactivate --
   ----------------

   overriding procedure Deactivate (Shader : in out Rho_Program_Record) is
   begin
      null;
   end Deactivate;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Shader   : in out Rho_Program)
   is
   begin
      null;
   end Destroy;

   --------
   -- Id --
   --------

   function Id (Program : Rho_Program_Record'Class) return Rho_Program_Id is
   begin
      return Program.Id;
   end Id;

   --------------------
   -- Rho_Initialize --
   --------------------

   procedure Rho_Initialize
     (Shader   : in out Rho_Program_Record'Class;
      Context  : not null access Rho.Context.Rho_Context_Record'Class)
   is
   begin
      Shader.Context := Context;
   end Rho_Initialize;

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New
     (Shader   : in out Rho_Program;
      Context  : not null access Rho.Context.Rho_Context_Record'Class)
   is
   begin
      Shader := Create (Context);
   end Rho_New;

   --------------------------
   -- Uniform_Matrix_Value --
   --------------------------

   function Uniform_Matrix_Value
     (Program : in out Rho_Program_Record'Class;
      Matrix  : Rho.Matrices.Matrix_Mode_Type)
      return Rho.Shaders.Values.Rho_Uniform_Value
   is
      use type Rho.Shaders.Values.Rho_Uniform_Value;
   begin
      if Program.Matrices (Matrix) = null then
         declare
            Name : constant String :=
                     (case Matrix is
                         when Rho.Matrices.Projection => "camera",
                         when Rho.Matrices.Model_View => "model",
                         when Rho.Matrices.Texture    => "texture");
         begin
            Program.Matrices (Matrix) :=
              Program.Uniform_Value (Name);
         end;
      end if;

      return Program.Matrices (Matrix);
   end Uniform_Matrix_Value;

   -------------------
   -- Uniform_Value --
   -------------------

   function Uniform_Value
     (Program : not null access Rho_Program_Record'Class;
      Name    : String)
      return Rho.Shaders.Values.Rho_Uniform_Value
   is
      Renderer : constant Rho.Rendering.Rho_Renderer :=
                   Program.Context.Renderer;
      Loc      : constant Rho_Uniform_Id :=
                   Renderer.Get_Uniform_Location
                     (Program.Id, Name);
   begin
      return Rho.Shaders.Values.New_Uniform_Value
        (Program, Loc, Name);
   end Uniform_Value;

end Rho.Shaders.Program;
