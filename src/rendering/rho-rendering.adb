with Rho.Shaders.Program;

package body Rho.Rendering is

   ---------------------
   -- Activate_Shader --
   ---------------------

   procedure Activate_Shader
     (Target : in out Rho_Renderer_Record)
   is
      use type Rho.Shaders.Rho_Shader;
   begin
      if Target.Active_Shader
        and then Target.Current_Shader /= Target.Shaders.First_Element
      then
         Target.Current_Shader.Deactivate;
         Target.Active_Shader := False;
      end if;

      if not Target.Active_Shader
        or else Target.Current_Shader
          /= Target.Shaders.First_Element
      then
         Target.Clear_Matrix_Saved;
         Target.Active_Shader := True;
         Target.Current_Shader := Target.Shaders.First_Element;
         Target.Current_Shader.Activate;
      end if;
   end Activate_Shader;

   ------------------------
   -- Clear_Matrix_Saved --
   ------------------------

   procedure Clear_Matrix_Saved
     (Target : in out Rho_Renderer_Record'Class)
   is
   begin
      Target.Matrix_Saved := (others => False);
   end Clear_Matrix_Saved;

   --------------------
   -- Current_Shader --
   --------------------

   function Current_Shader
     (Target : Rho_Renderer_Record)
      return Rho.Shaders.Rho_Shader
   is
   begin
      if Target.Shaders.Is_Empty then
         return null;
      else
         return Target.Shaders.First_Element;
      end if;
   end Current_Shader;

   ------------------
   -- Matrix_Saved --
   ------------------

   function Matrix_Saved
     (Target : Rho_Renderer_Record'Class;
      Matrix : Rho.Matrices.Matrix_Mode_Type)
      return Boolean
   is
   begin
      return Target.Matrix_Saved (Matrix);
   end Matrix_Saved;

   ----------------
   -- Pop_Shader --
   ----------------

   procedure Pop_Shader
     (Target : in out Rho_Renderer_Record)
   is
   begin
      Target.Shaders.Delete_First;
   end Pop_Shader;

   -----------------
   -- Push_Shader --
   -----------------

   procedure Push_Shader
     (Target : in out Rho_Renderer_Record;
      Shader : Rho.Shaders.Rho_Shader)
   is
   begin
      Target.Shaders.Insert (Target.Shaders.First, Shader);
   end Push_Shader;

   -----------------
   -- Save_Matrix --
   -----------------

   procedure Save_Matrix
     (Renderer : in out Rho_Renderer_Record;
      Matrix   : Rho.Matrices.Matrix_Mode_Type)
   is
      Program : constant Rho.Shaders.Program.Rho_Program :=
                  Rho.Shaders.Program.Rho_Program
                    (Renderer.Current_Shader);
   begin
      Renderer.Activate_Shader;

      Program.Uniform_Matrix_Value (Matrix).Set_Value
        (Renderer.Current (Matrix));
      Renderer.Set_Matrix_Saved (Matrix);

   end Save_Matrix;

   ----------------------
   -- Set_Matrix_Saved --
   ----------------------

   procedure Set_Matrix_Saved
     (Target : in out Rho_Renderer_Record'Class;
      Matrix : Rho.Matrices.Matrix_Mode_Type)
   is
   begin
      Target.Matrix_Saved (Matrix) := True;
   end Set_Matrix_Saved;

end Rho.Rendering;
