with Rho.Rendering.GL_Renderer;

package body Rho.Rendering is

   ---------------------
   -- Create_Renderer --
   ---------------------

   function Create_Renderer
     (Name : String)
      return Rho_Renderer
   is
   begin
--        if Name = "gtk-cairo" then
--           return Rho.Rendering.Gtk_Renderer.Create_Gtk_Renderer;
--        end if;

      if Name = "gl-cairo" then
         return Rho.Rendering.GL_Renderer.Create_GL_Renderer;
      end if;

      raise Constraint_Error with Name & ": unknown renderer";

   end Create_Renderer;

end Rho.Rendering;
