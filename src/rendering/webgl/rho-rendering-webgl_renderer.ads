with Gnoga.Gui.Element;

private with Rho.Mouse;

with Rho.Handles;

package Rho.Rendering.WebGL_Renderer is

   function Create_WebGL_Renderer
     (Parent : in out Gnoga.Gui.Element.Element_Type'Class)
      return Rho_Renderer;

   function WebGL_Event_Source return Rho.Handles.Render_Event_Access;

private

   type Built_In_Shader is
     (Pass_Through_Shader,
      Texture_Shader);

   task type Render_Task_Type is
        new Rho.Handles.Render_Event_Source with
      entry Run (Handle : Rho.Handles.Rho_Handle);
      entry Stop;
   end Render_Task_Type;

end Rho.Rendering.WebGL_Renderer;
