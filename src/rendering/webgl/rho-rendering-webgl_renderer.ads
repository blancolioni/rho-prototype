with Gnoga.Gui.Element;

private with Rho.Mouse;

package Rho.Rendering.WebGL_Renderer is

   function Create_WebGL_Renderer
     (Parent : in out Gnoga.Gui.Element.Element_Type'Class)
      return Rho_Renderer;

private

   type Built_In_Shader is
     (Pass_Through_Shader,
      Texture_Shader);

end Rho.Rendering.WebGL_Renderer;
