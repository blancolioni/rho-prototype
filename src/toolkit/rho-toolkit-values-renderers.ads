with Rho.Toolkit.Widget;

package Rho.Toolkit.Values.Renderers is

   type Rho_Value_Renderer_Interface is interface;

   subtype Rho_Value_Renderer is Rho_Value_Renderer_Interface'Class;

   function Render
     (Renderer : Rho_Value_Renderer_Interface;
      Value    : Rho_Value_Interface'Class)
      return Rho.Toolkit.Widget.Rho_Widget
      is abstract;

   procedure Update
     (Renderer : Rho_Value_Renderer_Interface;
      Widget   : not null access
        Rho.Toolkit.Widget.Rho_Widget_Record'Class;
      Value    : Rho_Value_Interface'Class)
   is abstract;

   function Text_Renderer return Rho_Value_Renderer;

end Rho.Toolkit.Values.Renderers;
