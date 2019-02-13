with Cairo;

with Rho.Color;
with Rho.Renderable;
with Rho.Viewport;

package Rho.Toolkit is

   type Rho_Top_Level_Interface is interface
     and Rho.Renderable.Rho_Renderable;

   function Viewport
     (Top_Level : Rho_Top_Level_Interface)
      return Rho.Viewport.Rho_Viewport
      is abstract;

   procedure Set_Viewport
     (Top_Level : in out Rho_Top_Level_Interface;
      Viewport  : Rho.Viewport.Rho_Viewport)
   is abstract;

   type Rho_Top_Level is access all Rho_Top_Level_Interface'Class;

   procedure Initialize
     (Css_Path : String := "");

   procedure Set_Color
     (Context : Cairo.Cairo_Context;
      Color   : Rho.Color.Rho_Color);

end Rho.Toolkit;
