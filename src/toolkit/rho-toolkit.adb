with Glib;

with Css.Parser;

package body Rho.Toolkit is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Css_Path : String := "")
   is
   begin
      if Css_Path /= "" then
         Css.Parser.Load_Css_File (Css_Path);
      end if;
   end Initialize;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Context : Cairo.Cairo_Context;
      Color   : Rho.Color.Rho_Color)
   is
   begin
      Cairo.Set_Source_Rgba
        (Context,
         Glib.Gdouble (Color.Red),
         Glib.Gdouble (Color.Green),
         Glib.Gdouble (Color.Blue),
         Glib.Gdouble (Color.Alpha));
   end Set_Color;

end Rho.Toolkit;
