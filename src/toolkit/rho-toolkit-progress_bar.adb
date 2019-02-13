with Glib;

--  with Pango.Cairo;
--  with Pango.Font;
--  with Pango.Layout;

with Rho.Color;
--  with Rho.Font;

package body Rho.Toolkit.Progress_Bar is

   -------------------
   -- Bar_Rectangle --
   -------------------

   function Bar_Rectangle
     (Item : Rho_Progress_Bar_Record'Class)
      return Rho.Rectangle.Rho_Rectangle
   is
      use type Rho_Float;
      Border : constant Rho_Float := Rho_Float (Item.Border_Width);
   begin
      return Result : Rho.Rectangle.Rho_Rectangle do
         Result.X := Border;
         Result.Y := Border;
         Result.Width :=
           (Item.Value - Item.Low)
           / (Item.High - Item.Low)
           * (Item.Layout_Width - Border * 2.0);
         Result.Height :=
           Item.Layout_Height - Border * 2.0;
      end return;
   end Bar_Rectangle;

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (Item : in out Rho_Progress_Bar_Record;
      Low  : Rho_Float;
      High : Rho_Float;
      Value : Rho_Float)
   is
   begin
      Rho.Toolkit.Meter.Create
        (Rho.Toolkit.Meter.Rho_Meter_Record (Item),
         Low, High, Value);
   end Create;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Progress : in out Rho_Progress_Bar_Record;
      Context  : in     Cairo.Cairo_Context;
      X, Y     : in     Rho_Float;
      Region   : in     Rho.Rectangle.Rho_Rectangle)
   is
      use Maas;
      use Glib;

      P : Rho_Float :=
            (Progress.Value - Progress.Low)
            / (Progress.High - Progress.Low);
      Border : constant Rho_Float := Rho_Float (Progress.Border_Width);
      Bar_Color : constant Rho.Color.Rho_Color :=
                    Progress.Foreground_Color;
   begin
      Rho.Toolkit.Meter.Rho_Meter_Record (Progress).Draw
        (Context, X, Y, Region);

      if Progress.MaMaasmum_Change /= 0.0
        and then abs (P - Progress.Previous_Progress) > Progress.MaMaasmum_Change
      then
         if P > Progress.Previous_Progress then
            P := Progress.Previous_Progress + Progress.MaMaasmum_Change;
         else
            P := Progress.Previous_Progress - Progress.MaMaasmum_Change;
         end if;
      end if;

      Progress.Previous_Progress := P;

      declare
         Bar_X : constant Rho_Float := X + Region.X + Border;
         Bar_Y : constant Rho_Float := Y + Region.Y + Border;
         Bar_W : constant Rho_Float :=
                   P * (Progress.Layout_Width - Border * 2.0);
         Bar_H : constant Rho_Float :=
                   Progress.Layout_Height - Border * 2.0;
      begin
         Set_Color (Context, Bar_Color);
         Cairo.Rectangle (Context, Gdouble (Bar_X), Gdouble (Bar_Y),
                          Gdouble (Bar_W), Gdouble (Bar_H));
         Cairo.Fill (Context);
      end;

--        if Progress.Label /= "" then
--           declare
--              S             : constant String := Progress.Label;
--              Layout        : constant Pango.Layout.Pango_Layout :=
--                                Rho.Font.Pango_Cairo_Create_Layout (Context);
--           begin
--              Layout.Set_Width (-1);
--              Layout.Set_Height (Glib.Gint (Progress.Height) * 1024);
--              Layout.Set_Text (S);
--              Layout.Set_Font_Description
--                (Pango.Font.From_String
--                   (Progress.Font));
--
--              Set_Color (Context, Progress.Foreground_Color);
--
--              Cairo.Move_To (Context,
--                             Glib.Gdouble (X + Region.X),
--                             Glib.Gdouble (Y + Region.Y));
--
--              Pango.Cairo.Show_Layout (Context, Layout);

            --           case Label.Get_Vertical_Alignment is
            --              when Rho.Toolkit.Widget.Top =>
            --                 Start_Y := Start_Y
            --                   + Natural (Region.Height) - Natural (Height);
            --              when Rho.Toolkit.Widget.Middle =>
            --                 Start_Y := Start_Y
            --               + Natural (Region.Height / 2.0 - Height / 2.0);
            --              when Rho.Toolkit.Widget.Bottom =>
            --                 null;
            --           end case;
            --
            --           case Label.Get_Horizontal_Alignment is
            --              when Rho.Toolkit.Widget.Right =>
            --                 Start_X := Start_X
            --                   + Natural (Region.Width) - Natural (Width);
            --              when Rho.Toolkit.Widget.Middle =>
            --                 Start_X := Start_X
            --                   + Natural (Region.Width / 2.0 - Width / 2.0);
            --              when Rho.Toolkit.Widget.Left =>
            --                 null;
            --           end case;

--           end;
--
--        end if;
   end Draw;

   ---------------
   -- Set_Value --
   ---------------

   overriding procedure Set_Value
     (Progress : in out Rho_Progress_Bar_Record;
      Value : Rho_Float)
   is
      use Maas;
      use Rho.Rectangle;
      Old_Value : constant Rho_Float :=
                    Rho_Progress_Bar_Record'Class (Progress).Value;
      Old_Bar_Rectangle : constant Rho_Rectangle := Progress.Bar_Rectangle;
      Invalid_Region    : Rho_Rectangle;
   begin
      Rho.Toolkit.Meter.Rho_Meter_Record (Progress).Set_Value (Value);

      if Value > Old_Value then
         Invalid_Region := Progress.Bar_Rectangle;
         Invalid_Region.Width :=
           Invalid_Region.Width - Old_Bar_Rectangle.Width + 4.0;
         Invalid_Region.X :=
           Old_Bar_Rectangle.X + Old_Bar_Rectangle.Width - 4.0;
         if Invalid_Region.X < 0.0 then
            Invalid_Region.Width := Invalid_Region.Width + Invalid_Region.X;
            Invalid_Region.X := 0.0;
         end if;
      else
         Invalid_Region := Old_Bar_Rectangle;
      end if;

      Rho_Progress_Bar_Record'Class (Progress).Invalidate_Region
        (Invalid_Region);
   end Set_Value;

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New
     (Item  : out Rho_Progress_Bar;
      Low   : Rho_Float;
      High  : Rho_Float;
      Value : Rho_Float)
   is
   begin
      Item := new Rho_Progress_Bar_Record;
      Item.Create (Low, High, Value);
   end Rho_New;

end Rho.Toolkit.Progress_Bar;
