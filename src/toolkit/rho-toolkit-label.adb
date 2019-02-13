with Glib;
with Gtkada.Types;

with Rho.Font;

with Rho.Toolkit.Events;

package body Rho.Toolkit.Label is

   function Get_Text_Extents
     (Context : Cairo.Cairo_Context;
      Text    : String)
      return Cairo.Cairo_Text_Extents;

   function Handle_Draw
     (Widget : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class;
      Cr     : Cairo.Cairo_Context)
      return Rho.Toolkit.Events.Event_Response;

   ------------
   -- Create --
   ------------

   procedure Create_With_Label
     (Item       : in out Rho_Label_Record;
      Label      : String;
      Element_Id : String := "")
   is
   begin
      Rho.Toolkit.Widget.Rho_Widget_Record (Item).Create (Element_Id);

      Item.Set_Label (Label);
   end Create_With_Label;

   ----------------------
   -- Get_Text_Extents --
   ----------------------

   function Get_Text_Extents
     (Context : Cairo.Cairo_Context;
      Text    : String)
      return Cairo.Cairo_Text_Extents
   is
      Extents : aliased Cairo.Cairo_Text_Extents;
      Str_Ptr : Gtkada.Types.Chars_Ptr :=
                  Gtkada.Types.New_String (Text);
   begin
      Cairo.Text_Extents (Context, Str_Ptr, Extents'Access);
      Gtkada.Types.Free (Str_Ptr);
      return Extents;
   end Get_Text_Extents;

   -----------------
   -- Handle_Draw --
   -----------------

   function Handle_Draw
     (Widget : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class;
      Cr     : Cairo.Cairo_Context)
      return Rho.Toolkit.Events.Event_Response
   is
      Label : constant Rho_Label := Rho_Label (Widget);
   begin
      Label.Font.Cairo_Font (Cr);

      declare
         Text    : constant String :=
                     Ada.Strings.Unbounded.To_String
                       (Label.Text);
         Extents : constant Cairo.Cairo_Text_Extents :=
                     Get_Text_Extents (Cr, Text);
         Text_X  : constant Rho_Float := 0.0;
         Text_Y  : constant Rho_Float :=
                     Rho_Float (Label.Layout_Height) / 2.0
                     + Rho_Float (Extents.Height) / 2.0;
      begin

         --              case Label.Get_Horizontal_Alignment is
         --                 when Rho.Toolkit.Alignment.Start =>
         --                    null;
         --                 when Rho.Toolkit.Alignment.Finish =>
         --                    Text_X := Text_X
         --                      + Region.Width - Rho_Float (Extents.Width);
         --                 when others =>
         --                    Text_X := Text_X + Region.Width / 2.0
         --                      - Rho_Float (Extents.Width) / 2.0;
         --              end case;
         --
         --              case Label.Get_Vertical_Alignment is
         --                 when Rho.Toolkit.Alignment.Finish =>
         --                    null;
         --                 when Rho.Toolkit.Alignment.Start =>
         --                    Text_Y := Text_Y
         --                  - Region.Height + Rho_Float (Extents.Height);
         --                 when others =>
         --                    Text_Y := Text_Y - Region.Height / 2.0
         --                      + Rho_Float (Extents.Height) / 2.0;
         --              end case;

         Set_Color (Cr, Label.Foreground_Color);
         Cairo.Move_To (Cr,
                        Glib.Gdouble (Text_X),
                        Glib.Gdouble (Text_Y));
         Cairo.Show_Text (Cr, Text);
      end;

      --           case Label.Get_Vertical_Alignment is
      --              when Rho.Toolkit.Widget.Top =>
      --                 Start_Y := Start_Y
      --                   + Natural (Region.Height) - Natural (Height);
      --              when Rho.Toolkit.Widget.Middle =>
      --                 Start_Y := Start_Y
      --                   + Natural (Region.Height / 2.0 - Height / 2.0);
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

      return Rho.Toolkit.Events.Propagate_Event;

   end Handle_Draw;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Label : in out Rho_Label_Record)
   is
   begin
      Rho.Toolkit.Widget.Rho_Widget_Record (Label).Initialize;
      Label.On_Draw
        (Handle_Draw'Access);
   end Initialize;

   -----------
   -- Label --
   -----------

   function Label (Label : Rho_Label_Record) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Label.Text);
   end Label;

   ------------------
   -- Minimum_Size --
   ------------------

   overriding function Minimum_Size
     (Label      : Rho_Label_Record;
      Constraint : Css.Layout_Size)
      return Css.Layout_Size
   is
      pragma Unreferenced (Constraint);  --  for now ...
      S        : constant String :=
                   Ada.Strings.Unbounded.To_String (Label.Text);
      S_Width  : Rho_Float;
      S_Height : Rho_Float;
   begin
      Rho.Font.Measure_String (Label.Font, S, S_Width, S_Height);
      return (True, True, Float (S_Width), Float (S_Height));
   end Minimum_Size;

   ------------
   -- Rho_New --
   ------------

   procedure Rho_New
     (Item : in out Rho_Label;
      Text : String)
   is
   begin
      Item := new Rho_Label_Record;
      Item.Create_With_Label (Text);
   end Rho_New;

   -------------
   -- Rho_New --
   -------------

   function Rho_New
     (Text : String)
      return Rho_Label
   is
      Result : Rho_Label;
   begin
      Rho_New (Result, Text);
      return Result;
   end Rho_New;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
     (Label : in out Rho_Label_Record;
      Text  : in     String)
   is
      use Ada.Strings.Unbounded;
   begin
      if Text /= Label.Text then
         Label.Text := To_Unbounded_String (Text);
         Rho_Label_Record'Class (Label).Invalidate_Region
           (Label.Layout_Rectangle);
         Label.Queue_Resize;
      end if;
   end Set_Label;

end Rho.Toolkit.Label;
