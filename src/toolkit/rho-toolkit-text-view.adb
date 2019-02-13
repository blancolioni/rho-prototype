with Ada.Containers.Indefinite_Vectors;

with Glib;
with Gtkada.Types;

with Cairo;

with Rho.Font;
with Rho.Keyboard;

with Rho.Toolkit.Events;

package body Rho.Toolkit.Text.View is

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   function Get_Text_Extents
     (Context : Cairo.Cairo_Context;
      Text    : String)
      return Cairo.Cairo_Text_Extents;

   function Handle_Draw
     (Widget  : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class;
      Context : Cairo.Cairo_Context)
      return Rho.Toolkit.Events.Event_Response;

   function On_Key_Press
     (Widget : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class;
      Key    : Rho.Keyboard.Rho_Key_Code)
      return Rho.Toolkit.Events.Event_Response;

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
     (Widget  : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class;
      Context : Cairo.Cairo_Context)
      return Rho.Toolkit.Events.Event_Response
   is
      Line_Height : constant := 20.0;
      Text_View   : constant Rho_Text_View := Rho_Text_View (Widget);
      Font        : constant Rho.Font.Rho_Font := Text_View.Buffer.Get_Font;
      Text        : constant String := Text_View.Buffer.Get_Text;
      Text_X      : constant Rho_Float := 0.0;
      Text_Y      : Rho_Float := Line_Height;
      Lines       : String_Vectors.Vector;
      Start       : Positive := Text'First;
      Finish      : Natural  := 0;
      Got_NL      : Boolean := False;
   begin

      Font.Cairo_Font (Context);
      Set_Color (Context, Text_View.Foreground_Color);

      for I in Text'Range loop
         declare
            Partial : constant String := Text (Start .. I);
            Extents : constant Cairo.Cairo_Text_Extents :=
                        Get_Text_Extents (Context, Partial);
         begin
            if Text (I) = Character'Val (10)
              or else Text (I) = Character'Val (13)
            then
               if Got_NL then
                  null;
               else
                  Finish := I;
                  Lines.Append (Text (Start .. Finish - 1));
               end if;
               Start := I + 1;
               Got_NL := True;
            else
               Got_NL := False;
               if Start = I and then Text (I) = ' ' then
                  Start := I + 1;
               elsif Rho_Float (Extents.Width)
                 >= Rho_Float (Text_View.Layout_Width)
               then
                  Finish := I;
                  case Text_View.Wrap is
                     when No_Wrap | Character_Wrap =>
                        null;
                     when Word_Wrap =>
                        while Finish > Start
                          and then Text (Finish) /= ' '
                        loop
                           Finish := Finish - 1;
                        end loop;
                        if Finish = Start then
                           Finish := I;
                        end if;
                  end case;

                  Lines.Append (Text (Start .. Finish - 1));
                  Start := Finish + 1;
                  while Start <= I
                    and then Text (Start) = ' '
                  loop
                     Start := Start + 1;
                  end loop;
               end if;
            end if;
         end;
      end loop;

      if Start <= Text'Last then
         Lines.Append (Text (Start .. Text'Last));
      end if;

      declare
         Line_Count : constant Natural :=
                        Integer'Max
                          (Integer (Text_View.Layout_Height / Line_Height),
                           0);
         First_Line : constant Positive :=
                        Integer'Max (Lines.Last_Index - Line_Count + 1,
                                     1);
      begin
         for I in First_Line .. Lines.Last_Index loop
            Cairo.Move_To (Context,
                           Glib.Gdouble (Text_X),
                           Glib.Gdouble (Text_Y));
            Cairo.Show_Text (Context, Lines.Element (I));
            Text_Y := Text_Y + Line_Height;
         end loop;
      end;

      return Rho.Toolkit.Events.Propagate_Event;
   end Handle_Draw;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Item : in out Rho_Text_View_Record)
   is
   begin
      Rho.Toolkit.Widget.Rho_Widget_Record (Item).Initialize;
      --  Rho.Toolkit.Widget.Rho_Widget_Record (Item).Create ("view");
      Rho.Toolkit.Text.Buffer.Rho_New (Item.Buffer);
      Item.On_Draw (Handle_Draw'Access);
      Item.On_Key_Press (On_Key_Press'Access);
   end Initialize;

   ------------------
   -- Minimum_Size --
   ------------------

   overriding function Minimum_Size
     (View       : Rho_Text_View_Record;
      Constraint : Css.Layout_Size)
      return Css.Layout_Size
   is
      pragma Unreferenced (View, Constraint);
   begin
      return (True, True, 300.0, 100.0);
   end Minimum_Size;

   ------------------
   -- On_Key_Press --
   ------------------

   function On_Key_Press
     (Widget : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class;
      Key    : Rho.Keyboard.Rho_Key_Code)
      return Rho.Toolkit.Events.Event_Response
   is
      use type Rho.Keyboard.Rho_Key_Code;
      View : constant Rho_Text_View := Rho_Text_View (Widget);
   begin
      if View.Is_Editable then
         if Rho.Keyboard.Is_Graphic (Key) then
            View.Buffer.Insert_At_Cursor
              ((1 => Rho.Keyboard.To_Character (Key)));
         elsif Key = Rho.Keyboard.Key_New_Line then
            View.Buffer.Insert_At_Cursor
              ((1 => Character'Val (10)));
         elsif Key = Rho.Keyboard.Key_Back then
            View.Buffer.Backspace;
         end if;
      end if;
      return Rho.Toolkit.Events.Stop_Event;
   end On_Key_Press;

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New
     (Item : in out Rho_Text_View)
   is
   begin
      Item := new Rho_Text_View_Record;
      Item.Buffer.Add_View (Item);
   end Rho_New;

   ------------------------
   -- Set_Cursor_Visible --
   ------------------------

   procedure Set_Cursor_Visible
     (View    : in out Rho_Text_View_Record;
      Visible : Boolean)
   is
   begin
      View.Cursor_Visible := Visible;
   end Set_Cursor_Visible;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
     (View     : in out Rho_Text_View_Record;
      Editable : Boolean)
   is
   begin
      View.Editable := Editable;
   end Set_Editable;

   --------------
   -- Set_Text --
   --------------

   overriding procedure Set_Text
     (View : in out Rho_Text_View_Record;
      Text : String)
   is
   begin
      View.Buffer.Set_Text (Text);
   end Set_Text;

   -----------------
   -- Text_Buffer --
   -----------------

   function Text_Buffer
     (View : Rho_Text_View_Record'Class)
      return Rho.Toolkit.Text.Buffer.Rho_Text_Buffer
   is
   begin
      return View.Buffer;
   end Text_Buffer;

   ------------------
   -- Text_Changed --
   ------------------

   overriding procedure Text_Changed
     (View : in out Rho_Text_View_Record)
   is
   begin
      View.Queue_Draw;
   end Text_Changed;

end Rho.Toolkit.Text.View;
