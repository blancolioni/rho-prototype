package body Rho.Toolkit.Text.Buffer is

   --------------
   -- Add_View --
   --------------

   procedure Add_View
     (Buffer : in out Rho_Text_Buffer_Record;
      View   : not null access Rho_Text_View_Interface'Class)
   is
   begin
      Buffer.Viewers.Append (View);
   end Add_View;

   ------------
   -- Append --
   ------------

   procedure Append
     (Buffer : in out Rho_Text_Buffer_Record'Class;
      Text   : String)
   is
      Move_Cursor : constant Boolean := Buffer.Cursor = Buffer.End_Iter;
   begin
      Ada.Strings.Unbounded.Append (Buffer.Text, Text);
      if Move_Cursor then
         Buffer.Cursor := Buffer.Cursor + Rho_Text_Iter (Text'Length);
      end if;
      Buffer.On_Changed;
   end Append;

   ---------------
   -- Backspace --
   ---------------

   procedure Backspace
     (Buffer : in out Rho_Text_Buffer_Record'Class)
   is
   begin
      if Buffer.Cursor > 0 then
         Buffer.Delete_Range (Buffer.Cursor, Buffer.Cursor);
      end if;
   end Backspace;

   ------------------
   -- Delete_Range --
   ------------------

   procedure Delete_Range
     (Buffer : in out Rho_Text_Buffer_Record'Class;
      Start  : Rho_Text_Iter;
      Finish : Rho_Text_Iter)
   is
      use Ada.Strings.Unbounded;
      First : constant Positive :=
                Positive'Max (Natural (Start), 1);
      Last  : constant Natural :=
                Natural'Min (Natural (Finish), Length (Buffer.Text));
   begin
      Delete (Buffer.Text, First, Last);
      if Buffer.Cursor >= Rho_Text_Iter (Last) then
         Buffer.Cursor := Buffer.Cursor - Rho_Text_Iter (Last - First + 1);
      end if;
      Buffer.On_Changed (Content_Change => True);
   end Delete_Range;

   --------------
   -- Get_Font --
   --------------

   function Get_Font
     (Buffer : Rho_Text_Buffer_Record)
      return Rho.Font.Rho_Font
   is
   begin
      return Buffer.Font;
   end Get_Font;

   --------------------
   -- Get_Line_Count --
   --------------------

   function Get_Line_Count
     (Buffer : Rho_Text_Buffer_Record)
      return Rho_Line_Count
   is
      use Ada.Strings.Unbounded;
      Result : Rho_Line_Count := 1;
   begin
      for I in 1 .. Length (Buffer.Text) loop
         if Element (Buffer.Text, I) = Character'Val (10) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Get_Line_Count;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Buffer : Rho_Text_Buffer_Record)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Buffer.Text);
   end Get_Text;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Buffer : Rho_Text_Buffer_Record;
      Start  : Rho_Text_Iter;
      Finish : Rho_Text_Iter)
      return String
   is
      use Ada.Strings.Unbounded;
   begin
      return Slice (Buffer.Text, Positive (Start), Natural (Finish));
   end Get_Text;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Buffer : in out Rho_Text_Buffer_Record'Class)
   is
   begin
      Buffer.Set_Font (Rho.Font.Get_Font ("SegoeUI", 14.0));
      Buffer.Cursor := 0;
   end Initialize;

   ----------------------
   -- Insert_At_Cursor --
   ----------------------

   procedure Insert_At_Cursor
     (Buffer : in out Rho_Text_Buffer_Record'Class;
      Text   : String)
   is
      use Ada.Strings.Unbounded;
   begin
      Buffer.Text :=
        Slice (Buffer.Text, 1, Natural (Buffer.Cursor))
        & To_Unbounded_String (Text)
        & Slice (Buffer.Text, Natural (Buffer.Cursor) + 1,
                 Length (Buffer.Text));
      Buffer.Cursor := Buffer.Cursor + Rho_Text_Iter (Text'Length);
      Buffer.On_Changed (Content_Change => True);
   end Insert_At_Cursor;

   ------------------
   -- Iter_At_Line --
   ------------------

   function Iter_At_Line
     (Buffer : Rho_Text_Buffer_Record;
      Line   : Rho_Line_Index)
      return Rho_Text_Iter
   is
      use Ada.Strings.Unbounded;
      Current : Rho_Line_Count := 1;
   begin
      if Line = 1 then
         return Buffer.Start_Iter;
      end if;

      for I in 1 .. Length (Buffer.Text) loop
         if Element (Buffer.Text, I) = Character'Val (10) then
            Current := Current + 1;
            if Current = Line then
               return Rho_Text_Iter (I + 1);
            end if;
         end if;
      end loop;
      return Buffer.End_Iter;
   end Iter_At_Line;

   -------------------------
   -- Iter_At_Line_Offset --
   -------------------------

   function Iter_At_Line_Offset
     (Buffer : Rho_Text_Buffer_Record;
      Line   : Rho_Line_Index;
      Offset : Rho_Character_Offset)
      return Rho_Text_Iter
   is
   begin
      return Buffer.Iter_At_Line (Line) + Rho_Text_Iter (Offset);
   end Iter_At_Line_Offset;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
     (Buffer         : in out Rho_Text_Buffer_Record'Class;
      Content_Change : Boolean := True)
   is
   begin
      Buffer.Changed := Buffer.Changed or else Content_Change;
      for Viewer of Buffer.Viewers loop
         Viewer.Text_Changed;
      end loop;
   end On_Changed;

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New
     (Buffer : in out Rho_Text_Buffer)
   is
   begin
      Buffer := new Rho_Text_Buffer_Record;
      Buffer.Initialize;
   end Rho_New;

   -------------------------
   -- Set_Cursor_Position --
   -------------------------

   procedure Set_Cursor_Position
     (Buffer : in out Rho_Text_Buffer_Record;
      Iter   : Rho_Text_Iter)
   is
   begin
      Buffer.Cursor := Iter;
      Buffer.On_Changed (Content_Change => False);
   end Set_Cursor_Position;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font
     (Buffer : in out Rho_Text_Buffer_Record;
      Font   : Rho.Font.Rho_Font)
   is
   begin
      Buffer.Font := Font;
   end Set_Font;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Buffer : in out Rho_Text_Buffer_Record;
      Text   : String)
   is
   begin
      Buffer.Text := Ada.Strings.Unbounded.To_Unbounded_String (Text);
      Buffer.On_Changed;
   end Set_Text;

end Rho.Toolkit.Text.Buffer;
