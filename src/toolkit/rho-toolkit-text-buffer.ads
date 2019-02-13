private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

with Rho.Font;

package Rho.Toolkit.Text.Buffer is

   type Rho_Text_Buffer_Record is tagged private;

   type Rho_Text_Buffer is access all Rho_Text_Buffer_Record'Class;

   procedure Initialize
     (Buffer : in out Rho_Text_Buffer_Record'Class);

   procedure Rho_New
     (Buffer : in out Rho_Text_Buffer);

   procedure Set_Font
     (Buffer : in out Rho_Text_Buffer_Record;
      Font   : Rho.Font.Rho_Font);

   function Get_Font
     (Buffer : Rho_Text_Buffer_Record)
      return Rho.Font.Rho_Font;

   function Get_Text
     (Buffer : Rho_Text_Buffer_Record)
      return String;

   function Get_Text
     (Buffer : Rho_Text_Buffer_Record;
      Start  : Rho_Text_Iter;
      Finish : Rho_Text_Iter)
      return String;

   function Get_Line_Count
     (Buffer : Rho_Text_Buffer_Record)
      return Rho_Line_Count;

   procedure Add_View
     (Buffer : in out Rho_Text_Buffer_Record;
      View   : not null access Rho_Text_View_Interface'Class);

   procedure Append
     (Buffer : in out Rho_Text_Buffer_Record'Class;
      Text   : String);

   procedure Set_Text
     (Buffer : in out Rho_Text_Buffer_Record;
      Text   : String);

   procedure Insert_At_Cursor
     (Buffer : in out Rho_Text_Buffer_Record'Class;
      Text   : String);

   procedure Delete_Range
     (Buffer : in out Rho_Text_Buffer_Record'Class;
      Start  : Rho_Text_Iter;
      Finish : Rho_Text_Iter);

   procedure Backspace
     (Buffer : in out Rho_Text_Buffer_Record'Class);

   function Cursor_Position
     (Buffer : Rho_Text_Buffer_Record)
      return Rho_Text_Iter;

   procedure Set_Cursor_Position
     (Buffer : in out Rho_Text_Buffer_Record;
      Iter   : Rho_Text_Iter);

   function Start_Iter
     (Buffer : Rho_Text_Buffer_Record)
      return Rho_Text_Iter;

   function End_Iter
     (Buffer : Rho_Text_Buffer_Record)
      return Rho_Text_Iter;

   function Iter_At_Line
     (Buffer : Rho_Text_Buffer_Record;
      Line   : Rho_Line_Index)
      return Rho_Text_Iter;

   function Iter_At_Line_Offset
     (Buffer : Rho_Text_Buffer_Record;
      Line   : Rho_Line_Index;
      Offset : Rho_Character_Offset)
      return Rho_Text_Iter;

private

   type Text_Viewer is access all Rho_Text_View_Interface'Class;

   package Text_Viewer_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Text_Viewer);

   type Rho_Text_Buffer_Record is tagged
      record
         Text     : Ada.Strings.Unbounded.Unbounded_String;
         Viewers  : Text_Viewer_Lists.List;
         Font     : Rho.Font.Rho_Font;
         Cursor   : Rho_Text_Iter := 0;
         Changed  : Boolean := False;
      end record;

   procedure On_Changed
     (Buffer         : in out Rho_Text_Buffer_Record'Class;
      Content_Change : Boolean := True);

   function Start_Iter
     (Buffer : Rho_Text_Buffer_Record)
      return Rho_Text_Iter
   is (1);

   function End_Iter
     (Buffer : Rho_Text_Buffer_Record)
      return Rho_Text_Iter
   is (Rho_Text_Iter (Ada.Strings.Unbounded.Length (Buffer.Text)));

   function Cursor_Position
     (Buffer : Rho_Text_Buffer_Record)
      return Rho_Text_Iter
   is (Buffer.Cursor);

end Rho.Toolkit.Text.Buffer;
