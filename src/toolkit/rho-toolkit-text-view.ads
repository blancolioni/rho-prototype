private with Css;

with Rho.Toolkit.Text.Buffer;
with Rho.Toolkit.Widget;

package Rho.Toolkit.Text.View is

   type Rho_Text_View_Record is
     new Rho.Toolkit.Widget.Rho_Widget_Record
     and Rho_Text_View_Interface
   with private;

   type Rho_Text_View is access all Rho_Text_View_Record'Class;

   overriding function Default_Tag
     (Text_View : Rho_Text_View_Record)
      return String
   is ("Text_View");

   overriding function Widget_Hierarchy_Tags
     (Text_View : Rho_Text_View_Record)
      return String
   is ("Text_View "
       & Rho.Toolkit.Widget.Rho_Widget_Record (Text_View)
       .Widget_Hierarchy_Tags);

   function Text_Buffer
     (View : Rho_Text_View_Record'Class)
      return Rho.Toolkit.Text.Buffer.Rho_Text_Buffer;

   overriding procedure Initialize
     (Item : in out Rho_Text_View_Record);

   procedure Rho_New
     (Item : in out Rho_Text_View);

   function Is_Editable
     (View : Rho_Text_View_Record)
      return Boolean;

   procedure Set_Editable
     (View     : in out Rho_Text_View_Record;
      Editable : Boolean);

   function Cursor_Visible
     (View : Rho_Text_View_Record)
      return Boolean;

   procedure Set_Cursor_Visible
     (View    : in out Rho_Text_View_Record;
      Visible : Boolean);

private

   type Rho_Text_View_Record is
     new Rho.Toolkit.Widget.Rho_Widget_Record
     and Rho_Text_View_Interface with
      record
         Buffer         : Rho.Toolkit.Text.Buffer.Rho_Text_Buffer;
         Wrap           : Rho_Text_Wrap_Mode := Word_Wrap;
         Cursor_Hidden  : Duration := 0.1;
         Cursor_Shown   : Duration := 0.2;
         Editable       : Boolean := True;
         Cursor_Visible : Boolean := False;
      end record;

   overriding function Minimum_Size
     (View : Rho_Text_View_Record;
      Constraint : Css.Layout_Size)
      return Css.Layout_Size;

   overriding procedure Text_Changed
     (View : in out Rho_Text_View_Record);

   overriding procedure Set_Text
     (View : in out Rho_Text_View_Record;
      Text : String);

   function Is_Editable
     (View : Rho_Text_View_Record)
      return Boolean
   is (View.Editable);

   function Cursor_Visible
     (View : Rho_Text_View_Record)
      return Boolean
   is (View.Cursor_Visible);

end Rho.Toolkit.Text.View;
