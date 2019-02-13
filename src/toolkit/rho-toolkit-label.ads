private with Ada.Strings.Unbounded;

with Css;

with Rho.Toolkit.Widget;

package Rho.Toolkit.Label is

   type Rho_Label_Record is
     new Rho.Toolkit.Widget.Rho_Widget_Record
       with private;

   type Rho_Label is access all Rho_Label_Record'Class;

   overriding procedure Initialize
     (Label : in out Rho_Label_Record);

   overriding function Default_Tag
     (Label : Rho_Label_Record)
      return String
   is ("Label");

   overriding function Widget_Hierarchy_Tags
     (Label : Rho_Label_Record)
      return String
   is ("Label "
       & Rho.Toolkit.Widget.Rho_Widget_Record (Label).Widget_Hierarchy_Tags);

   overriding function Minimum_Size
     (Label      : Rho_Label_Record;
      Constraint : Css.Layout_Size)
      return Css.Layout_Size;

   function Label (Label : Rho_Label_Record) return String;
   procedure Set_Label (Label : in out Rho_Label_Record;
                        Text  : in     String);

   procedure Create_With_Label
     (Item       : in out Rho_Label_Record;
      Label      : String;
      Element_Id : String := "");

   procedure Rho_New
     (Item : in out Rho_Label;
      Text : String);

   function Rho_New
     (Text : String)
      return Rho_Label;

private

   type Rho_Label_Record is
     new Rho.Toolkit.Widget.Rho_Widget_Record with
      record
         Text : Ada.Strings.Unbounded.Unbounded_String;
      end record;

end Rho.Toolkit.Label;
