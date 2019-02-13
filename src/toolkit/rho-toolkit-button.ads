with Rho.Mouse;

with Rho.Toolkit.Label;

package Rho.Toolkit.Button is

   type Rho_Button_Record is
     new Rho.Toolkit.Label.Rho_Label_Record
   with private;

   overriding function Default_Tag
     (Button : Rho_Button_Record)
      return String
   is ("Button");

   overriding function Widget_Hierarchy_Tags
     (Button : Rho_Button_Record)
      return String
   is ("Button "
       & Rho.Toolkit.Label.Rho_Label_Record (Button).Widget_Hierarchy_Tags);

   type Rho_Button is access all Rho_Button_Record'Class;

   procedure Create
     (Item       : in out Rho_Button_Record;
      Label      : String;
      Element_Id : String);

   procedure Rho_New
     (Item       : in out Rho_Button;
      Text       : String;
      Element_Id : String);

   procedure Rho_New
     (Item       : in out Rho_Button);

private

   type Rho_Button_Record is
     new Rho.Toolkit.Label.Rho_Label_Record
       with null record;

end Rho.Toolkit.Button;
