with Cairo;

with Rho.Rectangle;
with Rho.Toolkit.Container;
with Rho.Toolkit.Widget;

package Rho.Toolkit.Fixed is

   type Rho_Fixed_Record is
     new Rho.Toolkit.Container.Rho_Container_Record
       with private;

   overriding function Default_Tag
     (Fixed : Rho_Fixed_Record)
      return String
   is ("Fixed");

   overriding function Widget_Hierarchy_Tags
     (Fixed : Rho_Fixed_Record)
      return String
   is ("Fixed "
       & Rho.Toolkit.Container.Rho_Container_Record (Fixed).Widget_Hierarchy_Tags);

   overriding procedure Draw
     (Fixed   : in out Rho_Fixed_Record;
      Context  : in     Cairo.Cairo_Context;
      X, Y     : in     Rho_Float;
      Region   : in     Rho.Rectangle.Rho_Rectangle);

   overriding procedure Add
     (Fixed : not null access Rho_Fixed_Record;
      Child  : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class);

   overriding procedure Get_Preferred_Size
     (Fixed : Rho_Fixed_Record;
      Width, Height : out Rho_Float);

   type Rho_Fixed is access all Rho_Fixed_Record'Class;

   procedure Rho_New
     (Fixed : in out Rho_Fixed;
      Id    : String := "");

private

   type Rho_Fixed_Record is
     new Rho.Toolkit.Container.Rho_Container_Record
       with null record;

end Rho.Toolkit.Fixed;
