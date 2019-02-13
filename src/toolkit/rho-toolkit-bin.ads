with Rho.Toolkit.Container;
with Rho.Toolkit.Widget;

package Rho.Toolkit.Bin is

   type Rho_Bin_Record is
     abstract new Rho.Toolkit.Container.Rho_Container_Record with private;

   overriding function Default_Tag
     (Bin : Rho_Bin_Record)
      return String
   is ("Bin");

   overriding function Widget_Hierarchy_Tags
     (Bin : Rho_Bin_Record)
      return String
   is ("Bin "
       & Rho.Toolkit.Container.Rho_Container_Record (Bin)
       .Widget_Hierarchy_Tags);

   function Has_Child
     (Bin : Rho_Bin_Record'Class)
      return Boolean;

   function Child
     (Bin : Rho_Bin_Record'Class)
      return Rho.Toolkit.Widget.Rho_Widget;

   type Rho_Bin is access all Rho_Bin_Record'Class;

private

   type Rho_Bin_Record is
     abstract new Rho.Toolkit.Container.Rho_Container_Record with null record;

   function Has_Child
     (Bin : Rho_Bin_Record'Class)
      return Boolean
   is (Bin.Has_Children);

end Rho.Toolkit.Bin;
