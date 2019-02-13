with Rho.Toolkit.Container;

package Rho.Toolkit.Div_Element is

   type Rho_Div_Element_Record is
     new Rho.Toolkit.Container.Rho_Container_Record with private;

   type Rho_Div_Element is access all Rho_Div_Element_Record'Class;

   procedure Rho_New
     (Div : in out Rho_Div_Element);

private

   type Rho_Div_Element_Record is
     new Rho.Toolkit.Container.Rho_Container_Record with null record;

end Rho.Toolkit.Div_Element;
