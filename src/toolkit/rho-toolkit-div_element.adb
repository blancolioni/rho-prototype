package body Rho.Toolkit.Div_Element is

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New
     (Div : in out Rho_Div_Element)
   is
   begin
      Div := new Rho_Div_Element_Record;
      Div.Create ("");
   end Rho_New;

end Rho.Toolkit.Div_Element;
