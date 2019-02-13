with Rho.Toolkit.Widget;

private package Rho.Toolkit.Widget_Registry is

   type Creator_Function is access
     function (Tag        : String;
               Parent     : Rho.Toolkit.Widget.Rho_Widget;
               Top_Level  : Boolean;
               Inner_Text : String)
               return Rho.Toolkit.Widget.Rho_Widget;

   procedure Register
     (Tag     : String;
      Creator : Creator_Function);

   procedure Register_Standard_Widgets;

   function Create
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget;

end Rho.Toolkit.Widget_Registry;
