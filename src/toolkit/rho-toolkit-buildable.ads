package Rho.Toolkit.Buildable is

   type Rho_Buildable_Interface is interface;

   procedure Set_Attribute
     (Buildable : in out Rho_Buildable_Interface;
      Name      : String;
      Value     : String)
   is abstract;

   procedure Set_Text
     (Buildable : in out Rho_Buildable_Interface;
      Text      : String)
   is abstract;

   procedure Add_Child
     (Buildable : not null access Rho_Buildable_Interface;
      Child     : not null access Rho_Buildable_Interface'Class)
   is abstract;

   procedure Set_Tag
     (Buildable : in out Rho_Buildable_Interface;
      Tag       : String)
   is abstract;

   type Rho_Buildable is access all Rho_Buildable_Interface'Class;

end Rho.Toolkit.Buildable;
