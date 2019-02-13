package Rho.Toolkit.Orientable is

   type Rho_Orientation is (Across, Down);

   type Rho_Orientable_Interface is interface;

   function Orientation
     (Orientable : Rho_Orientable_Interface)
      return Rho_Orientation
      is abstract;

   procedure Set_Orientation
     (Orientable : in out Rho_Orientable_Interface;
      Orientation : Rho_Orientation)
   is abstract;

end Rho.Toolkit.Orientable;
