package Rho.Scene_Manager is

   type Rho_Scene_Manager_Record is tagged private;

   type Rho_Scene_Manager is access all Rho_Scene_Manager_Record'Class;

private

   type Rho_Scene_Manager_Record is tagged null record;

end Rho.Scene_Manager;
