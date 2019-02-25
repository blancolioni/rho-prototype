limited with Rho.Context;

with Rho.Materials.Material;

package Rho.Materials.Loader is

   function Load
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Path    : String)
      return Rho.Materials.Material.Rho_Material;

end Rho.Materials.Loader;
