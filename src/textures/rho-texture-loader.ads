limited with Rho.Context;

package Rho.Texture.Loader is

   function Load_Texture
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Path    : String)
     return Rho_Texture;

end Rho.Texture.Loader;
