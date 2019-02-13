with Demos.Faces;
with Demos.Ogre;
with Demos.Red_Book;

package body Rho_Demo_Loader is

   ----------------
   -- Load_Demos --
   ----------------

   procedure Load_Demos is
   begin
      Demos.Add_Demo (Demos.Red_Book.Triangles);
      Demos.Add_Demo (Demos.Ogre.Ogre_Head);
      Demos.Add_Demo (Demos.Faces.Face_Demo);
   end Load_Demos;

end Rho_Demo_Loader;
