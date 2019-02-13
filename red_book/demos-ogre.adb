with Rho.Assets;
with Rho.Entity;
with Rho.Node;
with Rho.Camera;
with Rho.Light;
with Rho.Mesh;

package body Demos.Ogre is

   type Ogre_Head_Demo is
     new Root_Demo_Type with null record;

   overriding function Identity (Demo : Ogre_Head_Demo) return String
   is ("ogre-head");

   overriding function Name (Demo : Ogre_Head_Demo) return String
   is ("Ogre Head Demo");

   overriding function Description (Demo : Ogre_Head_Demo) return String
   is ("First Ogre tutorial example");

   overriding function Create_Scene
     (Demo : in out Ogre_Head_Demo)
      return Rho.Scene.Rho_Scene;

   ------------------
   -- Create_Scene --
   ------------------

   overriding function Create_Scene
     (Demo : in out Ogre_Head_Demo)
      return Rho.Scene.Rho_Scene
   is
      pragma Unreferenced (Demo);
      use Rho;
      Scene     : constant Rho.Scene.Rho_Scene := Rho.Scene.Create_Scene;
      Camera    : constant Rho.Camera.Rho_Camera := Scene.Active_Camera;
      Node      : constant Rho.Node.Rho_Node := Scene.Create_Node ("ogre");
      Light     : Rho.Light.Rho_Light;
      Ogre_Mesh : constant Rho.Mesh.Rho_Mesh :=
                    Rho.Assets.Mesh ("ogrehead");
      Ogre      : constant Rho.Entity.Rho_Entity :=
                    Ogre_Mesh.Create_Entity;
   begin

--        if False then
--           GL.Enable_Debug;
--        end if;

      Camera.Set_Position (0.0, 0.0, 100.0);
      Camera.Look_At (0.0, 1.0, 0.0, 0.0, 0.0, 0.0);
      Camera.Frustum (-15.0, 15.0, -15.0, 15.0, 5.0, 500.0);

      Rho.Light.Rho_New (Light, Rho.Light.Ambient);
      Light.Set_Position (100.0, 30.0, 30.0);
      Light.Set_Color (0.5, 0.5, 0.5, 1.0);
      Scene.Add_Light (Light);
      Node.Set_Entity (Ogre);

      return Scene;

   end Create_Scene;

   ---------------
   -- Ogre_Head --
   ---------------

   function Ogre_Head return Rho_Demo_Type is
   begin
      return new Ogre_Head_Demo;
   end Ogre_Head;

end Demos.Ogre;
