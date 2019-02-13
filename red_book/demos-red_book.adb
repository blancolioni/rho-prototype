with Rho.Entity;
with Rho.Node;
with Rho.Render_Operation;

package body Demos.Red_Book is

   type Triangles_Demo is
     new Root_Demo_Type with null record;

   overriding function Identity (Demo : Triangles_Demo) return String
   is ("01-triangles");

   overriding function Name (Demo : Triangles_Demo) return String
   is ("Red Book Triangles Demo");

   overriding function Description (Demo : Triangles_Demo) return String
   is ("Simple triangle demo");

   overriding function Create_Scene
     (Demo : in out Triangles_Demo)
      return Rho.Scene.Rho_Scene;

   ------------------
   -- Create_Scene --
   ------------------

   overriding function Create_Scene
     (Demo : in out Triangles_Demo)
      return Rho.Scene.Rho_Scene
   is
      pragma Unreferenced (Demo);
      use Rho;
      Entity : Rho.Entity.Rho_Entity;
      Node   : Rho.Node.Rho_Node;
      Scene  : Rho.Scene.Rho_Scene;
   begin
      Rho.Entity.Rho_New (Entity);
      Entity.Begin_Operation (Rho.Render_Operation.Triangle_List);
      Entity.Vertex (-0.90, -0.90);
      Entity.Vertex (0.85, -0.90);
      Entity.Vertex (-0.90, 0.85);
      Entity.Vertex (0.90, -0.85);
      Entity.Vertex (0.90, 0.90);
      Entity.Vertex (-0.85, 0.90);
      Entity.End_Operation;

      Scene := Rho.Scene.Create_Scene;
      Node := Scene.Create_Node ("triangles");
      Node.Set_Entity (Entity);
      Scene.Active_Camera.Set_Position (0.0, 0.0, 3.0);
      Scene.Active_Camera.Look_At (0.0, 0.0, 0.0);
      Scene.Active_Camera.Frustum (-1.0, 1.0, -1.0, 1.0, 1.5, 20.0);
      return Scene;
   end Create_Scene;

   ---------------
   -- Triangles --
   ---------------

   function Triangles return Rho_Demo_Type is
   begin
      return new Triangles_Demo;
   end Triangles;

end Demos.Red_Book;
