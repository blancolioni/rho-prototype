with Maas;                               use Maas;

with Rho.Camera;
with Rho.Entity;
with Rho.Main;
with Rho.Node;
with Rho.Paths;
with Rho.Scene;
with Rho.Render_Operation;
with Rho.Render_Window;
with Rho.Texture;

package body Simple_Texture is

   -------------------
   -- Create_Window --
   -------------------

   procedure Create_Window is
      Scene : constant Rho.Scene.Rho_Scene := Rho.Scene.Create_Scene;
      Camera : constant Rho.Camera.Rho_Camera := Scene.Active_Camera;
      Node  : constant Rho.Node.Rho_Node := Scene.Create_Node ("wire_cube");
      Window : constant Rho.Render_Window.Rho_Render_Window :=
                 Rho.Main.Current_Renderer.Create_Top_Level_Window;
      Entity : Rho.Entity.Rho_Entity;
      Texture  : constant Rho.Texture.Rho_Texture :=
                   Rho.Texture.Create_From_Png ("earth",
                                               Rho.Paths.Config_Path
                                               & "/earth.png");
   begin
      Rho.Entity.Rho_New (Entity);
      Entity.Begin_Operation (Rho.Render_Operation.Quad_List);

      Entity.Texture_Coordinate (0.0, 0.0);
      Entity.Vertex (-1.0, -1.0, 0.0);

      Entity.Texture_Coordinate (1.0, 0.0);
      Entity.Vertex (1.0, -1.0, 0.0);

      Entity.Texture_Coordinate (1.0, 1.0);
      Entity.Vertex (1.0, 1.0, 0.0);

      Entity.Texture_Coordinate (0.0, 1.0);
      Entity.Vertex (-1.0, 1.0, 0.0);

      Entity.End_Operation;

      Entity.Set_Texture (Texture);

      Camera.Set_Viewport (Window.Full_Viewport);
      Camera.Set_Position (0.0, 0.0, 5.0);
      Camera.Set_Orientation (0.0, 0.0, 1.0, 0.0);
      Camera.Look_At (0.0, 1.0, 0.0, 0.0, 0.0, 0.0);
      Camera.Frustum (-1.0, 1.0, -1.0, 1.0, 1.5, 20.0);
      Node.Set_Entity (Entity);
      Window.Set_Scene (Scene);

   end Create_Window;

end Simple_Texture;
