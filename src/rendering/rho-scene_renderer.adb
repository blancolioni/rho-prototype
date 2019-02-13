package body Rho.Scene_Renderer is

   -------------------
   -- Add_Top_Level --
   -------------------

   procedure Add_Top_Level
     (Renderer  : in out Rho_Scene_Renderer_Record'Class;
      Top_Level : not null access
        Rho.Toolkit.Rho_Top_Level_Interface'Class)
   is
   begin
      Renderer.Top_Levels.Append (Rho.Toolkit.Rho_Top_Level (Top_Level));
   end Add_Top_Level;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Renderer : not null access Rho_Scene_Renderer_Record)
   is
      use type Rho.Scene.Rho_Scene;
   begin
      Renderer.Lock;

      Rho_Scene_Renderer_Record'Class (Renderer.all).Before_Render;
      if Renderer.Scene /= null then
         Renderer.Scene.Render (Renderer);
      end if;
      for Control of Renderer.Top_Levels loop
         Control.Render (Renderer);
      end loop;
      Rho_Scene_Renderer_Record'Class (Renderer.all).After_Render;
      Renderer.Unlock;
   end Render;

   -----------
   -- Scene --
   -----------

   function Scene
     (Renderer : Rho_Scene_Renderer_Record'Class)
      return Rho.Scene.Rho_Scene
   is
   begin
      return Renderer.Current_Scene;
   end Scene;

   ---------------
   -- Set_Scene --
   ---------------

   procedure Set_Scene
     (Renderer : in out Rho_Scene_Renderer_Record'Class;
      Scene    : Rho.Scene.Rho_Scene)
   is
   begin
      Renderer.Lock;
      Renderer.Current_Scene := Scene;
      Scene.Active_Camera.Set_Viewport (Renderer.Full_Viewport);
      Renderer.Unlock;
   end Set_Scene;

end Rho.Scene_Renderer;
