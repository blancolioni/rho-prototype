private with Ada.Containers.Doubly_Linked_Lists;

with Rho.Render_Target;
with Rho.Scene;

with Rho.Toolkit;

package Rho.Scene_Renderer is

   type Rho_Scene_Renderer_Record is
     abstract new Rho.Render_Target.Rho_Render_Target_Record
   with private;

   type Rho_Scene_Renderer is access all Rho_Scene_Renderer_Record'Class;

   overriding procedure Render
     (Renderer : not null access Rho_Scene_Renderer_Record);

   function Scene
     (Renderer : Rho_Scene_Renderer_Record'Class)
      return Rho.Scene.Rho_Scene;

   procedure Set_Scene
     (Renderer : in out Rho_Scene_Renderer_Record'Class;
      Scene    : Rho.Scene.Rho_Scene);

   procedure Add_Top_Level
     (Renderer  : in out Rho_Scene_Renderer_Record'Class;
      Top_Level : not null access Rho.Toolkit.Rho_Top_Level_Interface'Class);

private

   package List_Of_Top_Levels is
     new Ada.Containers.Doubly_Linked_Lists
       (Rho.Toolkit.Rho_Top_Level, Rho.Toolkit."=");

   type Rho_Scene_Renderer_Record is
     abstract new Rho.Render_Target.Rho_Render_Target_Record with
      record
         Current_Scene : Rho.Scene.Rho_Scene;
         Top_Levels    : List_Of_Top_Levels.List;
      end record;

end Rho.Scene_Renderer;
