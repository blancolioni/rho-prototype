with GL;

with Maas;                               use Maas;

with Rho.Camera;
with Rho.Entity;
with Rho.Float_Arrays;
with Rho.Frame_Event;
with Rho.Light;
with Rho.Main;
with Rho.Matrices;
with Rho.Mesh;
with Rho.Mesh.Reader;
with Rho.Mouse;
with Rho.Node;
with Rho.Paths;
with Rho.Scene;
with Rho.Render_Window;

package body Cobra is

   type Double_Frame_Listener is
     new Rho.Frame_Event.Rho_Frame_Listener_Interface with
      record
         Count : Natural := 0;
         Roll  : Rho_Float := 0.0;
         Pitch : Rho_Float := 0.0;
         Node  : Rho.Node.Rho_Node;
         Camera : Rho.Camera.Rho_Camera;
         Spin   : Rho_Float := 0.0;
      end record;

   overriding procedure Frame_Started
     (Listener : in out Double_Frame_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event);

   -------------------
   -- Create_Window --
   -------------------

   procedure Create_Window is
      Scene : constant Rho.Scene.Rho_Scene := Rho.Scene.Create_Scene;
      Camera : constant Rho.Camera.Rho_Camera := Scene.Active_Camera;
      Node  : constant Rho.Node.Rho_Node := Scene.Create_Node ("cobra");
      Light  : Rho.Light.Rho_Light;
      Window : constant Rho.Render_Window.Rho_Render_Window :=
                 Rho.Main.Current_Renderer.Create_Top_Level_Window;
      Cobra_Mesh : constant Rho.Mesh.Rho_Mesh :=
                     Rho.Mesh.Reader.Read_Dat_File
                       (Rho.Paths.Config_Path
                        & "/cobra1_redux.dat");
      Cobra      : constant Rho.Entity.Rho_Entity :=
                     Cobra_Mesh.Create_Entity;
   begin

      GL.Enable_Debug;

      Window.Set_Wireframe (False);
      Window.Set_Back_Face_Removal (True);

      Camera.Set_Viewport (Window.Full_Viewport);
      Camera.Set_Position (0.0, 0.0, 200.0);
      Camera.Look_At (0.0, 1.0, 0.0, 0.0, 0.0, 0.0);
      Camera.Frustum (-15.0, 15.0, -15.0, 15.0, 50.0, 300.0);
      Rho.Light.Rho_New (Light, Rho.Light.Point);
      Light.Set_Position (0.0, 30.0, 30.0);
      Light.Set_Color (1.0, 1.0, 1.0, 1.0);
      Scene.Add_Light (Light);
      Node.Set_Entity (Cobra);
      Window.Set_Scene (Scene);

      declare
         Listener : constant Rho.Frame_Event.Rho_Frame_Listener :=
                      new Double_Frame_Listener'
                        (Roll => 0.0, Pitch => 0.0,
                         Node => Node, Count => 0,
                         Spin => 0.0,
                         Camera => Camera);
      begin
         Rho.Main.Add_Frame_Listener (Listener);
      end;

   end Create_Window;

   -------------------
   -- Frame_Started --
   -------------------

   overriding procedure Frame_Started
     (Listener : in out Double_Frame_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event)
   is
      use Rho.Float_Arrays;
      use Rho.Matrices;
      Rotation : constant Matrix_3 :=
                   Rotation_Matrix
                     (Listener.Spin, (0.0, 1.0, 0.0))
                   * Rotation_Matrix
                     (30.0, (1.0, 0.0, 0.0));
   begin
      Listener.Spin := Listener.Spin
        + 10.0 * Rho_Float (Event.Time_Since_Last_Event);
      Listener.Node.Set_Orientation (Rotation);
   end Frame_Started;

end Cobra;
