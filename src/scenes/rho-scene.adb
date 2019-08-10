with Ada.Calendar;

with Rho.Limits;
with Rho.Rectangle;
with Rho.Viewport;

with Rho.Logging;

package body Rho.Scene is

   -------------------
   -- Active_Camera --
   -------------------

   function Active_Camera
     (Scene : Rho_Scene_Record)
      return Rho.Camera.Rho_Camera
   is
   begin
      return Scene.Active_Camera;
   end Active_Camera;

   ---------------
   -- Add_Light --
   ---------------

   procedure Add_Light
     (Scene : in out Rho_Scene_Record;
      Light : Rho.Light.Rho_Light)
   is
   begin
      Scene.Lights.Append (Light);
      Light.Set_Index (Rho.Limits.Light_Index (Scene.Lights.Length));
   end Add_Light;

   --------------------
   -- Add_Transition --
   --------------------

   procedure Add_Transition
     (Scene      : in out Rho_Scene_Record'Class;
      Transition : not null access
        Rho.Transition.Rho_Transition_Record'Class)
   is
   begin
      Scene.Transitions.Append (Rho.Transition.Rho_Transition (Transition));
   end Add_Transition;

   -----------------
   -- Append_Node --
   -----------------

   procedure Append_Node
     (Scene : in out Rho_Scene_Record;
      Node  : not null access Rho.Node.Rho_Node_Record'Class)
   is
   begin
      Scene.Root_Node.Append_Child (Node);
   end Append_Node;

   ------------------
   -- Check_Events --
   ------------------

   procedure Check_Events (Scene : in out Rho_Scene_Record'Class) is
      use Rho.Mouse;
      Mouse : constant Rho_Mouse_State := Current_Mouse;
   begin
      if Mouse /= null then
         for Button in Mouse_Button loop
            if Mouse.State.Button (Button)
              /= Scene.Last_Mouse.Button (Button)
            then
               case Mouse.State.Button (Button) is
               when Down =>
                  Scene.Emit
                    (Rho.Event.Mouse_Button_Press
                       (Button, Mouse.State.X, Mouse.State.Y));
               when Up =>
                  Scene.Emit
                    (Rho.Event.Mouse_Button_Release
                       (Button, Mouse.State.X, Mouse.State.Y));
                  if Button = Left then
                     Rho.Logging.Put ("click: ");
                     Rho.Logging.Put (Mouse.State.X);
                     Rho.Logging.Put (", ");
                     Rho.Logging.Put (Mouse.State.Y);
                     Rho.Logging.New_Line;
                     Scene.Emit
                       (Rho.Event.Mouse_Button_Click
                          (Button, Mouse.State.X, Mouse.State.Y));
                  end if;
               end case;
            end if;
         end loop;

         Scene.Last_Mouse := Mouse.State;
      end if;
   end Check_Events;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (Scene : in out Rho_Scene_Record;
      Name  : in     String)
      return Rho.Node.Rho_Node
   is
   begin
      return Scene.Root_Node.Create_Child (Name);
   end Create_Node;

   ------------------
   -- Create_Scene --
   ------------------

   function Create_Scene
     (Context : not null access Rho.Context.Rho_Context_Record'Class)
      return Rho_Scene
   is
      Result : constant Rho_Scene := new Rho_Scene_Record;
   begin
      Result.Context := Context;
      Result.Root_Node := new Rho.Node.Rho_Node_Record;
      Result.Root_Node.Initialize (Context, "root");
      Result.Root_Node.Set_Event_Manager (Result);

      Result.Active_Camera := Rho.Camera.Create (Context);
      Result.Active_Camera.Set_Position (0.0, 0.0, 0.0);
      Result.Active_Camera.Set_Orientation
        (0.0, 0.0, 0.0, -1.0);
      Result.Default_Camera := Result.Active_Camera;
      Result.Cameras.Append (Result.Active_Camera);
      Result.Root_Node.Append_Child (Result.Active_Camera);

--        Result.Camera.Frustum
--          (Left   => -1.0,
--           Right  => 1.0,
--           Bottom => -1.0,
--           Top    => 1.0,
--           Near   => 0.0,
--           Far    => 150.0);
      return Result;
   end Create_Scene;

   --------------------
   -- Default_Camera --
   --------------------

   function Default_Camera
     (Scene : Rho_Scene_Record)
      return Rho.Camera.Rho_Camera
   is
   begin
      return Scene.Default_Camera;
   end Default_Camera;

   ----------
   -- Emit --
   ----------

   procedure Emit
     (Scene : in out Rho_Scene_Record'Class;
      Event : Rho.Event.Rho_Event)
   is
   begin
      for Source of Scene.Event_Sources (Rho.Event.Get_Signal (Event)) loop
         if Rho.Rectangle.Contains_Point
           (Source.Node.Screen_Rectangle,
            Rho.Event.Event_X (Event),
            Rho.Event.Event_Y (Event))
         then
            Source.Node.On_Event (Event);
         end if;
      end loop;
   end Emit;

   --------------------
   -- Execute_Render --
   --------------------

   overriding procedure Execute_Render
     (Item : in out Rho_Scene_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
      use Ada.Calendar;
      Start : constant Time := Clock;
   begin

      if not Item.Loaded then
         Rho_Scene_Record'Class (Item).Load;
         Item.Loaded := True;
      end if;

      declare
         Old_List : constant Active_Transition_Lists.List := Item.Transitions;
      begin
         Item.Transitions.Clear;

         for Transition of Old_List loop
            if not Transition.Complete then
               Transition.Update;
               Item.Transitions.Append (Transition);
            end if;
         end loop;
      end;

      Item.Check_Events;

      for Light of Item.Lights loop
         Light.Render (Target);
      end loop;

      Item.Active_Camera.Activate (Target);

      if Item.Active_Camera.Changed then
         Item.Root_Node.Set_Changed;
         Item.Active_Camera.Clear_Changed;
      end if;

      Item.Root_Node.Render (Target);

      Item.Render_Time := Clock - Start;

   end Execute_Render;

   --------------
   -- Get_Node --
   --------------

   function Get_Node
     (Scene : Rho_Scene_Record;
      Name  : String)
      return Rho.Node.Rho_Node
   is
   begin
      return Scene.Root_Node.Find_Child (Name);
   end Get_Node;

   ----------------------
   -- Last_Render_Time --
   ----------------------

   function Last_Render_Time
     (Scene : Rho_Scene_Record'Class)
      return Duration
   is
   begin
      return Scene.Render_Time;
   end Last_Render_Time;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Item   : in out Rho_Scene_Record)
   is
   begin
      if not Item.Loaded then
         Item.Loaded := True;
         Item.Root_Node.Load;
      end if;
   end Load;

   ------------
   -- Loaded --
   ------------

   overriding function Loaded (Scene : Rho_Scene_Record) return Boolean is
   begin
      return Scene.Loaded;
   end Loaded;

   ---------------------------
   -- Register_Event_Source --
   ---------------------------

   overriding procedure Register_Event_Source
     (Scene   : in out Rho_Scene_Record;
      Source  : not null access Rho.Event.Rho_Event_Source'Class;
      Signal  : Rho.Event.Rho_Signal)
   is
   begin
      Scene.Event_Sources (Signal).Append
        ((Node => Rho.Node.Rho_Node (Source)));
   end Register_Event_Source;

   ----------------
   -- Use_Camera --
   ----------------

   procedure Use_Camera
     (Scene  : in out Rho_Scene_Record'Class;
      Camera : Rho.Camera.Rho_Camera)
   is
      use type Rho.Camera.Rho_Camera;
      use type Rho.Viewport.Rho_Viewport;
   begin
      if Camera.Viewport = null then
         Camera.Set_Viewport (Scene.Active_Camera.Viewport);
      end if;
      if Camera /= Scene.Active_Camera then
         Camera.Set_Changed;
      end if;
      Scene.Active_Camera := Camera;

   end Use_Camera;

   ------------------------
   -- Use_Default_Camera --
   ------------------------

   procedure Use_Default_Camera
     (Scene  : in out Rho_Scene_Record'Class)
   is
   begin
      Scene.Use_Camera (Scene.Default_Camera);
   end Use_Default_Camera;

end Rho.Scene;
