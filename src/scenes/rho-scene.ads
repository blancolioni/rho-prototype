private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

with Rho.Renderable;
with Rho.Render_Target;

with Rho.Camera;
with Rho.Event;
with Rho.Light;
with Rho.Mouse;
with Rho.Node;
with Rho.Transition;

package Rho.Scene is

   type Rho_Scene_Record is
     new Rho.Renderable.Rho_Renderable
     and Rho.Event.Rho_Event_Manager
   with private;

   type Rho_Scene is access all Rho_Scene_Record'Class;

   overriding function Loaded (Scene : Rho_Scene_Record) return Boolean;

   overriding procedure Load (Item : in out Rho_Scene_Record);

   overriding procedure Execute_Render
     (Item : in out Rho_Scene_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   overriding procedure Register_Event_Source
     (Scene   : in out Rho_Scene_Record;
      Source  : not null access Rho.Event.Rho_Event_Source'Class;
      Signal  : Rho.Event.Rho_Signal);

   function Active_Camera
     (Scene : Rho_Scene_Record)
      return Rho.Camera.Rho_Camera;

   function Default_Camera
     (Scene : Rho_Scene_Record)
      return Rho.Camera.Rho_Camera;

   procedure Use_Camera
     (Scene  : in out Rho_Scene_Record'Class;
      Camera : Rho.Camera.Rho_Camera);

   procedure Use_Default_Camera
     (Scene  : in out Rho_Scene_Record'Class);

   procedure Add_Light
     (Scene : in out Rho_Scene_Record;
      Light : Rho.Light.Rho_Light);

   function Create_Node
     (Scene : in out Rho_Scene_Record;
      Name  : in     String)
      return Rho.Node.Rho_Node;

   procedure Append_Node
     (Scene : in out Rho_Scene_Record;
      Node  : not null access Rho.Node.Rho_Node_Record'Class);

   function Get_Node
     (Scene : Rho_Scene_Record;
      Name  : String)
      return Rho.Node.Rho_Node;

   procedure Add_Transition
     (Scene : in out Rho_Scene_Record'Class;
      Transition : not null access
        Rho.Transition.Rho_Transition_Record'Class);

   function Create_Scene return Rho_Scene;

   function Last_Render_Time
     (Scene : Rho_Scene_Record'Class)
      return Duration;

private

   package Light_Vectors is
     new Ada.Containers.Vectors
       (Positive, Rho.Light.Rho_Light, Rho.Light."=");

   package Camera_Vectors is
     new Ada.Containers.Vectors
       (Positive, Rho.Camera.Rho_Camera, Rho.Camera."=");

   package Active_Transition_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Rho.Transition.Rho_Transition, Rho.Transition."=");

   type Scene_Event_Source is
      record
         Node          : Rho.Node.Rho_Node;
      end record;

   package Event_Source_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Scene_Event_Source);

   type Signal_Event_Sources is
     array (Rho.Event.Rho_Signal) of Event_Source_Lists.List;

   type Rho_Scene_Record is
     new Rho.Renderable.Rho_Renderable
     and Rho.Event.Rho_Event_Manager with
      record
         Loaded         : Boolean := False;
         Lights         : Light_Vectors.Vector;
         Cameras        : Camera_Vectors.Vector;
         Active_Camera  : Rho.Camera.Rho_Camera;
         Default_Camera : Rho.Camera.Rho_Camera;
         Root_Node      : Rho.Node.Rho_Node;
         Render_Time    : Duration;
         Transitions    : Active_Transition_Lists.List;
         Event_Sources  : Signal_Event_Sources;
         Current_Node   : Rho.Node.Rho_Node;
         Last_Mouse     : Rho.Mouse.Mouse_State;
      end record;

   procedure Check_Events (Scene : in out Rho_Scene_Record'Class);

   procedure Emit
     (Scene : in out Rho_Scene_Record'Class;
      Event : Rho.Event.Rho_Event);

end Rho.Scene;
