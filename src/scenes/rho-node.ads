private with Ada.Containers.Vectors;

with Rho.Entity;
with Rho.Event;
with Rho.Float_Buffer;
with Rho.Matrices;
with Rho.Moveable;
with Rho.Object;
with Rho.Orientable;
with Rho.Rectangle;
with Rho.Renderable;
with Rho.Render_Target;
with Rho.Shaders.Values;

limited with Rho.Context;

package Rho.Node is

   type Rho_Node_Record is
     new Rho.Object.Rho_Object_Record
     and Rho.Renderable.Rho_Renderable
     and Rho.Orientable.Rho_Orientable
     and Rho.Orientable.Rho_Moveable_Orientable
     and Rho.Event.Rho_Event_Source
   with private;

   type Rho_Node is access all Rho_Node_Record'Class;

   procedure Rho_New
     (Node    : in out Rho_Node;
      Context : not null access Rho.Context.Rho_Context_Record'Class;
      Name    : String);

   procedure Initialize
     (Node    : in out Rho_Node_Record;
      Context : not null access Rho.Context.Rho_Context_Record'Class;
      Name    : String);

   function Create
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Name    : String)
      return Rho_Node;

   overriding function Loaded
     (Node : Rho_Node_Record)
      return Boolean;

   overriding procedure Load (Node : in out Rho_Node_Record);

   overriding procedure Before_Render
     (Node   : in out Rho_Node_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   overriding procedure After_Render
     (Node   : in out Rho_Node_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   overriding procedure Execute_Render
     (Node   : in out Rho_Node_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   procedure Set_Visible
     (Node    : in out Rho_Node_Record;
      Visible : Boolean);

   function Screen_Rectangle
     (Node : Rho_Node_Record)
      return Rho.Rectangle.Rho_Rectangle;

   function Context
     (Node : Rho_Node_Record'Class)
      return access Rho.Context.Rho_Context_Record'Class;

   function Entity
     (Node : Rho_Node_Record)
      return Rho.Entity.Rho_Entity;

   procedure Set_Entity
     (Node     : in out Rho_Node_Record;
      Entity   : not null access Rho.Entity.Rho_Entity_Record'Class);

   overriding procedure Set_Position
     (Node     : in out Rho_Node_Record;
      Position : in Rho.Matrices.Vector);

   overriding function Position
     (Node    : in     Rho_Node_Record)
      return Rho.Matrices.Vector_4;

   overriding procedure Set_Orientation
     (Node     : in out Rho_Node_Record;
      Rotation : in Rho.Matrices.Matrix_3);

   overriding function Orientation
     (Node    : in     Rho_Node_Record)
      return Rho.Matrices.Matrix_3;

   overriding procedure On_Event
     (Node    : not null access Rho_Node_Record;
      Event   : Rho.Event.Rho_Event);

   function Transformation_Matrix
     (Node : Rho_Node_Record)
      return Rho.Matrices.Matrix_4;

   function Final_Transformation_Matrix
     (Node : Rho_Node_Record)
      return Rho.Matrices.Matrix_4;

   function Inverse_Transformation_Matrix
     (Node : Rho_Node_Record)
      return Rho.Matrices.Matrix_4;

   procedure Set_Billboard
     (Node    : in out Rho_Node_Record;
      Enabled : Boolean);

   procedure Fixed_Pixel_Size
     (Node     : in out Rho_Node_Record;
      Width    : Non_Negative_Float;
      Height   : Non_Negative_Float;
      X_Offset : Rho_Float := 0.0;
      Y_Offset : Rho_Float := 0.0);
   --  scale the node so that it fits into width, height.
   --  offset the node on the screen

   procedure Set_Instanced
     (Node           : in out Rho_Node_Record;
      Instance_Count : Positive);

   type Float_Instance_Value_Function is access
     function (Child_Index : Positive)
               return Rho_Float;

   procedure Set_Instance_Value
     (Node            : in out Rho_Node_Record;
      Attribute_Value : in Rho.Shaders.Values.Rho_Attribute_Value;
      Access_Function : Float_Instance_Value_Function);

   type Vector_3_Instance_Value_Function is access
     function (Child_Index : Positive)
               return Rho.Matrices.Vector_3;

   procedure Set_Instance_Value
     (Node            : in out Rho_Node_Record;
      Attribute_Value : in Rho.Shaders.Values.Rho_Attribute_Value;
      Access_Function : Vector_3_Instance_Value_Function);

   procedure Scale
     (Node    : in out Rho_Node_Record'Class;
      X, Y, Z : Rho_Float);

   procedure Scale
     (Node    : in out Rho_Node_Record'Class;
      XYZ     : Rho_Float);

   function Create_Child
     (Parent : not null access Rho_Node_Record;
      Name   : String)
      return Rho_Node;

   function Create_Child
     (Parent : not null access Rho_Node_Record'Class)
      return Rho_Node;

   procedure Append_Child
     (Parent : not null access Rho_Node_Record;
      Child  : not null access Rho_Node_Record'Class);

   procedure Delete_Child
     (Parent : in out Rho_Node_Record;
      Child  : not null access Rho_Node_Record'Class);

   procedure Delete_Child
     (Parent : in out Rho_Node_Record;
      Name   : String);

   function Find_Child
     (Parent : not null access Rho_Node_Record'Class;
      Name   : String)
      return Rho_Node;

   function Child_Node
     (Parent     : Rho_Node_Record;
      Child_Name : String)
      return Rho_Node;

   function Child_Node
     (Parent      : Rho_Node_Record;
      Child_Index : Positive)
      return Rho_Node;

   function Has_Parent_Node
     (Node : Rho_Node_Record'Class)
      return Boolean;

   function Parent_Node
     (Node : Rho_Node_Record'Class)
      return Rho_Node;

   function Changed
     (Node : Rho_Node_Record'Class)
      return Boolean;

   procedure Set_Changed
     (Node : in out Rho_Node_Record'Class);

   procedure Clear_Changed
     (Node : in out Rho_Node_Record'Class);

   procedure Set_Event_Manager
     (Node    : in out Rho_Node_Record'Class;
      Manager : access Rho.Event.Rho_Event_Manager'Class);

   function Event_Manager
     (Node : Rho_Node_Record'Class)
      return access Rho.Event.Rho_Event_Manager'Class;

   type Node_Click_Handler is access
     procedure (Node : Rho_Node);

   procedure Add_Click_Handler
     (Node    : not null access Rho_Node_Record'Class;
      Handler : Node_Click_Handler);

private

   package Node_Vectors is
     new Ada.Containers.Vectors (Positive, Rho_Node);

   type Bound_Vertex_Value is
      record
         Value          : Rho.Shaders.Values.Rho_Attribute_Value;
         Buffer         : Rho.Float_Buffer.Rho_Float_Buffer;
         Start          : Positive;
         Component_Size : Positive;
      end record;

   package Bound_Vertex_Vectors is
     new Ada.Containers.Vectors (Positive, Bound_Vertex_Value);

   subtype Instanced_Vertex_Attribute_Length is
     Positive range 1 .. 4;

   type Instanced_Vertex_Value
     (Length : Instanced_Vertex_Attribute_Length := 1)
   is
      record
         Value  : Rho.Shaders.Values.Rho_Attribute_Value;
         Buffer : Rho.Float_Buffer.Rho_Float_Buffer;
         case Length is
            when 1 =>
               Scalar_Fn   : Float_Instance_Value_Function;
            when 2 =>
               null;
            when 3 =>
               Vector_3_Fn : Vector_3_Instance_Value_Function;
            when 4 =>
               null;
         end case;
      end record;

   package Instanced_Vertex_Value_Vectors is
     new Ada.Containers.Vectors (Positive, Instanced_Vertex_Value);

   type Rho_Node_Record is
     new Rho.Object.Rho_Object_Record
     and Rho.Renderable.Rho_Renderable
     and Rho.Orientable.Rho_Orientable
     and Rho.Orientable.Rho_Moveable_Orientable
     and Rho.Event.Rho_Event_Source with
      record
         Instanced           : Boolean := False;
         Loaded              : Boolean := False;
         Translated          : Boolean := False;
         Rotated             : Boolean := False;
         Scaled              : Boolean := False;
         Pixel_Scaled        : Boolean := False;
         Billboard           : Boolean := False;
         Bound               : Boolean := False;
         Visible             : Boolean := True;
         Node_Identity       : Boolean := True;
         Parent_Identity     : Boolean := True;
         View_Matrix_Cached  : Boolean := False;
         Clear_Child_Cache   : Boolean := True;
         Changed             : Boolean;
         Context             : access Rho.Context.Rho_Context_Record'Class;
         Entity              : Rho.Entity.Rho_Entity;
         Parent              : Rho_Node;
         Children            : Node_Vectors.Vector;
         Position            : Rho.Matrices.Vector_4 := (0.0, 0.0, 0.0, 1.0);
         Screen_Rectangle    : Rho.Rectangle.Rho_Rectangle;
         Orientation         : Rho.Matrices.Matrix_3 :=
                                 ((1.0, 0.0, 0.0),
                                  (0.0, 1.0, 0.0),
                                  (0.0, 0.0, 1.0));
         Scale               : Rho.Matrices.Vector_3 := (1.0, 1.0, 1.0);
         Model_View_Matrix   : Rho.Matrices.Matrix_4;
         Pixel_Scale_Width   : Non_Negative_Float;
         Pixel_Scale_Height  : Non_Negative_Float;
         Pixel_Offset_X      : Rho_Float;
         Pixel_Offset_Y      : Rho_Float;
         Instance_Count      : Natural := 0;
         Bound_Vertices      : Bound_Vertex_Vectors.Vector;
         Instanced_Vertices  : Instanced_Vertex_Value_Vectors.Vector;
         Local_Event_Manager : access Rho.Event.Rho_Event_Manager'Class;
         On_Click            : Node_Click_Handler;
      end record;

   procedure Load_Instanced_Attributes
     (Node : in out Rho_Node_Record'Class);

   procedure Check_Identity (Node : in out Rho_Node_Record'Class);

   function Context
     (Node : Rho_Node_Record'Class)
      return access Rho.Context.Rho_Context_Record'Class
   is (Node.Context);

end Rho.Node;
