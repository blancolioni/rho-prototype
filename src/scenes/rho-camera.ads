with Rho.Float_Arrays;
with Rho.Matrices;
with Rho.Node;
with Rho.Rectangle;
with Rho.Render_Target;
with Rho.Viewport;

limited with Rho.Context;

package Rho.Camera is

   type View_Volume_Shape is
     (Frustum, Orthographic, Custom);

   type Rho_Camera_Record is
     new Rho.Node.Rho_Node_Record with private;

   procedure Activate
     (Camera : in out Rho_Camera_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   procedure Set_Viewport
     (Camera    : in out Rho_Camera_Record'Class;
      Viewport  : Rho.Viewport.Rho_Viewport);

   function Viewport
     (Camera : Rho_Camera_Record'Class)
      return Rho.Viewport.Rho_Viewport;

   procedure Set_Shape
     (Camera : in out Rho_Camera_Record'Class;
      Shape  : View_Volume_Shape);

   procedure Frustum
     (Camera      : in out Rho_Camera_Record'Class;
      Left, Right : Rho_Float;
      Bottom, Top : Rho_Float;
      Near, Far   : Rho_Float);

   procedure Perspective
     (Camera       : in out Rho_Camera_Record'Class;
      Fovy         : Rho_Float;
      Aspect_Ratio : Rho_Float;
      Near, Far    : Rho_Float);

   procedure Perspective
     (Camera       : in out Rho_Camera_Record'Class;
      Fovy         : Rho_Float;
      Near, Far    : Rho_Float);

   procedure Orthographic
     (Camera       : in out Rho_Camera_Record'Class;
      Left, Right  : Rho_Float;
      Bottom, Top  : Rho_Float;
      Near, Far    : Rho_Float);

   procedure Orthographic
     (Camera       : in out Rho_Camera_Record'Class;
      Near, Far    : Rho_Float);

   function Near
     (Camera : Rho_Camera_Record'Class)
      return Rho_Float;

   function Far
     (Camera : Rho_Camera_Record'Class)
      return Rho_Float;

   procedure Set_Near
     (Camera : in out Rho_Camera_Record'Class;
      Near   : Rho_Float);

   procedure Set_Far
     (Camera : in out Rho_Camera_Record'Class;
      Far    : Rho_Float);

   procedure Set_Near_Clip
     (Camera : in out Rho_Camera_Record'Class;
      Clip   : Rho.Rectangle.Rho_Rectangle);

   procedure Set_Projection_Matrix
     (Camera : in out Rho_Camera_Record'Class;
      Matrix : Rho.Matrices.Matrix_4);

   function Projection_Matrix
     (Camera : Rho_Camera_Record'Class)
      return Rho.Matrices.Matrix_4;

   type Rho_Camera is access all Rho_Camera_Record'Class;

   overriding procedure Execute_Render
     (Camera : in out Rho_Camera_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is null;

   overriding procedure After_Render
     (Camera : in out Rho_Camera_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is null;

   overriding procedure Before_Render
     (Camera : in out Rho_Camera_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is null;

   function Create
     (Context : not null access Rho.Context.Rho_Context_Record'Class)
      return Rho_Camera;

private

   type Rho_Camera_Record is
     new Rho.Node.Rho_Node_Record with
      record
         Shape                : View_Volume_Shape := Frustum;
         Viewport             : Rho.Viewport.Rho_Viewport;
         Clip                 : Rho.Rectangle.Rho_Rectangle;
         Left, Right          : Rho_Float;
         Bottom, Top          : Rho_Float;
         Near, Far            : Rho_Float;
         Fovy                 : Rho_Float;
         Projection_Matrix    : Rho.Matrices.Matrix_4 :=
                                  Rho.Float_Arrays.Unit_Matrix (4);
         Current_Aspect_Ratio : Rho_Float := 0.0;
         Perspective_Changed  : Boolean := True;
         Auto_Perspective     : Boolean := False;
      end record;

   overriding procedure Set_Orientation
     (Camera   : in out Rho_Camera_Record;
      Rotation : in Rho.Matrices.Matrix_3);

   overriding procedure Set_Position
     (Camera : in out Rho_Camera_Record;
      Position : in Rho.Matrices.Vector);

end Rho.Camera;
