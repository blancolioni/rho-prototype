with Rho.Context;
with Rho.Rendering;

package body Rho.Camera is

   --------------
   -- Activate --
   --------------

   procedure Activate
     (Camera : in out Rho_Camera_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
      use Rho.Matrices, Rho.Float_Arrays;
      Renderer : constant Rho.Rendering.Rho_Renderer :=
                   Camera.Context.Renderer;
   begin
      Target.Set_Viewport (Camera.Viewport);
      Renderer.Matrix_Mode (Rho.Matrices.Projection);

      if Camera.Changed or else
        (Camera.Auto_Perspective
         and then Camera.Viewport.Aspect_Ratio /= Camera.Current_Aspect_Ratio)
      then
         if Camera.Auto_Perspective then
            case Camera.Shape is
            when Frustum | Custom =>
               Camera.Projection_Matrix :=
                 Perspective_Matrix
                   (Fovy         => Camera.Fovy,
                    Aspect_Ratio => Camera.Viewport.Aspect_Ratio,
                    Near         => Camera.Near,
                    Far          => Camera.Far);
            when Orthographic =>
               Camera.Projection_Matrix :=
                 Orthographic_Matrix
                   (0.0, Camera.Viewport.Width,
                    0.0, Camera.Viewport.Height,
                    Camera.Near, Camera.Far);
            end case;
            Camera.Context.Renderer.Clear_Matrix_Saved;
--              Target.Clear_Matrix_Saved;
         else
            if Camera.Perspective_Changed then
               Camera.Context.Renderer.Clear_Matrix_Saved;
               Camera.Perspective_Changed := False;
            end if;
         end if;
      end if;

      Renderer.Set_Current (Camera.Projection_Matrix);

      Renderer.Matrix_Mode (Rho.Matrices.Model_View);

      if Camera.Changed then
         Renderer.Load_Identity;

         declare
            M : Matrix_4 := Camera.Final_Transformation_Matrix;
         begin
            for I in 1 .. 3 loop
               for J in 1 .. 3 loop
                  if I /= J then
                     declare
                        T : constant Rho_Float := M (I, J);
                     begin
                        M (I, J) := M (J, I);
                        M (J, I) := T;
                     end;
                  end if;
               end loop;
               M (I, 4) := -M (I, 4);
            end loop;

            Renderer.Multiply (M);
         end;

         declare
            P : Vector_4 := (0.0, 0.0, 0.0, 1.0);
         begin
            P := Renderer.Current (Rho.Matrices.Model_View) * P;
            Target.Set_Camera_Position (P (1 .. 3));
         end;
      end if;

   end Activate;

   ------------
   -- Create --
   ------------

   function Create return Rho_Camera is
      Result : constant Rho_Camera := new Rho_Camera_Record;
   begin
      Result.Set_Position (0.0, 0.0, 0.0);
      Result.Set_Orientation (Rho.Float_Arrays.Unit_Matrix (3));
      return Result;
   end Create;

   ---------
   -- Far --
   ---------

   function Far
     (Camera : Rho_Camera_Record'Class)
      return Rho_Float
   is
   begin
      return Camera.Far;
   end Far;

   -------------
   -- Frustum --
   -------------

   procedure Frustum
     (Camera      : in out Rho_Camera_Record'Class;
      Left, Right : Rho_Float;
      Bottom, Top : Rho_Float;
      Near, Far   : Rho_Float)
   is
   begin
      Camera.Projection_Matrix :=
        Rho.Matrices.Frustum_Matrix
          (Left, Right, Bottom, Top, Near, Far);
      Camera.Set_Near (Near);
      Camera.Set_Far (Far);
      Camera.Left := Left;
      Camera.Right := Right;
      Camera.Bottom := Bottom;
      Camera.Top := Top;
      Camera.Shape := Frustum;
      Camera.Perspective_Changed := True;
      Camera.Auto_Perspective := False;
      Camera.Set_Changed;
   end Frustum;

   ----------
   -- Near --
   ----------

   function Near
     (Camera : Rho_Camera_Record'Class)
      return Rho_Float
   is
   begin
      return Camera.Near;
   end Near;

   ------------------
   -- Orthographic --
   ------------------

   procedure Orthographic
     (Camera      : in out Rho_Camera_Record'Class;
      Left, Right : Rho_Float;
      Bottom, Top : Rho_Float;
      Near, Far   : Rho_Float)
   is
   begin
      Camera.Projection_Matrix :=
        Rho.Matrices.Orthographic_Matrix
          (Left, Right, Bottom, Top, Near, Far);
      Camera.Set_Near (Near);
      Camera.Set_Far (Far);
      Camera.Left := Left;
      Camera.Right := Right;
      Camera.Bottom := Bottom;
      Camera.Top := Top;
      Camera.Shape := Orthographic;
      Camera.Perspective_Changed := True;
      Camera.Auto_Perspective := False;
      Camera.Set_Changed;
   end Orthographic;

   ------------------
   -- Orthographic --
   ------------------

   procedure Orthographic
     (Camera      : in out Rho_Camera_Record'Class;
      Near, Far   : Rho_Float)
   is
   begin
      Camera.Set_Near (Near);
      Camera.Set_Far (Far);
      Camera.Shape := Orthographic;
      Camera.Perspective_Changed := True;
      Camera.Auto_Perspective := True;
      Camera.Set_Changed;
   end Orthographic;

   -----------------
   -- Perspective --
   -----------------

   procedure Perspective
     (Camera       : in out Rho_Camera_Record'Class;
      Fovy         : Rho_Float;
      Aspect_Ratio : Rho_Float;
      Near, Far    : Rho_Float)
   is
   begin
      Camera.Projection_Matrix :=
        Rho.Matrices.Perspective_Matrix
          (Fovy, Aspect_Ratio, Near, Far);
      Camera.Perspective_Changed := True;
      Camera.Set_Changed;
   end Perspective;

   -----------------
   -- Perspective --
   -----------------

   procedure Perspective
     (Camera       : in out Rho_Camera_Record'Class;
      Fovy         : Rho_Float;
      Near, Far    : Rho_Float)
   is
   begin
      Camera.Auto_Perspective := True;
      Camera.Fovy := Fovy;
      Camera.Near := Near;
      Camera.Far := Far;
      Camera.Set_Changed;
   end Perspective;

   -----------------------
   -- Projection_Matrix --
   -----------------------

   function Projection_Matrix
     (Camera : Rho_Camera_Record'Class)
      return Rho.Matrices.Matrix_4
   is
   begin
      return Camera.Projection_Matrix;
   end Projection_Matrix;

   -------------
   -- Set_Far --
   -------------

   procedure Set_Far
     (Camera : in out Rho_Camera_Record'Class;
      Far    : Rho_Float)
   is
   begin
      Camera.Far := Far;
      Camera.Set_Changed;
   end Set_Far;

   --------------
   -- Set_Near --
   --------------

   procedure Set_Near
     (Camera : in out Rho_Camera_Record'Class;
      Near   : Rho_Float)
   is
   begin
      Camera.Near := Near;
      Camera.Set_Changed;
   end Set_Near;

   -------------------
   -- Set_Near_Clip --
   -------------------

   procedure Set_Near_Clip
     (Camera : in out Rho_Camera_Record'Class;
      Clip   : Rho.Rectangle.Rho_Rectangle)
   is
   begin
      Camera.Clip := Clip;
      Camera.Set_Changed;
   end Set_Near_Clip;

   ---------------------
   -- Set_Orientation --
   ---------------------

   overriding procedure Set_Orientation
     (Camera   : in out Rho_Camera_Record;
      Rotation : in Rho.Matrices.Matrix_3)
   is
   begin
      Rho.Node.Rho_Node_Record (Camera).Set_Orientation (Rotation);
   end Set_Orientation;

   ------------------
   -- Set_Position --
   ------------------

   overriding procedure Set_Position
     (Camera   : in out Rho_Camera_Record;
      Position : in Rho.Matrices.Vector)
   is
   begin
      Rho.Node.Rho_Node_Record (Camera).Set_Position (Position);
   end Set_Position;

   ---------------------------
   -- Set_Projection_Matrix --
   ---------------------------

   procedure Set_Projection_Matrix
     (Camera : in out Rho_Camera_Record'Class;
      Matrix : Rho.Matrices.Matrix_4)
   is
   begin
      Camera.Projection_Matrix := Matrix;
      Camera.Auto_Perspective := False;
      Camera.Set_Changed;
   end Set_Projection_Matrix;

   ---------------
   -- Set_Shape --
   ---------------

   procedure Set_Shape
     (Camera : in out Rho_Camera_Record'Class;
      Shape  : View_Volume_Shape)
   is
   begin
      Camera.Shape := Shape;
      Camera.Set_Changed;
   end Set_Shape;

   ------------------
   -- Set_Viewport --
   ------------------

   procedure Set_Viewport
     (Camera   : in out Rho_Camera_Record'Class;
      Viewport : Rho.Viewport.Rho_Viewport)
   is
   begin
      Camera.Viewport := Viewport;
      Camera.Set_Changed;
   end Set_Viewport;

   --------------
   -- Viewport --
   --------------

   function Viewport
     (Camera : Rho_Camera_Record'Class)
      return Rho.Viewport.Rho_Viewport
   is
   begin
      return Camera.Viewport;
   end Viewport;

end Rho.Camera;
