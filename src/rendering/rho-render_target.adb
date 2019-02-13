package body Rho.Render_Target is

   protected Render_Lock is
      entry Begin_Operation;
      procedure End_Operation;
      function Active return Boolean;
   private
      Is_Active : Boolean := False;
   end Render_Lock;

   ---------------------
   -- Activate_Shader --
   ---------------------

   procedure Activate_Shader
     (Target : in out Rho_Render_Target_Record)
   is
      use type Rho.Shader.Rho_Shader;
   begin
      if Target.Active_Shader
        and then Target.Current_Shader /= Target.Shaders.First_Element
      then
         Target.Current_Shader.Deactivate;
         Target.Active_Shader := False;
      end if;

      if not Target.Active_Shader
        or else Target.Current_Shader
          /= Target.Shaders.First_Element
      then
         Target.Clear_Matrix_Saved;
         Target.Active_Shader := True;
         Target.Current_Shader := Target.Shaders.First_Element;
         Target.Current_Shader.Activate;
      end if;
   end Activate_Shader;

   ------------------
   -- Active_Light --
   ------------------

   function Active_Light
     (Target : Rho_Render_Target_Record;
      Index  : Rho.Limits.Light_Index)
      return Boolean
   is
   begin
      return Target.Current_Lights (Index).Active;
   end Active_Light;

   ------------------
   -- After_Resize --
   ------------------

   procedure After_Resize
     (Target : not null access Rho_Render_Target_Record'Class)
   is
   begin
      if Target.Resize_Handler /= null then
         Target.Resize_Handler (Target);
      end if;
   end After_Resize;

   -----------------------
   -- Back_Face_Removal --
   -----------------------

   function Back_Face_Removal
     (Target  : Rho_Render_Target_Record)
      return Boolean
   is
   begin
      return Target.Back_Face_Removal;
   end Back_Face_Removal;

   -----------
   -- Blend --
   -----------

   procedure Blend
     (Target               : in out Rho_Render_Target_Record'Class;
      Source_Function      : Rho_Blend_Function;
      Destination_Function : Rho_Blend_Function)
   is
   begin
      Target.Blend (True, Source_Function, Destination_Function);
   end Blend;

   ---------------------
   -- Camera_Position --
   ---------------------

   function Camera_Position
     (Target : Rho_Render_Target_Record)
      return Rho.Matrices.Vector_3
   is
   begin
      return Target.Camera_Position;
   end Camera_Position;

   -----------------
   -- Clear_Color --
   -----------------

   function Clear_Color
     (Target : Rho_Render_Target_Record)
      return Rho.Color.Rho_Color
   is
   begin
      return Target.Clear_Color;
   end Clear_Color;

   ------------------------
   -- Clear_Matrix_Saved --
   ------------------------

   procedure Clear_Matrix_Saved
     (Target : in out Rho_Render_Target_Record'Class)
   is
   begin
      Target.Matrix_Saved := (others => False);
   end Clear_Matrix_Saved;

   --------------------
   -- Current_Shader --
   --------------------

   function Current_Shader
     (Target : Rho_Render_Target_Record)
      return Rho.Shader.Rho_Shader
   is
   begin
      if Target.Shaders.Is_Empty then
         return null;
      else
         return Target.Shaders.First_Element;
      end if;
   end Current_Shader;

   ----------------
   -- Depth_Test --
   ----------------

   function Depth_Test
     (Target  : Rho_Render_Target_Record)
      return Boolean
   is
   begin
      return Target.Z_Buffer;
   end Depth_Test;

   ---------------------
   -- Double_Buffered --
   ---------------------

   function Double_Buffered
     (Target  : Rho_Render_Target_Record)
      return Boolean
   is
   begin
      return Target.Double_Buffered;
   end Double_Buffered;

   -----------------------
   -- Enable_Point_Size --
   -----------------------

   procedure Enable_Point_Size
     (Target  : in out Rho_Render_Target_Record;
      Enabled : Boolean)
   is
   begin
      Target.Point_Size := Enabled;
   end Enable_Point_Size;

   -------------
   -- Frustum --
   -------------

   procedure Frustum
     (Target      : in out Rho_Render_Target_Record;
      Left, Right : Rho_Float;
      Bottom, Top : Rho_Float;
      Near, Far   : Rho_Float)
   is
      A : constant Rho_Float := (Right + Left) / (Right - Left);
      B : constant Rho_Float := (Top + Bottom) / (Top - Bottom);
      C : constant Rho_Float := (Far + Near) / (Far - Near);
      D : constant Rho_Float := 2.0 * Far * Near / (Far - Near);
   begin
      Rho_Render_Target_Record'Class (Target).Multiply
        (((2.0 * Near / (Right - Left), 0.0, A, 0.0),
         (0.0, 2.0 * Near / (Top - Bottom), B, 0.0),
         (0.0, 0.0, C, D),
         (0.0, 0.0, -1.0, 0.0)));
   end Frustum;

   -------------------
   -- Full_Viewport --
   -------------------

   function Full_Viewport
     (Target : Rho_Render_Target_Record)
      return Rho.Viewport.Rho_Viewport
   is
   begin
      return Target.Entire_Target;
   end Full_Viewport;

   -------------------------------
   -- Light_Ambient_Coefficient --
   -------------------------------

   function Light_Ambient_Coefficient
     (Target : Rho_Render_Target_Record;
      Index  : Rho.Limits.Light_Index)
      return Unit_Float
   is
   begin
      return Target.Current_Lights (Index).Ambient_Coefficient;
   end Light_Ambient_Coefficient;

   -----------------------------
   -- Light_Ambient_Intensity --
   -----------------------------

   function Light_Ambient_Intensity
     (Target : Rho_Render_Target_Record;
      Index  : Rho.Limits.Light_Index)
      return Rho.Color.Rho_Color
   is
   begin
      return Target.Current_Lights (Index).Ambient_Intensity;
   end Light_Ambient_Intensity;

   -----------------------
   -- Light_Attenuation --
   -----------------------

   function Light_Attenuation
     (Target : Rho_Render_Target_Record;
      Index  : Rho.Limits.Light_Index)
      return Unit_Float
   is
   begin
      return Target.Current_Lights (Index).Attenuation;
   end Light_Attenuation;

   -----------------------------
   -- Light_Diffuse_Intensity --
   -----------------------------

   function Light_Diffuse_Intensity
     (Target : Rho_Render_Target_Record;
      Index  : Rho.Limits.Light_Index)
      return Rho.Color.Rho_Color
   is
   begin
      return Target.Current_Lights (Index).Diffuse_Intensity;
   end Light_Diffuse_Intensity;

   --------------------
   -- Light_Position --
   --------------------

   function Light_Position
     (Target : Rho_Render_Target_Record;
      Index  : Rho.Limits.Light_Index)
      return Rho.Matrices.Vector_3
   is
   begin
      return Target.Current_Lights (Index).Position;
   end Light_Position;

   ----------
   -- Lock --
   ----------

   procedure Lock
     (Target : in out Rho_Render_Target_Record'Class)
   is
      pragma Unreferenced (Target);
   begin
      Render_Lock.Begin_Operation;
   end Lock;

   ------------------
   -- Matrix_Saved --
   ------------------

   function Matrix_Saved
     (Target : Rho_Render_Target_Record'Class;
      Matrix : Rho.Matrices.Matrix_Mode_Type)
      return Boolean
   is
   begin
      return Target.Matrix_Saved (Matrix);
   end Matrix_Saved;

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize
     (Target  : not null access Rho_Render_Target_Record'Class;
      Handler : Render_Target_Resize_Handler)
   is
   begin
      Target.Resize_Handler := Handler;
   end On_Resize;

   -----------
   -- Ortho --
   -----------

   procedure Ortho
     (Target      : in out Rho_Render_Target_Record;
      Left, Right : Rho_Float;
      Bottom, Top : Rho_Float;
      Near, Far   : Rho_Float)
   is
      TX : constant Rho_Float :=
             (Right + Left) / (Right - Left);
      TY : constant Rho_Float :=
             (Top + Bottom) / (Top - Bottom);
      TZ : constant Rho_Float :=
             (Far + Near) / (Far - Near);
   begin
      Rho_Render_Target_Record'Class (Target).Multiply
        (((2.0 / (Right - Left), 0.0, 0.0, -TX),
         (0.0, 2.0 / (Top - Bottom), 0.0, -TY),
         (0.0, 0.0, -2.0 / (Far - Near), -TZ),
         (0.0, 0.0, 0.0, 1.0)));
   end Ortho;

   ----------------
   -- Pop_Shader --
   ----------------

   procedure Pop_Shader
     (Target : in out Rho_Render_Target_Record)
   is
--        use type Rho.Shader.Rho_Shader;
--        Top : constant Rho.Shader.Rho_Shader :=
--                Target.Shaders.First_Element;
   begin
      Target.Shaders.Delete_First;
--        if Target.Shaders.Is_Empty
--          or else Top /= Target.Shaders.First_Element
--        then
--           Target.Clear_Matrix_Saved;
--           if Target.Active_Shader then
--              Top.Deactivate;
--              Target.Active_Shader := False;
--           end if;
--        end if;
   end Pop_Shader;

   -----------------
   -- Push_Shader --
   -----------------

   procedure Push_Shader
     (Target : in out Rho_Render_Target_Record;
      Shader : Rho.Shader.Rho_Shader)
   is
--        use type Rho.Shader.Rho_Shader;
--        Changed : constant Boolean :=
--                    Target.Shaders.Is_Empty;
                  --  or else Shader /= Target.Shaders.First_Element;
   begin
--        if Changed then
--           Target.Clear_Matrix_Saved;
--        end if;

--        if Changed and then Target.Active_Shader then
--           Target.Shaders.First_Element.Deactivate;
--           Target.Active_Shader := False;
--        end if;
      Target.Shaders.Insert (Target.Shaders.First, Shader);
   end Push_Shader;

   -----------------
   -- Render_Lock --
   -----------------

   protected body Render_Lock is

      function Active return Boolean is
      begin
         return Is_Active;
      end Active;

      entry Begin_Operation when not Active is
      begin
         Is_Active := True;
      end Begin_Operation;

      procedure End_Operation is
      begin
         Is_Active := False;
      end End_Operation;

   end Render_Lock;

   ---------------------------
   -- Set_Back_Face_Removal --
   ---------------------------

   procedure Set_Back_Face_Removal
     (Target  : in out Rho_Render_Target_Record;
      Enabled : Boolean)
   is
   begin
      Target.Back_Face_Removal := Enabled;
   end Set_Back_Face_Removal;

   -------------------------
   -- Set_Camera_Position --
   -------------------------

   procedure Set_Camera_Position
     (Target   : in out Rho_Render_Target_Record;
      Position : Rho.Matrices.Vector_3)
   is
   begin
      Target.Camera_Position := Position;
   end Set_Camera_Position;

   ---------------------
   -- Set_Clear_Color --
   ---------------------

   procedure Set_Clear_Color
     (Target      : in out Rho_Render_Target_Record;
      Clear_Color : Rho.Color.Rho_Color)
   is
   begin
      Target.Clear_Color := Clear_Color;
      Target.Viewport.Set_Color (Clear_Color);
   end Set_Clear_Color;

   --------------------
   -- Set_Depth_Test --
   --------------------

   procedure Set_Depth_Test
     (Target  : in out Rho_Render_Target_Record;
      Enabled : Boolean)
   is
   begin
      Target.Z_Buffer := Enabled;
   end Set_Depth_Test;

   -------------------------
   -- Set_Double_Buffered --
   -------------------------

   procedure Set_Double_Buffered
     (Target  : in out Rho_Render_Target_Record;
      Enabled : Boolean)
   is
   begin
      Target.Double_Buffered := Enabled;
   end Set_Double_Buffered;

   ---------------
   -- Set_Light --
   ---------------

   procedure Set_Light
     (Target              : not null access Rho_Render_Target_Record;
      Index               : Rho.Limits.Light_Index;
      Ambient_Intensity   : Rho.Color.Rho_Color := (0.0, 0.0, 0.0, 1.0);
      Diffuse_Intensity   : Rho.Color.Rho_Color := (1.0, 1.0, 1.0, 1.0);
      Specular_Intensity  : Rho.Color.Rho_Color := (1.0, 1.0, 1.0, 1.0);
      Position            : Rho.Matrices.Vector_3 := (0.0, 0.0, 1.0);
      Spot_Direction      : Rho.Matrices.Vector_3 := (0.0, 0.0, -1.0);
      Spot_Exponent       : Rho_Float := 0.0;
      Spot_Cutoff         : Rho_Float := 180.0;
      Attenuation         : Unit_Float := 0.0;
      Ambient_Coefficient : Unit_Float := 0.0)
   is
   begin
      Target.Current_Lights (Index) :=
        (True, Ambient_Intensity, Diffuse_Intensity, Specular_Intensity,
         Position, Spot_Direction, Spot_Exponent, Spot_Cutoff,
         Attenuation, Ambient_Coefficient);
   end Set_Light;

   ----------------------
   -- Set_Matrix_Saved --
   ----------------------

   procedure Set_Matrix_Saved
     (Target : in out Rho_Render_Target_Record'Class;
      Matrix : Rho.Matrices.Matrix_Mode_Type)
   is
   begin
      Target.Matrix_Saved (Matrix) := True;
   end Set_Matrix_Saved;

   ------------------
   -- Set_Viewport --
   ------------------

   procedure Set_Viewport
     (Target   : in out Rho_Render_Target_Record;
      Viewport : in Rho.Viewport.Rho_Viewport)
   is
   begin
      Target.Viewport := Viewport;
   end Set_Viewport;

   -------------------
   -- Set_Wireframe --
   -------------------

   procedure Set_Wireframe
     (Target  : in out Rho_Render_Target_Record;
      Enabled : Boolean)
   is
   begin
      Target.Wireframe := Enabled;
   end Set_Wireframe;

   ------------
   -- Unlock --
   ------------

   procedure Unlock
     (Target : in out Rho_Render_Target_Record'Class)
   is
      pragma Unreferenced (Target);
   begin
      Render_Lock.End_Operation;
   end Unlock;

   --------------
   -- Viewport --
   --------------

   function Viewport
     (Target : Rho_Render_Target_Record)
      return Rho.Viewport.Rho_Viewport
   is
      use type Rho.Viewport.Rho_Viewport;
   begin
      return (if Target.Viewport = null
              then Target.Full_Viewport
              else Target.Viewport);
   end Viewport;

   ---------------
   -- Wireframe --
   ---------------

   function Wireframe
     (Target  : Rho_Render_Target_Record)
      return Boolean
   is
   begin
      return Target.Wireframe;
   end Wireframe;

end Rho.Render_Target;
