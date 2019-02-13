with Rho.Float_Arrays;

package body Rho.Transition.Orientation is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Transition         : in out Rho_Orientation_Transition_Record'Class;
      Node               : not null access Rho.Node.Rho_Node_Record'Class;
      Transition_Time    : Duration;
      Target_Angle       : Rho_Float;
      Angle_X            : Rho_Float;
      Angle_Y            : Rho_Float;
      Angle_Z            : Rho_Float;
      Cyclic             : Boolean := False)
   is
   begin
      Transition.Initialize (Node, Transition_Time, Cyclic);
      Transition.End_Angle := Target_Angle;
      Transition.End_Vector := (Angle_X, Angle_Y, Angle_Z);
   end Initialize;

   --------------------------------
   -- New_Orientation_Transition --
   --------------------------------

   function New_Orientation_Transition
     (Node               : not null access Rho.Node.Rho_Node_Record'Class;
      Transition_Time    : Duration;
      Target_Angle       : Rho_Float;
      Angle_X            : Rho_Float;
      Angle_Y            : Rho_Float;
      Angle_Z            : Rho_Float;
      Cyclic             : Boolean := False)
      return Rho_Orientation_Transition
   is
   begin
      return Transition : constant Rho_Orientation_Transition :=
        new Rho_Orientation_Transition_Record
      do
         Transition.Initialize
           (Node,
            Transition_Time, Target_Angle,
            Angle_X, Angle_Y, Angle_Z,
            Cyclic);
      end return;
   end New_Orientation_Transition;

   -------------------------
   -- Transition_Complete --
   -------------------------

   overriding procedure Transition_Complete
     (Transition : in out Rho_Orientation_Transition_Record)
   is
   begin
      Transition.Target.Set_Orientation
        (Transition.End_Angle,
         Transition.End_Vector);
   end Transition_Complete;

   ------------------------
   -- Transition_Started --
   ------------------------

   overriding procedure Transition_Started
     (Transition : in out Rho_Orientation_Transition_Record)
   is
      use Rho.Float_Arrays;
   begin
      Transition.Target.Get_Orientation
        (Transition.Start_Angle,
         Transition.Start_Vector (1),
         Transition.Start_Vector (2),
         Transition.Start_Vector (3));
      Transition.Angle_Delta :=
        Transition.End_Angle - Transition.Start_Angle;
      Transition.Vector_Delta :=
        Transition.End_Vector - Transition.Start_Vector;
   end Transition_Started;

   ------------
   -- Update --
   ------------

   overriding procedure Update_Progress
     (Transition : in out Rho_Orientation_Transition_Record;
      Progress   : Unit_Float)
   is
   begin
      Transition.Target.Set_Orientation
        (Transition.Start_Angle,
         Transition.Start_Vector);
      Transition.Target.Rotate
        (Transition.Angle_Delta * Progress,
         Transition.End_Vector (1),
         Transition.End_Vector (2),
         Transition.End_Vector (3));
   end Update_Progress;

end Rho.Transition.Orientation;
