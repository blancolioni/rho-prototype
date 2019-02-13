with Rho.Matrices;

package Rho.Transition.Orientation is

   type Rho_Orientation_Transition_Record is
     new Rho_Transition_Record with private;

   type Rho_Orientation_Transition is
     access all Rho_Orientation_Transition_Record'Class;

   procedure Initialize
     (Transition         : in out Rho_Orientation_Transition_Record'Class;
      Node               : not null access Rho.Node.Rho_Node_Record'Class;
      Transition_Time    : Duration;
      Target_Angle       : Rho_Float;
      Angle_X            : Rho_Float;
      Angle_Y            : Rho_Float;
      Angle_Z            : Rho_Float;
      Cyclic             : Boolean := False);

   function New_Orientation_Transition
     (Node               : not null access Rho.Node.Rho_Node_Record'Class;
      Transition_Time    : Duration;
      Target_Angle       : Rho_Float;
      Angle_X            : Rho_Float;
      Angle_Y            : Rho_Float;
      Angle_Z            : Rho_Float;
      Cyclic             : Boolean := False)
      return Rho_Orientation_Transition;

   overriding procedure Transition_Started
     (Transition : in out Rho_Orientation_Transition_Record);

   overriding procedure Transition_Complete
     (Transition : in out Rho_Orientation_Transition_Record);

   overriding procedure Update_Progress
     (Transition : in out Rho_Orientation_Transition_Record;
      Progress   : Unit_Float);

private

   type Rho_Orientation_Transition_Record is
     new Rho_Transition_Record with
      record
         Start_Angle  : Rho_Float;
         End_Angle    : Rho_Float;
         Angle_Delta  : Rho_Float;
         Start_Vector : Rho.Matrices.Vector_3;
         End_Vector   : Rho.Matrices.Vector_3;
         Vector_Delta : Rho.Matrices.Vector_3;
      end record;

end Rho.Transition.Orientation;
