with Rho.Matrices;

package Rho.Transition.Translation is

   type Rho_Translation_Transition_Record is
     new Rho_Transition_Record with private;

   type Rho_Translation_Transition is
     access all Rho_Translation_Transition_Record'Class;

   procedure Initialize
     (Transition         : in out Rho_Translation_Transition_Record'Class;
      Node               : not null access Rho.Node.Rho_Node_Record'Class;
      Transition_Time    : Duration;
      Target_Position    : Rho.Matrices.Vector_3;
      Cyclic             : Boolean := False);

   procedure Initialize
     (Transition         : in out Rho_Translation_Transition_Record'Class;
      Node               : not null access Rho.Node.Rho_Node_Record'Class;
      Transition_Time    : Duration;
      Target_X           : Rho_Float;
      Target_Y           : Rho_Float;
      Target_Z           : Rho_Float;
      Cyclic             : Boolean := False);

   function Translate
     (Node               : not null access Rho.Node.Rho_Node_Record'Class;
      Transition_Time    : Duration;
      Target_Position    : Rho.Matrices.Vector;
      Cyclic             : Boolean := False)
      return Rho_Transition;

   function Translate
     (Node               : not null access Rho.Node.Rho_Node_Record'Class;
      Transition_Time    : Duration;
      Target_X           : Rho_Float;
      Target_Y           : Rho_Float;
      Target_Z           : Rho_Float;
      Cyclic             : Boolean := False)
      return Rho_Transition;

   function Translate
     (Node               : not null access Rho.Node.Rho_Node_Record'Class;
      Transition_Time    : Duration;
      Target_Node        : Rho.Node.Rho_Node;
      Offset_X           : Rho_Float;
      Offset_Y           : Rho_Float;
      Offset_Z           : Rho_Float;
      Cyclic             : Boolean := False)
      return Rho_Transition;

   function Translate
     (Node               : not null access Rho.Node.Rho_Node_Record'Class;
      Transition_Time    : Duration;
      Target_Node        : Rho.Node.Rho_Node;
      Offset             : Rho.Matrices.Vector_3;
      Cyclic             : Boolean := False)
      return Rho_Transition;

private

   type Rho_Translation_Transition_Record is
     new Rho_Transition_Record with
      record
         Target_Node    : Rho.Node.Rho_Node;
         Node_Offset    : Rho.Matrices.Vector_3;
         Start_Position : Rho.Matrices.Vector_3;
         End_Position   : Rho.Matrices.Vector_3 := (0.0, 0.0, 0.0);
         Position_Delta : Rho.Matrices.Vector_3;
      end record;

   overriding procedure Transition_Started
     (Transition : in out Rho_Translation_Transition_Record);

   overriding procedure Transition_Complete
     (Transition : in out Rho_Translation_Transition_Record);

   overriding procedure Update_Progress
     (Transition : in out Rho_Translation_Transition_Record;
      Progress   : Unit_Float);

end Rho.Transition.Translation;
