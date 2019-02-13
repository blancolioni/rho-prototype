with Rho.Float_Arrays;

package body Rho.Transition.Translation is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Transition         : in out Rho_Translation_Transition_Record'Class;
      Node               : not null access Rho.Node.Rho_Node_Record'Class;
      Transition_Time    : Duration;
      Target_Position    : Rho.Matrices.Vector_3;
      Cyclic             : Boolean := False)
   is
   begin
      Rho_Transition_Record (Transition).Initialize
        (Node            => Node,
         Transition_Time => Transition_Time,
         Cyclic          => Cyclic);

      Transition.End_Position := Target_Position;

   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Transition         : in out Rho_Translation_Transition_Record'Class;
      Node               : not null access Rho.Node.Rho_Node_Record'Class;
      Transition_Time    : Duration;
      Target_X           : Rho_Float;
      Target_Y           : Rho_Float;
      Target_Z           : Rho_Float;
      Cyclic             : Boolean := False)
   is
   begin
      Transition.Initialize
        (Node, Transition_Time, (Target_X, Target_Y, Target_Z), Cyclic);
   end Initialize;

   -------------------------
   -- Transition_Complete --
   -------------------------

   overriding procedure Transition_Complete
     (Transition : in out Rho_Translation_Transition_Record)
   is
   begin
      Transition.Target.Set_Position (Transition.End_Position);
   end Transition_Complete;

   ------------------------
   -- Transition_Started --
   ------------------------

   overriding procedure Transition_Started
     (Transition : in out Rho_Translation_Transition_Record)
   is
      use Rho.Float_Arrays;
   begin
      Transition.Start_Position := Transition.Target.Position_3;
      Transition.Position_Delta :=
        Transition.End_Position - Transition.Start_Position;
   end Transition_Started;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Node               : not null access Rho.Node.Rho_Node_Record'Class;
      Transition_Time    : Duration;
      Target_Position    : Rho.Matrices.Vector;
      Cyclic             : Boolean := False)
      return Rho_Transition
   is
      Result : constant Rho_Translation_Transition :=
                 new Rho_Translation_Transition_Record;
   begin
      Result.Initialize (Node, Transition_Time, Target_Position, Cyclic);
      return Rho_Transition (Result);
   end Translate;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Node               : not null access Rho.Node.Rho_Node_Record'Class;
      Transition_Time    : Duration;
      Target_X           : Rho_Float;
      Target_Y           : Rho_Float;
      Target_Z           : Rho_Float;
      Cyclic             : Boolean := False)
      return Rho_Transition
   is
      Result : constant Rho_Translation_Transition :=
                 new Rho_Translation_Transition_Record;
   begin
      Result.Initialize (Node, Transition_Time,
                         Target_X, Target_Y, Target_Z,
                         Cyclic);
      return Rho_Transition (Result);
   end Translate;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Node               : not null access Rho.Node.Rho_Node_Record'Class;
      Transition_Time    : Duration;
      Target_Node        : Rho.Node.Rho_Node;
      Offset             : Rho.Matrices.Vector_3;
      Cyclic             : Boolean := False)
      return Rho_Transition
   is
      Result : constant Rho_Translation_Transition :=
                 new Rho_Translation_Transition_Record;
   begin
      Result.Initialize (Node, Transition_Time,
                         Target_Node.Position_3, Cyclic);
      Result.Target_Node := Target_Node;
      Result.Node_Offset := Offset;
      return Rho_Transition (Result);
   end Translate;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Node               : not null access Rho.Node.Rho_Node_Record'Class;
      Transition_Time    : Duration;
      Target_Node        : Rho.Node.Rho_Node;
      Offset_X           : Rho_Float;
      Offset_Y           : Rho_Float;
      Offset_Z           : Rho_Float;
      Cyclic             : Boolean := False)
      return Rho_Transition
   is
   begin
      return Translate (Node, Transition_Time, Target_Node,
                        (Offset_X, Offset_Y, Offset_Z),
                        Cyclic);
   end Translate;

   ---------------------
   -- Update_Progress --
   ---------------------

   overriding procedure Update_Progress
     (Transition : in out Rho_Translation_Transition_Record;
      Progress   : Unit_Float)
   is
      use type Rho.Node.Rho_Node;
      use Rho.Float_Arrays;
   begin
      if Transition.Target_Node /= null then
         Transition.End_Position :=
           Transition.Target_Node.Position_3 + Transition.Node_Offset;
         Transition.Position_Delta :=
           Transition.End_Position - Transition.Start_Position;
      end if;
      Transition.Target.Set_Position
        (Transition.Start_Position + Transition.Position_Delta * Progress);
   end Update_Progress;

end Rho.Transition.Translation;
