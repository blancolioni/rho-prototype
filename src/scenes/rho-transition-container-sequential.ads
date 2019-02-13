package Rho.Transition.Container.Sequential is

   type Rho_Sequential_Transition_Record is
     new Rho_Container_Transition_Record with private;

   type Rho_Sequential_Transition is
     access all Rho_Sequential_Transition_Record'Class;

   procedure Initialize
     (Transition : in out Rho_Sequential_Transition_Record;
      Cyclic     : Boolean := False);

   overriding procedure Append
     (Container : in out Rho_Sequential_Transition_Record;
      Child     : not null access Rho_Transition_Record'Class);

   overriding procedure Transition_Started
     (Transition : in out Rho_Sequential_Transition_Record);

   overriding procedure Transition_Complete
     (Transition : in out Rho_Sequential_Transition_Record);

   overriding procedure Update_Progress
     (Transition : in out Rho_Sequential_Transition_Record;
      Progress   : Unit_Float);

   function New_Sequential_Transition
     (Cyclic : Boolean := False)
     return Rho_Sequential_Transition;

private

   package Progress_Waypoints is
     new Ada.Containers.Vectors (Positive, Unit_Float);

   type Rho_Sequential_Transition_Record is
     new Rho_Container_Transition_Record with
      record
         Waypoints     : Progress_Waypoints.Vector;
         Current_Index : Natural := 0;
      end record;

end Rho.Transition.Container.Sequential;
