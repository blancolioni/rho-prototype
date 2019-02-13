package body Rho.Transition.Container.Sequential is

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Container : in out Rho_Sequential_Transition_Record;
      Child     : not null access Rho_Transition_Record'Class)
   is
   begin
      Rho_Container_Transition_Record (Container).Append (Child);
      Container.Transition_Time :=
        Container.Transition_Time + Child.Transition_Time;
   end Append;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Transition : in out Rho_Sequential_Transition_Record;
      Cyclic     : Boolean := False)
   is
   begin
      Rho_Container_Transition_Record (Transition).Initialize
        (Node            => null,
         Transition_Time => 0.0,
         Cyclic          => Cyclic);
   end Initialize;

   -------------------------------
   -- New_Sequential_Transition --
   -------------------------------

   function New_Sequential_Transition
     (Cyclic : Boolean := False)
      return Rho_Sequential_Transition
   is
   begin
      return Result : constant Rho_Sequential_Transition :=
        new Rho_Sequential_Transition_Record
      do
         Result.Initialize (Cyclic);
      end return;
   end New_Sequential_Transition;

   -------------------------
   -- Transition_Complete --
   -------------------------

   overriding procedure Transition_Complete
     (Transition : in out Rho_Sequential_Transition_Record)
   is
   begin
      Transition.Update_Progress (1.0);
      Transition.Children.Last_Element.Transition_Complete;
   end Transition_Complete;

   ------------------------
   -- Transition_Started --
   ------------------------

   overriding procedure Transition_Started
     (Transition : in out Rho_Sequential_Transition_Record)
   is
      Children        : Transition_Vectors.Vector renames Transition.Children;
      Index           : Natural renames Transition.Current_Index;
      Cumulative_Time : Duration := 0.0;
   begin
      Transition.Waypoints.Clear;
      for Child of Children loop
         Transition.Waypoints.Append
           (Rho_Float (Cumulative_Time)
            / Rho_Float (Transition.Transition_Time));
         Cumulative_Time := Cumulative_Time + Child.Transition_Time;
      end loop;

      Index := 1;
      if Children.Last_Index >= Index then
         Children.Element (Index).Transition_Started;
      end if;
   end Transition_Started;

   ---------------------
   -- Update_Progress --
   ---------------------

   overriding procedure Update_Progress
     (Transition : in out Rho_Sequential_Transition_Record;
      Progress   : Unit_Float)
   is
      Waypoints : Progress_Waypoints.Vector renames Transition.Waypoints;
      Children  : Transition_Vectors.Vector renames Transition.Children;
      Index     : Natural renames Transition.Current_Index;
   begin
      while Index < Children.Last_Index
        and then Waypoints (Index + 1) <= Progress
      loop
         Children.Element (Index).Transition_Complete;
         Index := Index + 1;
         Children.Element (Index).Transition_Started;
      end loop;

      Children.Element (Index).Update;

   end Update_Progress;

end Rho.Transition.Container.Sequential;
