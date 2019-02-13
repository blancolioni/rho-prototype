package body Rho.Transition is

   --------------
   -- Complete --
   --------------

   function Complete
     (Transition : Rho_Transition_Record)
      return Boolean
   is
   begin
      return Transition.Complete;
   end Complete;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Transition      : in out Rho_Transition_Record'Class;
      Node            : access Rho.Node.Rho_Node_Record'Class;
      Transition_Time : Duration;
      Cyclic          : Boolean := False)
   is
   begin
      Transition.Target := Rho.Node.Rho_Node (Node);
      Transition.Transition_Time := Transition_Time;
      Transition.Cyclic := Cyclic;
   end Initialize;

   -----------------
   -- On_Complete --
   -----------------

   procedure On_Complete
     (Transition : in out Rho_Transition_Record'Class;
      Callback   : not null access Transition_Callback_Interface'Class)
   is
   begin
      Transition.On_Complete := Callback;
   end On_Complete;

   -------------
   -- Restart --
   -------------

   procedure Restart
     (Transition : in out Rho_Transition_Record)
   is
   begin
      Transition.Started := False;
   end Restart;

   ----------------
   -- Start_Time --
   ----------------

   function Start_Time
     (Transition : Rho_Transition_Record'Class)
      return Ada.Calendar.Time
   is
   begin
      return Transition.Start_Time;
   end Start_Time;

   ------------
   -- Target --
   ------------

   function Target
     (Transition : Rho_Transition_Record'Class)
      return Rho.Node.Rho_Node
   is
   begin
      return Transition.Target;
   end Target;

   ------------
   -- Update --
   ------------

   procedure Update
     (Transition   : in out Rho_Transition_Record'Class)
   is
      use Ada.Calendar;
      Now : constant Time := Clock;
   begin
      if not Transition.Started then
         Transition.Started := True;
         Transition.Start_Time := Now;
         Transition.Transition_Started;
      end if;

      Transition.Elapsed_Time := Now - Transition.Start_Time;

      declare
         Progress : constant Non_Negative_Float :=
                      Rho_Float (Transition.Elapsed_Time)
                      / Rho_Float (Transition.Transition_Time);
      begin
         if Progress >= 1.0 then
            Transition.Transition_Complete;
            if Transition.On_Complete /= null then
               Transition.On_Complete.Execute;
            end if;
            if Transition.Cyclic then
               Transition.Restart;
            else
               Transition.Complete := True;
            end if;
         else
            Transition.Update_Progress (Progress);
         end if;
      end;

   end Update;

end Rho.Transition;
