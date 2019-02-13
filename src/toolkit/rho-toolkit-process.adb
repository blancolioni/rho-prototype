package body Rho.Toolkit.Process is

   task Process_Task is
      entry Start
        (Process  : Rho_Process;
         Reporter : Rho_Process_Reporter);
   end Process_Task;

   ------------------
   -- Process_Task --
   ------------------

   task body Process_Task is
      Current_Process  : Rho_Process;
      Current_Reporter : Rho_Process_Reporter;
   begin
      loop
         select
            accept Start (Process : in Rho_Process;
                          Reporter : in Rho_Process_Reporter)
            do
               Current_Process := Process;
               Current_Reporter := Reporter;
            end Start;
            Current_Process.Start;
            loop
               Current_Process.Next_Step;
               declare
                  use Maas;
                  Progress : Natural;
                  Finish   : Positive;
               begin
                  Current_Process.Current_Progress (Progress, Finish);
                  Current_Reporter.Update
                    (Current_Process.Current_Task,
                     Rho_Float (Progress) / Rho_Float (Finish));
                  exit when Progress >= Finish;
               end;
            end loop;
         or
            terminate;
         end select;
      end loop;
   end Process_Task;

   -------------------
   -- Start_Process --
   -------------------

   procedure Start_Process
     (Process  : not null access Rho_Process_Interface'Class;
      Reporter : not null access Rho_Process_Reporter_Interface'Class)
   is
   begin
      Process_Task.Start (Process, Reporter);
   end Start_Process;

end Rho.Toolkit.Process;
