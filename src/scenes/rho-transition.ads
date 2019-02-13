with Ada.Calendar;

with Rho.Node;

package Rho.Transition is

   type Rho_Transition_Record is abstract tagged private;

   type Rho_Transition is access all Rho_Transition_Record'Class;

   procedure Initialize
     (Transition      : in out Rho_Transition_Record'Class;
      Node            : access Rho.Node.Rho_Node_Record'Class;
      Transition_Time : Duration;
      Cyclic          : Boolean := False);

   procedure Transition_Started
     (Transition : in out Rho_Transition_Record)
   is abstract;

   procedure Transition_Complete
     (Transition : in out Rho_Transition_Record)
   is abstract;

   procedure Update_Progress
     (Transition : in out Rho_Transition_Record;
      Progress   : Unit_Float)
   is abstract;

   procedure Restart
     (Transition : in out Rho_Transition_Record);

   function Start_Time
     (Transition : Rho_Transition_Record'Class)
      return Ada.Calendar.Time;

   procedure Update
     (Transition : in out Rho_Transition_Record'Class);

   function Complete
     (Transition : Rho_Transition_Record)
      return Boolean;

   function Target
     (Transition : Rho_Transition_Record'Class)
      return Rho.Node.Rho_Node;

   type Transition_Callback_Interface is interface;

   procedure Execute (Callback : in out Transition_Callback_Interface)
   is abstract;

   type Transition_Callback is access all Transition_Callback_Interface'Class;

   procedure On_Complete
     (Transition : in out Rho_Transition_Record'Class;
      Callback   : not null access Transition_Callback_Interface'Class);

private

   type Rho_Transition_Record is abstract tagged
      record
         Started         : Boolean := False;
         Complete        : Boolean := False;
         Cyclic          : Boolean := False;
         Target          : Rho.Node.Rho_Node;
         Start_Time      : Ada.Calendar.Time;
         Transition_Time : Duration;
         Elapsed_Time    : Duration;
         On_Complete     : Transition_Callback;
      end record;

end Rho.Transition;
