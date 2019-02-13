package Rho.Toolkit.Process is

   type Rho_Process_Interface is interface;

   procedure Start (Process : in out Rho_Process_Interface)
   is abstract;

   procedure Next_Step (Process : in out Rho_Process_Interface)
   is abstract;

   procedure Current_Progress
     (Process  : Rho_Process_Interface;
      Progress : out Natural;
      Finish   : out Positive)
   is abstract;

   function Current_Task
     (Process : Rho_Process_Interface)
      return String
      is abstract;

   type Rho_Process is access all Rho_Process_Interface'Class;

   type Rho_Process_Reporter_Interface is interface;

   procedure Update
     (Reporter : not null access Rho_Process_Reporter_Interface;
      Text     : String;
      Progress : Rho.Unit_Float)
   is abstract;

   type Rho_Process_Reporter is
     access all Rho_Process_Reporter_Interface'Class;

   procedure Start_Process
     (Process  : not null access Rho_Process_Interface'Class;
      Reporter : not null access Rho_Process_Reporter_Interface'Class);

end Rho.Toolkit.Process;
