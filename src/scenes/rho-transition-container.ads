private with Ada.Containers.Vectors;

package Rho.Transition.Container is

   type Rho_Container_Transition_Record is
     abstract new Rho_Transition_Record with private;

   type Rho_Container_Transition is
     access all Rho_Container_Transition_Record'Class;

   overriding procedure Restart
     (Container : in out Rho_Container_Transition_Record);

   procedure Append
     (Container : in out Rho_Container_Transition_Record;
      Child     : not null access Rho_Transition_Record'Class);

private

   package Transition_Vectors is
     new Ada.Containers.Vectors (Positive, Rho_Transition);

   type Rho_Container_Transition_Record is
     abstract new Rho_Transition_Record with
      record
         Children : Transition_Vectors.Vector;
      end record;

end Rho.Transition.Container;
