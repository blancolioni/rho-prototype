package body Rho.Transition.Container is

   ------------
   -- Append --
   ------------

   procedure Append
     (Container : in out Rho_Container_Transition_Record;
      Child     : not null access Rho_Transition_Record'Class)
   is
   begin
      Container.Children.Append (Child);
   end Append;

   -------------
   -- Restart --
   -------------

   overriding procedure Restart
     (Container : in out Rho_Container_Transition_Record)
   is
   begin
      Rho_Transition_Record (Container).Restart;
      for Child of Container.Children loop
         Child.Restart;
      end loop;
   end Restart;

end Rho.Transition.Container;
