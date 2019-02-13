with Rho.Toolkit.Properties.Enumerated_Property;

package Rho.Toolkit.Alignment is

   type Rho.Toolkit_Alignment is (Fill, Start, Finish, Center, Baseline);

   package Alignment_Properties is
     new Rho.Toolkit.Properties.Enumerated_Property (Rho.Toolkit_Alignment);

end Rho.Toolkit.Alignment;
