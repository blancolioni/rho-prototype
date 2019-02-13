with Ada.Containers.Indefinite_Vectors;

package Rho.Toolkit.Values.Vectors is
  new Ada.Containers.Indefinite_Vectors (Positive, Rho_Value_Interface'Class);
