package Rho.Octets is

   type Rho_Octet is mod 256;

   type Rho_Octet_Array is array (Positive range <>) of aliased Rho_Octet;

   type Rho_Octet_Array_Access is access all Rho_Octet_Array;

end Rho.Octets;
