package Rho.Materials is

   type Material_Operator is
     (Always, Never,
      Equal, Not_Equal,
      Less_Than, Not_Less_Than,
      Greater_Than, Not_Greater_Than);

   type Material_Polygon_Mode is
     (Solid, Wireframe, Points);

   function Operator_Symbol
     (Operator : Material_Operator)
      return String;

end Rho.Materials;
