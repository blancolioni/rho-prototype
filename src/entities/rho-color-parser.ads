package Rho.Color.Parser is

   function Parse_Html_Color
     (Text : String)
      return Rho.Color.Rho_Color;
   --  parses a Color string of the form #RRGGBB

end Rho.Color.Parser;
