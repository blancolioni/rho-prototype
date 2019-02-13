package body Rho.Materials is

   ---------------------
   -- Operator_Symbol --
   ---------------------

   function Operator_Symbol
     (Operator : Material_Operator)
      return String
   is
   begin
      case Operator is
         when Always =>
            return "";
         when Never =>
            return "";
         when Equal =>
            return "==";
         when Not_Equal =>
            return "!=";
         when Less_Than =>
            return "<";
         when Not_Less_Than =>
            return ">=";
         when Greater_Than =>
            return ">";
         when Not_Greater_Than =>
            return "<=";
      end case;

   end Operator_Symbol;

end Rho.Materials;
