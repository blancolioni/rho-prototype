package body Rho.Toolkit.Bin is

   -----------
   -- Child --
   -----------

   function Child
     (Bin : Rho_Bin_Record'Class)
      return Rho.Toolkit.Widget.Rho_Widget
   is
   begin
      if Bin.Has_Children then
         return Bin.First_Child;
      else
         return null;
      end if;
   end Child;

end Rho.Toolkit.Bin;
