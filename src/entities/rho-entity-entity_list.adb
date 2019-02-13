package body Rho.Entity.Entity_List is

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (List : in out Rho_Entity_List;
      Item : in     Rho_Entity)
   is
   begin
      List.Entity_Vector.Append (Item);
   end Append;

   ------------
   -- Entity --
   ------------

   overriding function Entity
     (List : Rho_Entity_List;
      Name : String)
      return Rho_Entity
   is
   begin
      for E of List.Entity_Vector loop
         if E.Name = Name then
            return E;
         end if;
      end loop;
      return null;
   end Entity;

end Rho.Entity.Entity_List;
