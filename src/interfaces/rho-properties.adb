package body Rho.Properties is

   ------------------
   -- Get_Property --
   ------------------

   overriding function Get_Property
     (Container : Rho_Property_Container_Record;
      Name      : String)
      return Rho.Value.Rho_Value
   is
   begin
      return Container.Map.Element (Name);
   end Get_Property;

   ------------------
   -- Has_Property --
   ------------------

   overriding function Has_Property
     (Container : Rho_Property_Container_Record;
      Name      : String)
      return Boolean
   is
   begin
      return Container.Map.Contains (Name);
   end Has_Property;

   ------------------
   -- Set_Property --
   ------------------

   overriding procedure Set_Property
     (Container : in out Rho_Property_Container_Record;
      Name      : String;
      Value     : Rho.Value.Rho_Value)
   is
   begin
      if Container.Map.Contains (Name) then
         Container.Map.Replace (Name, Value);
      else
         Container.Map.Insert (Name, Value);
      end if;
   end Set_Property;

end Rho.Properties;
