package body Rho.Toolkit.Properties is

   ---------
   -- Add --
   ---------

   procedure Add
     (To_List  : in out Rho_Property_List;
      Property : Root_Rho_Property'Class)
   is
   begin
      To_List.Map.Insert (Name (Property), Property);
   end Add;

   ------------
   -- Create --
   ------------

   procedure Create
     (Property : in out Root_Rho_Property'Class;
      Name     : in String)
   is
   begin
      Property.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Create;

   ------------------
   -- Has_Property --
   ------------------

   overriding function Has_Property
     (List : Rho_Property_List;
      Name : String)
      return Boolean
   is
   begin
      return List.Map.Contains (Name);
   end Has_Property;

   ----------
   -- Name --
   ----------

   function Name (Property : Root_Rho_Property'Class) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Property.Name);
   end Name;

   --------------
   -- Property --
   --------------

   overriding function Property
     (List : Rho_Property_List;
      Name : String)
      return Root_Rho_Property'Class
   is
   begin
      return List.Map.Element (Name);
   end Property;

   ------------
   -- Update --
   ------------

   procedure Update
     (To_List  : in out Rho_Property_List;
      Property : Root_Rho_Property'Class)
   is
   begin
      To_List.Map.Replace_Element
        (To_List.Map.Find (Name (Property)),
         Property);
   end Update;

end Rho.Toolkit.Properties;
