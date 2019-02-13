with Ada.Unchecked_Deallocation;

package body Rho.Object is

   procedure Free is
     new Ada.Unchecked_Deallocation (Rho_Object_Record'Class, Rho_Object);

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Object : access Rho_Object_Record'Class) is
      X : Rho_Object := Rho_Object (Object);
   begin
      Free (X);
   end Destroy;

   ----------
   -- Name --
   ----------

   function Name (Item : Rho_Object_Record'Class) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Name);
   end Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Item : in out Rho_Object_Record'Class;
      Name : in     String)
   is
   begin
      Item.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

end Rho.Object;
