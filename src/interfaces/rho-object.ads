with Ada.Finalization;
private with Ada.Strings.Unbounded;

package Rho.Object is

   type Rho_Object_Record is
     abstract new Ada.Finalization.Controlled with private;

   type Rho_Object is access all Rho_Object_Record'Class;

   function Name (Item : Rho_Object_Record'Class) return String;
   procedure Set_Name (Item : in out Rho_Object_Record'Class;
                       Name : in     String);

   procedure Destroy (Object : access Rho_Object_Record'Class);

   type Rho_Resource_Record is abstract new Rho_Object_Record with private;

   type Rho_Resource is access all Rho_Resource_Record'Class;

   function Class_Name
     (Item : Rho_Resource_Record) return String
      is abstract;

private

   type Rho_Object_Record is
     abstract new Ada.Finalization.Controlled with
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Rho_Resource_Record is
     abstract new Rho_Object_Record
   with null record;
end Rho.Object;
