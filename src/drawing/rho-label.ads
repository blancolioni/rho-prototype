private with Ada.Strings.Unbounded;

with Rho.Entity;
with Rho.Font;

package Rho.Label is

   type Rho_Label_Record is
     new Rho.Entity.Rho_Entity_Record with private;

   type Rho_Label is access all Rho_Label_Record'Class;

   function Create_Label
     (Name      : String;
      Text      : String;
      Font      : Rho.Font.Rho_Font)
      return Rho_Label;

private

   type Rho_Label_Record is
     new Rho.Entity.Rho_Entity_Record with
      record
         Text      : Ada.Strings.Unbounded.Unbounded_String;
         Font      : Rho.Font.Rho_Font;
      end record;

end Rho.Label;
