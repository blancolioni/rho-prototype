private with Ada.Strings.Unbounded;

with Rho.Toolkit.Process;
with Rho.Toolkit.Widget;

package Rho.Toolkit.Meter is

   type Rho_Meter_Record is
     abstract new Rho.Toolkit.Widget.Rho_Widget_Record
     and Rho.Toolkit.Process.Rho_Process_Reporter_Interface
       with private;

   overriding function Default_Tag
     (Meter : Rho_Meter_Record)
      return String
   is ("Meter");

   overriding function Widget_Hierarchy_Tags
     (Meter : Rho_Meter_Record)
      return String
   is ("Meter "
       & Rho.Toolkit.Widget.Rho_Widget_Record (Meter).Widget_Hierarchy_Tags);

   overriding procedure Update
     (Meter    : not null access Rho_Meter_Record;
      Text     : String;
      Progress : Rho.Unit_Float);

   type Rho_Meter is access all Rho_Meter_Record'Class;

   procedure Create
     (Item  : in out Rho_Meter_Record;
      Low   : Rho_Float;
      High  : Rho_Float;
      Value : Rho_Float);

   function High (Meter : Rho_Meter_Record) return Rho_Float;
   function Low (Meter : Rho_Meter_Record) return Rho_Float;
   function Value (Meter : Rho_Meter_Record) return Rho_Float;

   procedure Set_Value (Meter : in out Rho_Meter_Record;
                        Value : Rho_Float);

   function Label (Meter : Rho_Meter_Record) return String;
   procedure Set_Label (Meter : in out Rho_Meter_Record;
                        Label : in     String);

private

   type Rho_Meter_Record is
     abstract new Rho.Toolkit.Widget.Rho_Widget_Record
     and Rho.Toolkit.Process.Rho_Process_Reporter_Interface with
      record
         Label : Ada.Strings.Unbounded.Unbounded_String;
         Low   : Rho_Float := 0.0;
         High  : Rho_Float := 1.0;
         Value : Rho_Float := 0.0;
      end record;

end Rho.Toolkit.Meter;
