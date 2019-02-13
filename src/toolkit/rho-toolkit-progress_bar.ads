with Cairo;

with Rho.Rectangle;
with Rho.Toolkit.Meter;

package Rho.Toolkit.Progress_Bar is

   type Rho_Progress_Bar_Record is
     new Rho.Toolkit.Meter.Rho_Meter_Record
       with private;

   overriding function Default_Tag
     (Progress : Rho_Progress_Bar_Record)
      return String
   is ("Progress_Bar");

   overriding function Widget_Hierarchy_Tags
     (Progress : Rho_Progress_Bar_Record)
      return String
   is ("Progress_Bar "
       & Rho.Toolkit.Meter.Rho_Meter_Record (Progress).Widget_Hierarchy_Tags);

   overriding procedure Draw
     (Progress : in out Rho_Progress_Bar_Record;
      Context  : in     Cairo.Cairo_Context;
      X, Y     : in     Rho_Float;
      Region   : in     Rho.Rectangle.Rho_Rectangle);

   overriding procedure Set_Value
     (Progress : in out Rho_Progress_Bar_Record;
      Value : Rho_Float);

   type Rho_Progress_Bar is access all Rho_Progress_Bar_Record'Class;

   overriding procedure Create
     (Item : in out Rho_Progress_Bar_Record;
      Low  : Rho_Float;
      High : Rho_Float;
      Value : Rho_Float);

   procedure Rho_New
     (Item  : out Rho_Progress_Bar;
      Low   : Rho_Float;
      High  : Rho_Float;
      Value : Rho_Float);

private

   type Rho_Progress_Bar_Record is
     new Rho.Toolkit.Meter.Rho_Meter_Record with
      record
         Previous_Progress : Rho_Float := 0.0;
         MaMaasmum_Change    : Rho_Float := 0.01;
      end record;

   function Bar_Rectangle
     (Item : Rho_Progress_Bar_Record'Class)
      return Rho.Rectangle.Rho_Rectangle;

end Rho.Toolkit.Progress_Bar;
