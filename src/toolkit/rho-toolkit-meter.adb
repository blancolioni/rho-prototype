package body Rho.Toolkit.Meter is

   ------------
   -- Create --
   ------------

   procedure Create
     (Item  : in out Rho_Meter_Record;
      Low   : Rho_Float;
      High  : Rho_Float;
      Value : Rho_Float)
   is
   begin
      Rho.Toolkit.Widget.Create
        (Rho.Toolkit.Widget.Rho_Widget_Record (Item), "");
      Item.Low := Low;
      Item.High := High;
      Item.Value := Value;
   end Create;

   ----------
   -- High --
   ----------

   function High (Meter : Rho_Meter_Record) return Rho_Float is
   begin
      return Meter.High;
   end High;

   -----------
   -- Label --
   -----------

   function Label (Meter : Rho_Meter_Record) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Meter.Label);
   end Label;

   ---------
   -- Low --
   ---------

   function Low (Meter : Rho_Meter_Record) return Rho_Float is
   begin
      return Meter.Low;
   end Low;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label (Meter : in out Rho_Meter_Record;
                        Label : in     String)
   is
   begin
      Meter.Label := Ada.Strings.Unbounded.To_Unbounded_String (Label);
   end Set_Label;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Meter : in out Rho_Meter_Record;
                        Value : Rho_Float)
   is
   begin
      Meter.Value := Value;
   end Set_Value;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Meter    : not null access Rho_Meter_Record;
      Text     : String;
      Progress : Rho.Unit_Float)
   is
      use type Rho_Float;
   begin
      Meter.Set_Value (Meter.Low + (Meter.High - Meter.Low) * Progress);
      Meter.Set_Label (Text);
   end Update;

   -----------
   -- Value --
   -----------

   function Value (Meter : Rho_Meter_Record) return Rho_Float is
   begin
      return Meter.Value;
   end Value;

end Rho.Toolkit.Meter;
