with Rho.Color;
with Rho.Limits;
with Rho.Moveable;
with Rho.Node;
with Rho.Render_Target;

package Rho.Light is

   type Rho_Light_Type is (Ambient, Point, Directional, Spot);

   type Rho_Light_Record is
     new Rho.Node.Rho_Node_Record
     and Rho.Color.Rho_Has_Color
   with private;

   type Rho_Light is access all Rho_Light_Record'Class;

   procedure Rho_New (Light      : out Rho_Light;
                     Light_Type : Rho_Light_Type);

   procedure Initialize
     (Light      : in out Rho_Light_Record;
      Light_Type : Rho_Light_Type);

   overriding procedure Execute_Render
     (Light  : in out Rho_Light_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   overriding function Color
     (Light : Rho_Light_Record)
      return Rho.Color.Rho_Color;

   overriding procedure Set_Color
     (Light : in out Rho_Light_Record;
      Color : Rho.Color.Rho_Color);

   procedure Set_Index (Light : in out Rho_Light_Record;
                        Index : Rho.Limits.Light_Index);

   function Attenuation
     (Light : Rho_Light_Record'Class)
      return Unit_Float;

   procedure Set_Attenuation
     (Light       : in out Rho_Light_Record'Class;
      Attenuation : Unit_Float);

   function Ambient_Coefficient
     (Light : Rho_Light_Record'Class)
      return Unit_Float;

   procedure Set_Ambient_Coefficient
     (Light               : in out Rho_Light_Record'Class;
      Ambient_Coefficient : Unit_Float);

private

   type Rho_Light_Record is
     new Rho.Node.Rho_Node_Record
     and Rho.Color.Rho_Has_Color with
      record
         Index               : Rho.Limits.Light_Index :=
                                 Rho.Limits.Light_Index'First;
         Light_Type          : Rho_Light_Type;
         Color               : Rho.Color.Rho_Color;
         Attenuation         : Unit_Float;
         Ambient_Coefficient : Unit_Float;
      end record;

   overriding function Loaded
     (Light : Rho_Light_Record)
      return Boolean
   is (True);

end Rho.Light;
