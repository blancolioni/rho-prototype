with Rho.Float_Arrays;
with Rho.Matrices;

package body Rho.Light is

   -------------------------
   -- Ambient_Coefficient --
   -------------------------

   function Ambient_Coefficient
     (Light : Rho_Light_Record'Class)
      return Unit_Float
   is
   begin
      return Light.Ambient_Coefficient;
   end Ambient_Coefficient;

   -----------------
   -- Attenuation --
   -----------------

   function Attenuation
     (Light : Rho_Light_Record'Class)
      return Unit_Float
   is
   begin
      return Light.Attenuation;
   end Attenuation;

   -----------
   -- Color --
   -----------

   overriding function Color
     (Light : Rho_Light_Record)
      return Rho.Color.Rho_Color
   is
   begin
      return Light.Color;
   end Color;

   --------------------
   -- Execute_Render --
   --------------------

   overriding procedure Execute_Render
     (Light  : in out Rho_Light_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
      use Rho.Float_Arrays;
   begin
      case Light.Light_Type is
         when Ambient =>
            Target.Set_Light
              (Index              => Light.Index,
               Ambient_Intensity  => Light.Color);
         when Point =>
            declare
               T : constant Rho.Matrices.Matrix_4 :=
                     Light.Final_Transformation_Matrix;
               Position : constant Rho.Matrices.Vector_4 :=
                            T * (0.0, 0.0, 0.1, 1.0);
            begin
               Target.Set_Light
                 (Index              => Light.Index,
                  Diffuse_Intensity  => Light.Color,
                  Specular_Intensity => Light.Color,
                  Position           => Position (1 .. 3));
            end;
         when Directional =>
            Target.Set_Light
              (Index              => Light.Index,
               Diffuse_Intensity  => Light.Color,
               Specular_Intensity => Light.Color);
         when Spot =>
            null;
      end case;
   end Execute_Render;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Light      : in out Rho_Light_Record;
      Light_Type : Rho_Light_Type)
   is
   begin
      Light.Light_Type := Light_Type;
   end Initialize;

   --------------
   -- Rho_New --
   --------------

   procedure Rho_New
     (Light      : out Rho_Light;
      Light_Type : Rho_Light_Type)
   is
   begin
      Light := new Rho_Light_Record;
      Light.Initialize (Light_Type);
   end Rho_New;

   -----------------------------
   -- Set_Ambient_Coefficient --
   -----------------------------

   procedure Set_Ambient_Coefficient
     (Light               : in out Rho_Light_Record'Class;
      Ambient_Coefficient : Unit_Float)
   is
   begin
      Light.Ambient_Coefficient := Ambient_Coefficient;
   end Set_Ambient_Coefficient;

   ---------------------
   -- Set_Attenuation --
   ---------------------

   procedure Set_Attenuation
     (Light       : in out Rho_Light_Record'Class;
      Attenuation : Unit_Float)
   is
   begin
      Light.Attenuation := Attenuation;
   end Set_Attenuation;

   ---------------
   -- Set_Color --
   ---------------

   overriding procedure Set_Color
     (Light : in out Rho_Light_Record;
      Color : Rho.Color.Rho_Color)
   is
   begin
      Light.Color := Color;
   end Set_Color;

   ---------------
   -- Set_Index --
   ---------------

   procedure Set_Index (Light : in out Rho_Light_Record;
                        Index : Rho.Limits.Light_Index)
   is
   begin
      Light.Index := Index;
   end Set_Index;

end Rho.Light;
