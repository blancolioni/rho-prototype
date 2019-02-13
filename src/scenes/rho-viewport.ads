with Rho.Color;

package Rho.Viewport is

   type Rho_Viewport_Record is
     new Rho.Color.Rho_Has_Color
       with private;

   overriding procedure Set_Color
     (Viewport : in out Rho_Viewport_Record;
      Color    : Rho.Color.Rho_Color);

   overriding function Color
     (Viewport : Rho_Viewport_Record)
      return Rho.Color.Rho_Color;

   function Width (Viewport : Rho_Viewport_Record'Class) return Rho_Float;
   function Height (Viewport : Rho_Viewport_Record'Class) return Rho_Float;
   function X (Viewport : Rho_Viewport_Record'Class) return Rho_Float;
   function Y (Viewport : Rho_Viewport_Record'Class) return Rho_Float;

   procedure Set_Size (Viewport : in out Rho_Viewport_Record'Class;
                       W, H     : Rho_Float);

   procedure Set_Position (Viewport : in out Rho_Viewport_Record'Class;
                           X, Y     : Rho_Float);

   procedure Set_Height (Viewport : in out Rho_Viewport_Record'Class;
                         H        : Rho_Float);
   procedure Set_Width (Viewport : in out Rho_Viewport_Record'Class;
                        W        : Rho_Float);
   procedure Set_X (Viewport : in out Rho_Viewport_Record'Class;
                    X        : Rho_Float);
   procedure Set_Y (Viewport : in out Rho_Viewport_Record'Class;
                    Y        : Rho_Float);

   function Aspect_Ratio
     (Viewport : Rho_Viewport_Record'Class)
      return Rho_Float
   is (Viewport.Width / Viewport.Height);

   type Rho_Viewport is access all Rho_Viewport_Record'Class;

   function New_Viewport
     (X, Y, Width, Height : Rho_Float)
     return Rho_Viewport;

private

   type Rho_Viewport_Record is
     new Rho.Color.Rho_Has_Color with
      record
         Clear_Color   : Rho.Color.Rho_Color := (0.0, 0.0, 0.0, 1.0);
         X, Y          : Rho_Float := 0.0;
         Width, Height : Rho_Float := 100.0;
      end record;

end Rho.Viewport;
