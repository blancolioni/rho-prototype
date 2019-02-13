package body Rho.Viewport is

   -----------
   -- Color --
   -----------

   overriding function Color
     (Viewport : Rho_Viewport_Record)
      return Rho.Color.Rho_Color
   is
   begin
      return Viewport.Clear_Color;
   end Color;

   ------------
   -- Height --
   ------------

   function Height (Viewport : Rho_Viewport_Record'Class) return Rho_Float is
   begin
      return Viewport.Height;
   end Height;

   ------------------
   -- New_Viewport --
   ------------------

   function New_Viewport
     (X, Y, Width, Height : Rho_Float)
      return Rho_Viewport
   is
   begin
      return new Rho_Viewport_Record'
        (Clear_Color => (0.0, 0.0, 0.0, 1.0),
         X           => X,
         Y           => Y,
         Width       => Width,
         Height      => Height);
   end New_Viewport;

   ---------------
   -- Set_Color --
   ---------------

   overriding procedure Set_Color
     (Viewport : in out Rho_Viewport_Record;
      Color    : Rho.Color.Rho_Color)
   is
   begin
      Viewport.Clear_Color := Color;
   end Set_Color;

   ----------------
   -- Set_Height --
   ----------------

   procedure Set_Height (Viewport : in out Rho_Viewport_Record'Class;
                         H        : Rho_Float)
   is
   begin
      Viewport.Height := H;
   end Set_Height;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position (Viewport : in out Rho_Viewport_Record'Class;
                           X, Y     : Rho_Float)
   is
   begin
      Viewport.Set_X (X);
      Viewport.Set_Y (Y);
   end Set_Position;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size (Viewport : in out Rho_Viewport_Record'Class;
                       W, H     : Rho_Float)
   is
   begin
      Viewport.Set_Width (W);
      Viewport.Set_Height (H);
   end Set_Size;

   ---------------
   -- Set_Width --
   ---------------

   procedure Set_Width (Viewport : in out Rho_Viewport_Record'Class;
                        W        : Rho_Float)
   is
   begin
      Viewport.Width := W;
   end Set_Width;

   -----------
   -- Set_X --
   -----------

   procedure Set_X (Viewport : in out Rho_Viewport_Record'Class;
                    X        : Rho_Float)
   is
   begin
      Viewport.X := X;
   end Set_X;

   -----------
   -- Set_Y --
   -----------

   procedure Set_Y (Viewport : in out Rho_Viewport_Record'Class;
                    Y        : Rho_Float)
   is
   begin
      Viewport.Y := Y;
   end Set_Y;

   -----------
   -- Width --
   -----------

   function Width (Viewport : Rho_Viewport_Record'Class) return Rho_Float is
   begin
      return Viewport.Width;
   end Width;

   -------
   -- X --
   -------

   function X (Viewport : Rho_Viewport_Record'Class) return Rho_Float is
   begin
      return Viewport.X;
   end X;

   -------
   -- Y --
   -------

   function Y (Viewport : Rho_Viewport_Record'Class) return Rho_Float is
   begin
      return Viewport.Y;
   end Y;

end Rho.Viewport;
