package body Rho.Rectangle is

   --------------------
   -- Contains_Point --
   --------------------

   function Contains_Point
     (Item : Rho_Rectangle;
      X, Y : Rho_Float)
      return Boolean
   is
   begin
      return X in Item.X .. Item.X + Item.Width
        and then Y in Item.Y .. Item.Y + Item.Height;
   end Contains_Point;

   --------------------
   -- Contains_Point --
   --------------------

   function Contains_Point
     (Item : Rho_Rectangle_Interface'Class;
      X, Y : Rho_Float)
      return Boolean
   is
   begin
      return X in Item.X .. Item.X + Item.Width
        and then Y in Item.Y .. Item.Y + Item.Height;
   end Contains_Point;

   ------------
   -- Height --
   ------------

   function Height
     (Rectangle : Rho_Rectangle_Interface'Class)
      return Rho_Float
   is
   begin
      return Rectangle.Get_Rectangle.Height;
   end Height;

   ----------------
   -- Set_Height --
   ----------------

   procedure Set_Height (Rectangle : in out Rho_Rectangle_Interface'Class;
                         H        : Rho_Float)
   is
      R : Rho_Rectangle := Rectangle.Get_Rectangle;
   begin
      R.Height := H;
      Rectangle.Set_Rectangle (R);
   end Set_Height;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position (Rectangle : in out Rho_Rectangle_Interface'Class;
                           X, Y     : Rho_Float)
   is
      R : Rho_Rectangle := Rectangle.Get_Rectangle;
   begin
      R.X := X;
      R.Y := Y;
      Rectangle.Set_Rectangle (R);
   end Set_Position;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size (Rectangle : in out Rho_Rectangle_Interface'Class;
                       W, H     : Rho_Float)
   is
      R : Rho_Rectangle := Rectangle.Get_Rectangle;
   begin
      R.Width := W;
      R.Height := H;
      Rectangle.Set_Rectangle (R);
   end Set_Size;

   ---------------
   -- Set_Width --
   ---------------

   procedure Set_Width (Rectangle : in out Rho_Rectangle_Interface'Class;
                        W        : Rho_Float)
   is
      R : Rho_Rectangle := Rectangle.Get_Rectangle;
   begin
      R.Width := W;
      Rectangle.Set_Rectangle (R);
   end Set_Width;

   -----------
   -- Set_X --
   -----------

   procedure Set_X (Rectangle : in out Rho_Rectangle_Interface'Class;
                    X        : Rho_Float)
   is
      R : Rho_Rectangle := Rectangle.Get_Rectangle;
   begin
      R.X := X;
      Rectangle.Set_Rectangle (R);
   end Set_X;

   -----------
   -- Set_Y --
   -----------

   procedure Set_Y (Rectangle : in out Rho_Rectangle_Interface'Class;
                    Y        : Rho_Float)
   is
      R : Rho_Rectangle := Rectangle.Get_Rectangle;
   begin
      R.Y := Y;
      Rectangle.Set_Rectangle (R);
   end Set_Y;

   ---------------
   -- Specify_X --
   ---------------

   procedure Specify_X
     (Rectangle     : Rho_Rectangle_Interface'Class;
      Specification : String;
      X             : in out Rho_Float)
   is
   begin
      if Specification = "left" then
         X := Rectangle.X;
      elsif Specification = "right" then
         X := Rectangle.X + Rectangle.Width;
      elsif Specification = "centre" or else Specification = "center" then
         X := Rectangle.X + Rectangle.Width / 2.0;
      else
         begin
            X := Rho_Float'Value (Specification);
            X := Rectangle.X + Rectangle.Height * X;
         exception
            when Constraint_Error =>
               raise Constraint_Error with
                 "Specify_X: cannot understand " & Specification;
         end;
      end if;
   end Specify_X;

   ---------------
   -- Specify_Y --
   ---------------

   procedure Specify_Y
     (Rectangle     : Rho_Rectangle_Interface'Class;
      Specification : String;
      Y             : in out Rho_Float)
   is
   begin
      if Specification = "top" then
         Y := Rectangle.Y;
      elsif Specification = "bottom" then
         Y := Rectangle.Y + Rectangle.Height;
      elsif Specification = "centre" or else Specification = "center" then
         Y := Rectangle.Y + Rectangle.Height / 2.0;
      else
         begin
            Y := Rho_Float'Value (Specification);
            Y := Rectangle.Y + Rectangle.Height * Y;
         exception
            when Constraint_Error =>
               raise Constraint_Error with
                 "Specify_Y: cannot understand " & Specification;
         end;
      end if;
   end Specify_Y;

   -----------
   -- Width --
   -----------

   function Width
     (Rectangle : Rho_Rectangle_Interface'Class)
      return Rho_Float
   is
   begin
      return Rectangle.Get_Rectangle.Width;
   end Width;

   -------
   -- X --
   -------

   function X (Rectangle : Rho_Rectangle_Interface'Class) return Rho_Float is
   begin
      return Rectangle.Get_Rectangle.X;
   end X;

   -------
   -- Y --
   -------

   function Y (Rectangle : Rho_Rectangle_Interface'Class) return Rho_Float is
   begin
      return Rectangle.Get_Rectangle.Y;
   end Y;

end Rho.Rectangle;
