package Rho.Rectangle is

   type Rho_Rectangle is
      record
         X, Y : Rho_Float;
         Width, Height : Rho_Float;
      end record;

   function Contains_Point
     (Item : Rho_Rectangle;
      X, Y : Rho_Float)
      return Boolean;

   type Rho_Rectangle_Interface is interface;

   procedure Set_Rectangle (Item : in out Rho_Rectangle_Interface;
                            Rectangle : Rho_Rectangle)
   is abstract;

   function Get_Rectangle (Item : Rho_Rectangle_Interface)
                           return Rho_Rectangle
                           is abstract;

   function Contains_Point
     (Item : Rho_Rectangle_Interface'Class;
      X, Y : Rho_Float)
      return Boolean;

   function Width
     (Rectangle : Rho_Rectangle_Interface'Class)
      return Rho_Float;

   function Height
     (Rectangle : Rho_Rectangle_Interface'Class)
      return Rho_Float;

   function X (Rectangle : Rho_Rectangle_Interface'Class) return Rho_Float;
   function Y (Rectangle : Rho_Rectangle_Interface'Class) return Rho_Float;

   procedure Set_Size
     (Rectangle : in out Rho_Rectangle_Interface'Class;
      W, H      : Rho_Float);

   procedure Set_Position (Rectangle : in out Rho_Rectangle_Interface'Class;
                           X, Y     : Rho_Float);

   procedure Set_Height (Rectangle : in out Rho_Rectangle_Interface'Class;
                         H        : Rho_Float);
   procedure Set_Width (Rectangle : in out Rho_Rectangle_Interface'Class;
                        W        : Rho_Float);
   procedure Set_X (Rectangle : in out Rho_Rectangle_Interface'Class;
                    X        : Rho_Float);
   procedure Set_Y (Rectangle : in out Rho_Rectangle_Interface'Class;
                    Y        : Rho_Float);

   procedure Specify_X
     (Rectangle     : Rho_Rectangle_Interface'Class;
      Specification : String;
      X             : in out Rho_Float);

   procedure Specify_Y
     (Rectangle     : Rho_Rectangle_Interface'Class;
      Specification : String;
      Y             : in out Rho_Float);

end Rho.Rectangle;
