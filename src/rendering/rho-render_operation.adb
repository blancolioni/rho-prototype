package body Rho.Render_Operation is

   ------------
   -- Normal --
   ------------

   procedure Normal
     (Item    : in out Rho_Render_Operation_Interface'Class;
      X, Y, Z : Integer)
   is
   begin
      Item.Normal
        ((Rho_Float (X), Rho_Float (Y), Rho_Float (Z)));
   end Normal;

   ------------
   -- Normal --
   ------------

   procedure Normal
     (Item    : in out Rho_Render_Operation_Interface'Class;
      X, Y, Z : Rho_Float)
   is
   begin
      Item.Normal
        ((X, Y, Z));
   end Normal;

   ------------------------
   -- Texture_Coordinate --
   ------------------------

   procedure Texture_Coordinate
     (Item    : in out Rho_Render_Operation_Interface'Class;
      S, T    : Integer)
   is
   begin
      Item.Texture_Coordinate
        (Rho_Float (S), Rho_Float (T));
   end Texture_Coordinate;

   ------------
   -- Vertex --
   ------------

   procedure Vertex
     (Item    : in out Rho_Render_Operation_Interface'Class;
      X, Y, Z : Integer)
   is
   begin
      Item.Vertex
        ((Rho_Float (X), Rho_Float (Y), Rho_Float (Z)));
   end Vertex;

   ------------
   -- Vertex --
   ------------

   procedure Vertex
     (Item    : in out Rho_Render_Operation_Interface'Class;
      X, Y, Z : Rho_Float)
   is
   begin
      Item.Vertex
        ((X, Y, Z));
   end Vertex;

   ------------
   -- Vertex --
   ------------

   procedure Vertex
     (Item    : in out Rho_Render_Operation_Interface'Class;
      X, Y    : Integer)
   is
   begin
      Item.Vertex
        ((Rho_Float (X), Rho_Float (Y), 0.0));
   end Vertex;

   ------------
   -- Vertex --
   ------------

   procedure Vertex
     (Item    : in out Rho_Render_Operation_Interface'Class;
      X, Y    : Rho_Float)
   is
   begin
      Item.Vertex
        ((X, Y, 0.0));
   end Vertex;

end Rho.Render_Operation;
