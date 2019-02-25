with Rho.Color;
with Rho.Matrices;

package Rho.Render_Operation is

   type Operation_Type is
     (Point_List, Line_List, Line_Strip,
      Triangle_List, Triangle_Strip, Triangle_Fan);

   type Rho_Render_Operation_Interface is interface;

   subtype Rho_Render_Operation is Rho_Render_Operation_Interface'Class;

   procedure Begin_Operation
     (Item      : in out Rho_Render_Operation_Interface;
      Operation : Operation_Type)
   is abstract;

   procedure End_Operation (Item : in out Rho_Render_Operation_Interface)
   is abstract;

   procedure Color (Item    : in out Rho_Render_Operation_Interface;
                    Color   : Rho.Color.Rho_Color)
   is abstract;

   procedure Normal (Item    : in out Rho_Render_Operation_Interface;
                     Vector  : Rho.Matrices.Vector_3)
   is abstract;

   procedure Vertex (Item    : in out Rho_Render_Operation_Interface;
                     Vector  : Rho.Matrices.Vector_3)
   is abstract;

   procedure Texture_Coordinate
     (Item    : in out Rho_Render_Operation_Interface;
      S, T    : Rho_Float)
   is abstract;

   procedure Normal (Item    : in out Rho_Render_Operation_Interface'Class;
                     X, Y, Z : Integer);

   procedure Normal (Item    : in out Rho_Render_Operation_Interface'Class;
                     X, Y, Z : Rho_Float);

   procedure Vertex (Item    : in out Rho_Render_Operation_Interface'Class;
                     X, Y, Z : Integer);

   procedure Vertex (Item    : in out Rho_Render_Operation_Interface'Class;
                     X, Y, Z : Rho_Float);

   procedure Vertex (Item    : in out Rho_Render_Operation_Interface'Class;
                     X, Y    : Integer);

   procedure Vertex (Item    : in out Rho_Render_Operation_Interface'Class;
                     X, Y    : Rho_Float);

   procedure Texture_Coordinate
     (Item    : in out Rho_Render_Operation_Interface'Class;
      S, T    : Integer);

end Rho.Render_Operation;
