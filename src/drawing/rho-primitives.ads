with Ada.Finalization;

with Rho.Color;
with Rho.Matrices;

package Rho.Primitives is

   type Root_Primitive is abstract tagged limited private;

   procedure Begin_Primitive (Item : in out Root_Primitive) is abstract;

   procedure Array_Element
     (Item  : in out Root_Primitive'Class;
      Index : in     Positive);

   procedure Edge (Item    : in out Root_Primitive'Class;
                   Enabled : Boolean);

   procedure Color (Item    : in out Root_Primitive'Class;
                    Color   : Rho.Color.Rho_Color);

   procedure Normal (Item    : in out Root_Primitive'Class;
                     Vector  : Rho.Matrices.Vector_3);

   procedure Normal (Item    : in out Root_Primitive'Class;
                     X, Y, Z : Integer);

   procedure Normal (Item    : in out Root_Primitive'Class;
                     X, Y, Z : Rho_Float);

   procedure Vertex (Item    : in out Root_Primitive'Class;
                     Vector  : Rho.Matrices.Vector_3);

   procedure Vertex (Item    : in out Root_Primitive'Class;
                     X, Y, Z : Integer);

   procedure Vertex (Item    : in out Root_Primitive'Class;
                     X, Y, Z : Rho_Float);

   procedure Vertex (Item    : in out Root_Primitive'Class;
                     X, Y    : Integer);

   procedure Vertex (Item    : in out Root_Primitive'Class;
                     X, Y    : Rho_Float);

   procedure Texture_Coordinate
     (Item    : Root_Primitive'Class;
      S, T    : Integer);

   procedure Texture_Coordinate
     (Item    : Root_Primitive'Class;
      S, T    : Rho_Float);

   type Triangles is limited new Root_Primitive with private;
   type Triangle_Strip is limited new Root_Primitive with private;
   type Lines is limited new Root_Primitive with private;
   type Line_Strip is limited new Root_Primitive with private;
   type Quads is limited new Root_Primitive with private;
   type Polygon is limited new Root_Primitive with private;

private

   type Root_Primitive is
     abstract new Ada.Finalization.Limited_Controlled with
      record
         Executing : Boolean := False;
      end record;

   overriding
   procedure Initialize (Item : in out Root_Primitive);

   overriding
   procedure Finalize (Item : in out Root_Primitive);

   type Triangles is limited new Root_Primitive with null record;

   overriding
   procedure Begin_Primitive (Item : in out Triangles);

   type Triangle_Strip is limited new Root_Primitive with null record;

   overriding
   procedure Begin_Primitive (Item : in out Triangle_Strip);

   type Lines is limited new Root_Primitive with null record;

   overriding
   procedure Begin_Primitive (Item : in out Lines);

   type Line_Strip is limited new Root_Primitive with null record;

   overriding
   procedure Begin_Primitive (Item : in out Line_Strip);

   type Quads is limited new Root_Primitive with null record;

   overriding
   procedure Begin_Primitive (Item : in out Quads);

   type Polygon is limited new Root_Primitive with null record;

   overriding
   procedure Begin_Primitive (Item : in out Polygon);

end Rho.Primitives;
