with Ada.Numerics;

with Rho.Elementary_Functions;
with Rho.Float_Arrays;
with Rho.Matrices;
with Rho.Render_Operation;

package body Rho.Shapes is

   ----------
   -- Cube --
   ----------

   function Cube
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Size    : Rho_Float := 1.0)
      return Rho.Entity.Rho_Entity
   is
      use Rho.Matrices;
      use Rho.Float_Arrays;

      type Vertex_Index is range 1 .. 8;
      type Face_Index is range 1 .. 6;
      type Face_Triangle_Index is range 1 .. 2;
      type Triangle_Vertex_Index is range 1 .. 3;

      Vertices : constant array (Vertex_Index) of Vector_3 :=
                     ((-1.0, -1.0,  1.0),
                      (1.0, -1.0,  1.0),
                      (1.0,  1.0,  1.0),
                      (-1.0,  1.0,  1.0),
                      (-1.0, -1.0, -1.0),
                      (1.0, -1.0, -1.0),
                      (1.0,  1.0, -1.0),
                      (-1.0,  1.0, -1.0));

      U_Start : constant array (Face_Index) of Unit_Float :=
                   (0.25, 0.5, 0.75, 0.0, 0.25, 0.25);
      V_Start : constant array (Face_Index) of Unit_Float :=
                   (0.25, 0.25, 0.25, 0.25, 0.0, 0.5);
      U_Offset : constant array
        (Face_Triangle_Index, Triangle_Vertex_Index)
        of Unit_Float :=
          ((0.0, 1.0, 1.0), (1.0, 0.0, 0.0));
      V_Offset         : constant array
        (Face_Triangle_Index, Triangle_Vertex_Index)
        of Unit_Float :=
          ((0.0, 0.0, 1.0), (1.0, 1.0, 0.0));

      Vertex_Indices : constant array
        (Face_Index, Face_Triangle_Index, Triangle_Vertex_Index)
        of Vertex_Index :=
          (((1, 2, 3), (3, 4, 1)), --  front
           ((2, 6, 7), (7, 3, 2)), --  right
           ((8, 7, 6), (6, 5, 8)), --  back
           ((5, 1, 4), (4, 8, 5)), --  left
           ((5, 6, 2), (2, 1, 5)), --  bottom
           ((4, 3, 7), (7, 8, 4))); --  top

      Normals         : constant array (Face_Index) of Vector_3 :=
                  ((0.0, 0.0, 1.0),
                   (1.0, 0.0, 0.0),
                   (0.0, 0.0, -1.0),
                   (-1.0, 0.0, 0.0),
                   (0.0, -1.0, 0.0),
                   (0.0, 1.0, 0.0));

      Result   : Rho.Entity.Rho_Entity;

   begin
      Rho.Entity.Rho_New (Result, Context);
      Result.Begin_Operation (Rho.Render_Operation.Triangle_List);

      for Face in Vertex_Indices'Range (1) loop
         for Triangle in Vertex_Indices'Range (2) loop
            for Triangle_Vertex in Vertex_Indices'Range (3) loop
               Result.Normal (Normals (Face));
               Result.Texture_Coordinate
                 (U_Start (Face) +
                      0.25 * U_Offset (Triangle, Triangle_Vertex),
                  V_Start (Face) +
                      0.25 * V_Offset (Triangle, Triangle_Vertex));
               Result.Vertex
                 (Vertices
                    (Vertex_Indices (Face, Triangle, Triangle_Vertex))
                  * Size);
            end loop;
         end loop;
      end loop;
      Result.End_Operation;
      return Result;
   end Cube;

   ------------------------
   -- Icosohedral_Sphere --
   ------------------------

   function Icosohedral_Sphere
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Detail  : Natural)
      return Rho.Entity.Rho_Entity
   is
      function White
        (X, Y, Z : Signed_Unit_Float)
         return Rho.Color.Rho_Color;

      -----------
      -- White --
      -----------

      function White
        (X, Y, Z : Signed_Unit_Float)
         return Rho.Color.Rho_Color
      is
         pragma Unreferenced (X, Y, Z);
      begin
         return (1.0, 1.0, 1.0, 1.0);
      end White;

   begin
      return Icosohedral_Sphere (Context, Detail, White'Access);
   end Icosohedral_Sphere;

   ------------------------
   -- Icosohedral_Sphere --
   ------------------------

   function Icosohedral_Sphere
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Detail  : Natural;
      Color   : not null access
        function (X, Y, Z : Signed_Unit_Float)
      return Rho.Color.Rho_Color)
      return Rho.Entity.Rho_Entity
   is
      use Rho.Matrices;
      X    : constant := 0.525731112119133606;
      Z    : constant := 0.850650808352039932;

      Result : Rho.Entity.Rho_Entity;

      Vertex_Data : constant array (1 .. 12) of Vector_3 :=
                      ((-X, 0.0, Z), (X, 0.0, Z), (-X, 0.0, -Z), (X, 0.0, -Z),
                       (0.0, Z, X), (0.0, Z, -X), (0.0, -Z, X), (0.0, -Z, -X),
                       (Z, X, 0.0), (-Z, X, 0.0), (Z, -X, 0.0), (-Z, -X, 0.0));
      Triangles   : constant array (1 .. 20, 1 .. 3) of Positive :=
               ((2, 5, 1), (5, 10, 1), (5, 6, 10), (9, 6, 5), (2, 9, 5),
                (2, 11, 9), (11, 4, 9), (9, 4, 6), (4, 3, 6), (4, 8, 3),
                (4, 11, 8), (11, 7, 8), (7, 12, 8), (7, 1, 12), (7, 2, 1),
                (11, 2, 7), (12, 1, 10), (3, 12, 10), (6, 3, 10), (12, 3, 8));

      procedure Subdivide
        (V1, V2, V3 : Vector_3;
         Depth      : Natural);

      ---------------
      -- Subdivide --
      ---------------

      procedure Subdivide
        (V1, V2, V3 : Vector_3;
         Depth      : Natural)
      is
         procedure Texture_Coord (V : Vector_3);

         -------------------
         -- Texture_Coord --
         -------------------

         procedure Texture_Coord (V : Vector_3) is
            use Rho.Elementary_Functions;
            Pi : constant := Ada.Numerics.Pi;
            S  : constant Rho_Float :=
                   (if V (1) = 0.0 and then V (3) = 0.0
                    then 0.0
                    else 0.5 * (1.0 + Arctan (V (3), V (1)) / Pi));
            T  : constant Rho_Float :=
                   Arccos (V (2)) / Pi;
         begin
            Result.Texture_Coordinate (1.0 - S, 1.0 - T);
         end Texture_Coord;

      begin
         if Depth = 0 then
            Texture_Coord (V1);
            Result.Normal (V1 (1), V1 (2), V1 (3));
            Result.Color (Color (V1 (1), V1 (2), V1 (3)));
            Result.Vertex (V1 (1), V1 (2), V1 (3));

            Texture_Coord (V2);
            Result.Normal (V2 (1), V2 (2), V2 (3));
            Result.Color (Color (V2 (1), V2 (2), V2 (3)));
            Result.Vertex (V2 (1), V2 (2), V2 (3));

            Texture_Coord (V3);
            Result.Normal (V3 (1), V3 (2), V3 (3));
            Result.Color (Color (V3 (1), V3 (2), V3 (3)));
            Result.Vertex (V3 (1), V3 (2), V3 (3));
         else
            declare
               use Rho.Float_Arrays;
               V12 : Vector_3 :=  (V1 + V2) / 2.0;
               V23 : Vector_3 :=  (V2 + V3) / 2.0;
               V31 : Vector_3 :=  (V3 + V1) / 2.0;
            begin
               V12 := V12 / abs V12;
               V23 := V23 / abs V23;
               V31 := V31 / abs V31;
               Subdivide (V1, V12, V31, Depth - 1);
               Subdivide (V2, V23, V12, Depth - 1);
               Subdivide (V3, V31, V23, Depth - 1);
               Subdivide (V12, V23, V31, Depth - 1);
            end;
         end if;
      end Subdivide;

   begin
      Rho.Entity.Rho_New (Result, Context);
      Result.Begin_Operation (Rho.Render_Operation.Triangle_List);
      for I in Triangles'Range (1) loop
         Subdivide (Vertex_Data (Triangles (I, 1)),
                    Vertex_Data (Triangles (I, 2)),
                    Vertex_Data (Triangles (I, 3)),
                    Detail);
      end loop;
      Result.End_Operation;
      return Result;
   end Icosohedral_Sphere;

   ------------------
   -- Quadric_Cone --
   ------------------

   function Quadric_Cone
     (Context    : not null access Rho.Context.Rho_Context_Record'Class;
      Slices     : Positive;
      Stacks     : Positive)
      return Rho.Entity.Rho_Entity
   is
      function Radius (Z : Signed_Unit_Float) return Unit_Float
      is (Z / 2.0 + 0.5);

      function Z_Value (Z : Signed_Unit_Float) return Signed_Unit_Float
      is (Z);

   begin
      return Quadric_Surface
        (Context, Slices, Stacks,
         Radius_Fn => Radius'Access,
         Z_Fn      => Z_Value'Access);
   end Quadric_Cone;

   -----------------------------
   -- Quadric_Conical_Frustum --
   -----------------------------

   function Quadric_Conical_Frustum
     (Context      : not null access Rho.Context.Rho_Context_Record'Class;
      Slices       : Positive;
      Stacks       : Positive;
      Radius_Ratio : Unit_Float)
      return Rho.Entity.Rho_Entity
   is
      function Radius (Z : Signed_Unit_Float) return Unit_Float
      is ((1.0 - Z) / 2.0 * (1.0 - Radius_Ratio) + Radius_Ratio);

      function Z_Value (Z : Signed_Unit_Float) return Signed_Unit_Float
      is (Z);

   begin
      return Quadric_Surface
        (Context, Slices, Stacks,
         Radius_Fn => Radius'Access,
         Z_Fn      => Z_Value'Access);
   end Quadric_Conical_Frustum;

   ----------------------
   -- Quadric_Cylinder --
   ----------------------

   function Quadric_Cylinder
     (Context    : not null access Rho.Context.Rho_Context_Record'Class;
      Slices     : Positive;
      Stacks     : Positive)
      return Rho.Entity.Rho_Entity
   is
      function Radius (Z : Signed_Unit_Float) return Unit_Float;

      function Radius (Z : Signed_Unit_Float) return Unit_Float is
         pragma Unreferenced (Z);
      begin
         return 1.0;
      end Radius;

      function Z_Value (Z : Signed_Unit_Float) return Signed_Unit_Float
      is (Z);

   begin
      return Quadric_Surface
        (Context, Slices, Stacks,
         Radius_Fn => Radius'Access,
         Z_Fn      => Z_Value'Access);
   end Quadric_Cylinder;

   --------------------
   -- Quadric_Sphere --
   --------------------

   function Quadric_Sphere
     (Context    : not null access Rho.Context.Rho_Context_Record'Class;
      Slices     : Positive;
      Stacks     : Positive)
      return Rho.Entity.Rho_Entity
   is

      use Rho.Elementary_Functions;

      function Radius (Z : Signed_Unit_Float) return Unit_Float
      is (Cos (Z * 90.0, 360.0));

      function Z_Value (Z : Signed_Unit_Float) return Signed_Unit_Float
      is (Sin (Z * 90.0, 360.0));

   begin

      return Quadric_Surface
        (Context, Slices, Stacks,
         Radius_Fn => Radius'Access,
         Z_Fn      => Z_Value'Access);

   end Quadric_Sphere;

   ---------------------
   -- Quadric_Surface --
   ---------------------

   function Quadric_Surface
     (Context    : not null access Rho.Context.Rho_Context_Record'Class;
      Slices     : Positive;
      Stacks     : Positive;
      Z_Min      : Signed_Unit_Float := -1.0;
      Z_Max      : Signed_Unit_Float := 1.0;
      Radius_Fn  : not null access
        function (Z : Signed_Unit_Float) return Unit_Float;
      Z_Fn       : not null access
        function (Z : Signed_Unit_Float) return Signed_Unit_Float)
      return Rho.Entity.Rho_Entity
   is
      use Rho.Matrices;
      use Rho.Render_Operation;

      Result  : Rho.Entity.Rho_Entity;
      Mesh    : array (0 .. Stacks, 0 .. Slices - 1) of Vector_3;
      Start_R : constant Non_Negative_Float := Radius_Fn (Z_Min);
      End_R   : constant Non_Negative_Float := Radius_Fn (Z_Max);
   begin

      Rho.Entity.Rho_New (Result, Context);

      for Latitude_Index in 0 .. Stacks loop
         declare
            Latitude : constant Signed_Unit_Float :=
                         Rho_Float (Latitude_Index)
                         / Rho_Float (Stacks)
                         * (Z_Max - Z_Min) + Z_Min;
            Z        : constant Signed_Unit_Float :=
                         Z_Fn (Latitude);
         begin
            for Longitude_Index in 0 .. Slices - 1 loop
               declare
                  use Rho.Elementary_Functions;
                  Longitude : constant Rho_Float :=
                                Rho_Float (Longitude_Index) * 360.0
                                / Rho_Float (Slices);
                  Radius    : constant Unit_Float :=
                                Radius_Fn (Latitude);
               begin
                  Mesh (Latitude_Index, Longitude_Index) :=
                    (1 => Radius * Cos (Longitude, 360.0),
                     2 => Radius * Sin (Longitude, 360.0),
                     3 => Z);
               end;
            end loop;
         end;
      end loop;

      Result.Begin_Operation
        (Triangle_List);

      for Latitude_Index in 0 .. Stacks - 1 loop
         for Longitude_Index in 0 .. Slices - 1 loop
            declare
               Long_1 : constant Natural :=
                          (if Longitude_Index < Slices - 1
                           then Longitude_Index + 1
                           else 0);
               V1     : constant Vector_3 :=
                          Mesh (Latitude_Index, Longitude_Index);
               V2     : constant Vector_3 :=
                          Mesh (Latitude_Index, Long_1);
               V3     : constant Vector_3 :=
                          Mesh (Latitude_Index + 1, Long_1);
               V4     : constant Vector_3 :=
                          Mesh (Latitude_Index + 1, Longitude_Index);
            begin
               Result.Texture_Coordinate
                 (Rho_Float (Longitude_Index) / Rho_Float (Slices),
                  Rho_Float (Latitude_Index) / Rho_Float (Stacks) / 2.0 + 0.5);
               Result.Normal (V1 (1), V1 (2), V1 (3));
               Result.Vertex (V1 (1), V1 (2), V1 (3));

               Result.Texture_Coordinate
                 (Rho_Float (Longitude_Index + 1) / Rho_Float (Slices),
                  Rho_Float (Latitude_Index) / Rho_Float (Stacks) / 2.0 + 0.5);
               Result.Normal (V2 (1), V2 (2), V2 (3));
               Result.Vertex (V2 (1), V2 (2), V2 (3));

               Result.Texture_Coordinate
                 (Rho_Float (Longitude_Index + 1) / Rho_Float (Slices),
                  Rho_Float (Latitude_Index + 1) / Rho_Float (Stacks)
                  / 2.0 + 0.5);
               Result.Normal (V3 (1), V3 (2), V3 (3));
               Result.Vertex (V3 (1), V3 (2), V3 (3));

               Result.Texture_Coordinate
                 (Rho_Float (Longitude_Index) / Rho_Float (Slices),
                  Rho_Float (Latitude_Index) / Rho_Float (Stacks) / 2.0 + 0.5);
               Result.Normal (V1 (1), V1 (2), V1 (3));
               Result.Vertex (V1 (1), V1 (2), V1 (3));

               Result.Texture_Coordinate
                 (Rho_Float (Longitude_Index + 1) / Rho_Float (Slices),
                  Rho_Float (Latitude_Index + 1) / Rho_Float (Stacks)
                  / 2.0 + 0.5);
               Result.Normal (V3 (1), V3 (2), V3 (3));
               Result.Vertex (V3 (1), V3 (2), V3 (3));

               Result.Texture_Coordinate
                 (Rho_Float (Longitude_Index) / Rho_Float (Slices),
                  Rho_Float (Latitude_Index + 1) / Rho_Float (Stacks)
                  / 2.0 + 0.5);
               Result.Normal (V4 (1), V4 (2), V4 (3));
               Result.Vertex (V4 (1), V4 (2), V4 (3));

               if Latitude_Index = 0 and then Start_R > 0.0 then
                  Result.Texture_Coordinate
                    (S => 0.0,
                     T => 0.0);
                  Result.Normal (0.0, 0.0, -1.0);
                  Result.Vertex (0.0, 0.0, Z_Min);

                  Result.Texture_Coordinate
                    (Rho_Float (Longitude_Index + 1) / Rho_Float (Slices),
                     Rho_Float (Latitude_Index)
                     / Rho_Float (Stacks) / 2.0 + 0.5);
                  Result.Normal (V2 (1), V2 (2), V2 (3));
                  Result.Vertex (V2 (1), V2 (2), V2 (3));

                  Result.Texture_Coordinate
                    (Rho_Float (Longitude_Index) / Rho_Float (Slices),
                     Rho_Float (Latitude_Index)
                     / Rho_Float (Stacks) / 2.0 + 0.5);
                  Result.Normal (V1 (1), V1 (2), V1 (3));
                  Result.Vertex (V1 (1), V1 (2), V1 (3));

               elsif Latitude_Index = Stacks - 1 and then End_R > 0.0 then

                  Result.Texture_Coordinate
                    (S => 1.0,
                     T => 0.0);

                  Result.Normal (0.0, 0.0, 1.0);
                  Result.Vertex (0.0, 0.0, Z_Max);

                  Result.Texture_Coordinate
                    (Rho_Float (Longitude_Index) / Rho_Float (Slices),
                     Rho_Float (Latitude_Index + 1) / Rho_Float (Stacks)
                     / 2.0 + 0.5);
                  Result.Normal (V4 (1), V4 (2), V4 (3));
                  Result.Vertex (V4 (1), V4 (2), V4 (3));

                  Result.Texture_Coordinate
                    (Rho_Float (Longitude_Index + 1) / Rho_Float (Slices),
                     Rho_Float (Latitude_Index + 1) / Rho_Float (Stacks)
                     / 2.0 + 0.5);
                  Result.Normal (V3 (1), V3 (2), V3 (3));
                  Result.Vertex (V3 (1), V3 (2), V3 (3));

               end if;
            end;
         end loop;

      end loop;

      Result.End_Operation;

      return Result;
   end Quadric_Surface;

   ------------
   -- Square --
   ------------

   function Square
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Size    : Rho_Float)
      return Rho.Entity.Rho_Entity
   is
      Entity : Rho.Entity.Rho_Entity;
   begin
      Rho.Entity.Rho_New (Entity, Context);
      Entity.Begin_Operation (Rho.Render_Operation.Triangle_Fan);

      Entity.Texture_Coordinate (0.0, 0.0);
      Entity.Normal (0.0, 0.0, 1.0);
      Entity.Vertex (-Size, -Size, 0.0);

      Entity.Texture_Coordinate (1.0, 0.0);
      Entity.Normal (0.0, 0.0, 1.0);
      Entity.Vertex (Size, -Size, 0.0);

      Entity.Texture_Coordinate (1.0, 1.0);
      Entity.Normal (0.0, 0.0, 1.0);
      Entity.Vertex (Size, Size, 0.0);

      Entity.Texture_Coordinate (0.0, 1.0);
      Entity.Normal (0.0, 0.0, 1.0);
      Entity.Vertex (-Size, Size, 0.0);

      Entity.End_Operation;

      return Entity;
   end Square;

end Rho.Shapes;
