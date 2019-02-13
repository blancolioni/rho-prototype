with Ada.Numerics;
with Ada.Text_IO;

with Rho.Assets;
with Rho.Elementary_Functions;
with Rho.Entity;
with Rho.Node;
with Rho.Camera;
with Rho.Light;
with Rho.Matrices;
with Rho.Render_Operation;
with Rho.Float_Arrays;

with Rho.Paths;
with Rho.Materials.Material;
with Rho.Shapes;
with Rho.Texture.Loader;

with Rho.Float_Images;

package body Demos.Faces is

   use Rho;

   Face_Points_Across : constant := 100;
   Face_Points_Down   : constant := 100;

   Pi : constant := Ada.Numerics.Pi;

   type Face_Grid_Array is
     array (Positive range <>, Positive range <>)
     of Rho.Matrices.Vector_3;

   type Random_Face_Demo is
     new Root_Demo_Type with null record;

   overriding function Identity (Demo : Random_Face_Demo) return String
   is ("face-demo");

   overriding function Name (Demo : Random_Face_Demo) return String
   is ("Face Generation Demo");

   overriding function Description (Demo : Random_Face_Demo) return String
   is ("Random face generation example");

   overriding function Create_Scene
     (Demo : in out Random_Face_Demo)
      return Rho.Scene.Rho_Scene;

   procedure Create_Face_Grid
     (Pts : in out Face_Grid_Array);

   procedure Create_Face_Normals
     (Pts : Face_Grid_Array;
      Normals : out Face_Grid_Array);

   function Base_Coordinate
     (DX, DY : Rho.Signed_Unit_Float)
      return Rho.Matrices.Vector_3
     with Unreferenced;

   function Face_Fn (X, Y : Rho_Float) return Rho_Float;

   function Eye_Fn (X, Y : Rho_Float) return Rho_Float;
   function Nose_Fn (X, Y : Rho_Float) return Rho_Float;

   function Bivariate_Normal
     (Rho, Sigma_X, Sigma_Y, Mu_X, Mu_Y, X, Y : Rho_Float)
      return Rho_Float;

   ---------------------
   -- Base_Coordinate --
   ---------------------

   function Base_Coordinate
     (DX, DY : Rho.Signed_Unit_Float)
      return Rho.Matrices.Vector_3
   is
      X : constant Rho_Float := DX * 0.5;
      Y : constant Rho_Float := DY * 0.6;
--        Z : constant Rho_Float :=
--              Rho.Elementary_Functions.Sqrt
--                (1.0 - X ** 2 - Y ** 2);
   begin
      return (X, Y, Face_Fn (X, Y));
   end Base_Coordinate;

   ----------------------
   -- Bivariate_Normal --
   ----------------------

   function Bivariate_Normal
     (Rho, Sigma_X, Sigma_Y, Mu_X, Mu_Y, X, Y : Rho_Float)
      return Rho_Float
   is
      use Elementary_Functions;
   begin
      return 1.0 / (2.0 * Pi * Sigma_X * Sigma_Y * Sqrt (1.0 - Rho ** 2))
        * Exp (-1.0 / (2.0 * (1.0 - Rho ** 2))
               * ((X - Mu_X) ** 2 / Sigma_X ** 2
                 + (Y - Mu_Y) ** 2 / Sigma_Y ** 2
                 - 2.0 * Rho * (X - Mu_X) * (Y - Mu_Y) / Sigma_X / Sigma_Y));
   end Bivariate_Normal;

   ----------------------
   -- Create_Face_Grid --
   ----------------------

   procedure Create_Face_Grid
     (Pts : in out Face_Grid_Array)
   is
      use Ada.Text_IO;
   begin

      for X in Pts'Range (1) loop
         Set_Col (Count (10 + 8 * X));
         Put
           (Rho.Float_Images.Image
              (Rho_Float (X - 1) / Rho_Float (Pts'Length (1) - 1)
               * 2.0 - 1.0));
      end loop;

      New_Line;

      for Y in Pts'Range (2) loop
         declare
            DY : constant Rho_Float :=
                   Rho_Float (Pts'Length (2) - Y)
                   / Rho_Float (Pts'Length (2) - 1)
                   * 2.0 - 1.0;
         begin
            Put (Rho.Float_Images.Image (DY));

            for X in Pts'Range (1) loop
               declare
                  DX : constant Rho_Float :=
                         Rho_Float (X - 1) / Rho_Float (Pts'Length (1) - 1)
                         * 2.0 - 1.0;
               begin
                  Pts (X, Y) := (DX, DY, Face_Fn (DX, DY));
                  Set_Col (Count (10 + 8 * X));
                  Put (Rho.Float_Images.Image (Pts (X, Y) (3)));
               end;
            end loop;
            New_Line;
         end;
      end loop;
   end Create_Face_Grid;

   -------------------------
   -- Create_Face_Normals --
   -------------------------

   procedure Create_Face_Normals
     (Pts     : Face_Grid_Array;
      Normals : out Face_Grid_Array)
   is
   begin
      for Y in Pts'Range (2) loop
         for X in Pts'Range (1) loop
            declare
               use Rho.Matrices, Rho.Float_Arrays;
               Total : Vector_3 := (0.0, 0.0, 0.0);
            begin
               for DY in -1 .. 1 loop
                  for DX in -1 .. 1 loop
                     if X + DX in Pts'Range (1)
                       and then Y + DY in Pts'Range (2)
                     then
                        Total := Total + Pts (X + DX, Y + DY);
                     end if;
                  end loop;
               end loop;
               Normals (X, Y) := Total / abs Total;
            end;
         end loop;
      end loop;
   end Create_Face_Normals;

   ------------------
   -- Create_Scene --
   ------------------

   overriding function Create_Scene
     (Demo : in out Random_Face_Demo)
      return Rho.Scene.Rho_Scene
   is
      pragma Unreferenced (Demo);
      Scene     : constant Rho.Scene.Rho_Scene := Rho.Scene.Create_Scene;
      Camera    : constant Rho.Camera.Rho_Camera := Scene.Active_Camera;
      Node      : constant Rho.Node.Rho_Node := Scene.Create_Node ("face");
      Light     : Rho.Light.Rho_Light;
      Face      : Rho.Entity.Rho_Entity;
      Face_Pts  : Face_Grid_Array
        (1 .. Face_Points_Across, 1 .. Face_Points_Down);
      Face_Normals : Face_Grid_Array (Face_Pts'Range (1), Face_Pts'Range (2));
      Texture      : constant Rho.Texture.Rho_Texture :=
                       Rho.Texture.Loader.Load_Texture
                         (Rho.Paths.Config_File
                            ("textures/PinkSkin.png"));
      Eyeball      : constant Rho.Entity.Rho_Entity :=
                       Rho.Shapes.Square (1.0);
--                       Rho.Shapes.Icosohedral_Sphere (2);
      Eyeball_Texture : constant Rho.Texture.Rho_Texture :=
                          Rho.Texture.Loader.Load_Texture
                            (Rho.Paths.Config_File
                               ("textures/BlueEyeball.png"));
      Left_Eye        : constant Rho.Node.Rho_Node :=
                          Scene.Create_Node ("left-eye");
      Right_Eye       : constant Rho.Node.Rho_Node :=
                          Scene.Create_Node ("right-eye");
   begin

      Eyeball.Set_Texture (Eyeball_Texture);
      Left_Eye.Set_Entity (Eyeball);
      Left_Eye.Scale (0.15);
      Left_Eye.Set_Position (-0.3, 0.3, 0.0);
      Right_Eye.Set_Entity (Eyeball);
      Right_Eye.Scale (0.15);
      Right_Eye.Set_Position (0.3, 0.3, 0.0);

      Create_Face_Grid (Face_Pts);
      Create_Face_Normals (Face_Pts, Face_Normals);

      Rho.Entity.Rho_New (Face);
      Face.Begin_Operation (Rho.Render_Operation.Triangle_List);
      for Y in 1 .. Face_Points_Down - 1 loop
         for X in 1 .. Face_Points_Across - 1 loop
            declare
               use Rho.Matrices;
               P1 : constant Vector_3 := Face_Pts (X, Y);
               P2 : constant Vector_3 := Face_Pts (X, Y + 1);
               P3 : constant Vector_3 := Face_Pts (X + 1, Y);
               P4 : constant Vector_3 := Face_Pts (X + 1, Y + 1);
               N1 : constant Vector_3 := Face_Normals (X, Y);
               N2 : constant Vector_3 := Face_Normals (X + 1, Y);
               N3 : constant Vector_3 := Face_Normals (X, Y + 1);
               N4 : constant Vector_3 := Face_Normals (X + 1, Y + 1);
            begin
               Face.Texture_Coordinate
                 (Rho_Float (X - 1) / Rho_Float (Face_Points_Across - 1),
                  Rho_Float (Y - 1) / Rho_Float (Face_Points_Down - 1));
               Face.Normal (N1);
               Face.Vertex (P1);
               Face.Texture_Coordinate
                 (Rho_Float (X) / Rho_Float (Face_Points_Across - 1),
                  Rho_Float (Y - 1) / Rho_Float (Face_Points_Down - 1));
               Face.Normal (N2);
               Face.Vertex (P2);
               Face.Texture_Coordinate
                 (Rho_Float (X - 1) / Rho_Float (Face_Points_Across - 1),
                  Rho_Float (Y) / Rho_Float (Face_Points_Down - 1));
               Face.Normal (N3);
               Face.Vertex (P3);
               Face.Texture_Coordinate
                 (Rho_Float (X) / Rho_Float (Face_Points_Across - 1),
                  Rho_Float (Y) / Rho_Float (Face_Points_Down - 1));
               Face.Normal (N4);
               Face.Vertex (P4);
               Face.Texture_Coordinate
                 (Rho_Float (X - 1) / Rho_Float (Face_Points_Across - 1),
                  Rho_Float (Y) / Rho_Float (Face_Points_Down - 1));
               Face.Normal (P3);
               Face.Vertex (P3);
               Face.Texture_Coordinate
                 (Rho_Float (X) / Rho_Float (Face_Points_Across - 1),
                  Rho_Float (Y - 1) / Rho_Float (Face_Points_Down - 1));
               Face.Normal (N2);
               Face.Vertex (P2);
            end;
         end loop;
      end loop;
      Face.End_Operation;

      if False then
         Face.Set_Material (Rho.Assets.Material ("blue"));
      else
         Face.Set_Material (Rho.Materials.Material.Rho_New_With_Texture
                            ("face", Texture, True));
      end if;

      Camera.Set_Position (0.0, 0.0, 5.0);
      Camera.Look_At (0.0, 1.0, 0.0, 0.0, 0.0, 0.0);
      Camera.Perspective
        (30.0, 2.0, 10.0);

      Rho.Light.Rho_New (Light, Rho.Light.Ambient);
      Light.Set_Position (10.0, 3.0, 3.0);
      Light.Set_Color (0.5, 0.5, 0.5, 1.0);
      Scene.Add_Light (Light);
      Node.Set_Entity (Face);

      return Scene;

   end Create_Scene;

   ------------
   -- Eye_Fn --
   ------------

   function Eye_Fn (X, Y : Rho_Float) return Rho_Float is
      Correlation : constant := 0.1;
      Sigma_X     : constant := 0.1;
      Sigma_Y     : constant := 0.1;

      Left_Eye    : constant Rho_Float :=
                      Bivariate_Normal
                        (Rho     => Correlation,
                         Sigma_X => Sigma_X,
                         Sigma_Y => Sigma_Y,
                         Mu_X    => -0.3,
                         Mu_Y    => 0.3,
                         X       => X,
                         Y       => Y);
      Right_Eye    : constant Rho_Float :=
                       Bivariate_Normal
                        (Rho     => Correlation,
                         Sigma_X => Sigma_X,
                         Sigma_Y => Sigma_Y,
                         Mu_X    => 0.3,
                         Mu_Y    => 0.3,
                         X       => X,
                         Y       => Y);
      use Rho.Float_Images;
   begin
      return Z : constant Rho_Float :=
        1.0 - 0.1 * Left_Eye - 0.1 * Right_Eye
      do
         if False then
            Ada.Text_IO.Put_Line
              ("eye: x = " & Image (X) & " y = " & Image (Y)
               & " L = " & Image (Left_Eye)
               & " R = " & Image (Right_Eye)
               & ": z = " & Image (Z));
         end if;
      end return;
   end Eye_Fn;

   ---------------
   -- Face_Demo --
   ---------------

   function Face_Demo return Rho_Demo_Type is
   begin
      return new Random_Face_Demo;
   end Face_Demo;

   -------------
   -- Face_Fn --
   -------------

   function Face_Fn (X, Y : Rho_Float) return Rho_Float is
   begin
      return 1.0 * Nose_Fn (X, Y) + Eye_Fn (X, Y);
   end Face_Fn;

   -------------
   -- Nose_Fn --
   -------------

   function Nose_Fn (X, Y : Rho_Float) return Rho_Float is
      use Rho.Elementary_Functions;
   begin
      if Y in -0.1 .. 0.3 then
         return 2.0 * Exp (-(5.0 * X) ** 2) * (0.7 - Y);
      else
         return 0.0;
      end if;
   end Nose_Fn;

end Demos.Faces;
