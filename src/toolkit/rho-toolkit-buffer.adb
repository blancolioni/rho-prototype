with Ada.Directories;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Glib;

with Cairo.Image_Surface;
with Cairo.Png;

with Rho.Matrices;
with Rho.Render_Operation;
with Rho.Shaders.Load;

package body Rho.Toolkit.Buffer is

   Use_Textures : constant Boolean := True;

   Tile_Size : constant := 64;

   Shader : Rho.Shaders.Rho_Shader := null;
--     Tex    : Rho.Shaders.Rho_Uniform_Value;

   procedure Free is
     new Ada.Unchecked_Deallocation (Buffer_Tile_Array,
                                     Buffer_Tile_Array_Access);

   function Create_Tile_Entity
     (X, Y           : Natural;
      Buffer_Height  : Natural;
      Surface_Height : Natural;
      Material       : Rho.Materials.Material.Rho_Material)
      return Rho.Entity.Rho_Entity;

   ------------
   -- Create --
   ------------

   function Create
     (Geometry : Rho.Rectangle.Rho_Rectangle)
      return Rho_Buffer
   is
      use type Rho.Shaders.Rho_Shader;
   begin
      if Shader = null then
         Shader := Rho.Shaders.Load.Load_Standard_Shader ("rho_toolkit");
      end if;

      return Buffer : constant Rho_Buffer := new Rho_Buffer_Record do
         Buffer.Set_Rectangle (Geometry);
      end return;
   end Create;

   ------------------------
   -- Create_Tile_Entity --
   ------------------------

   function Create_Tile_Entity
     (X, Y           : Natural;
      Buffer_Height  : Natural;
      Surface_Height : Natural;
      Material       : Rho.Materials.Material.Rho_Material)
      return Rho.Entity.Rho_Entity
   is
      pragma Unreferenced (X, Y, Buffer_Height);
      pragma Unreferenced (Surface_Height);
      Entity : Rho.Entity.Rho_Entity;
      X1     : constant Rho_Float := 0.0;
      X2     : constant Rho_Float := X1 + Rho_Float (Tile_Size);
      Y2     : constant Rho_Float := Rho_Float (Tile_Size);
      Y1     : constant Rho_Float := Y2 - Rho_Float (Tile_Size);
   begin

      Rho.Entity.Rho_New (Entity);

      Entity.Begin_Operation (Rho.Render_Operation.Quad_List);

      Entity.Texture_Coordinate (0.0, 0.0);
      Entity.Vertex (X1, Y1, 0.0);

      Entity.Texture_Coordinate (1.0, 0.0);
      Entity.Vertex (X2, Y1, 0.0);

      Entity.Texture_Coordinate (1.0, 1.0);
      Entity.Vertex (X2, Y2, 0.0);

      Entity.Texture_Coordinate (0.0, 1.0);
      Entity.Vertex (X1, Y2, 0.0);

      Entity.End_Operation;

      Entity.Set_Material (Material);

      Entity.Load;

      return Entity;

   end Create_Tile_Entity;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Buffer : in out Rho_Buffer) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Rho_Buffer_Record'Class,
                                        Rho_Buffer);

   begin
      if Buffer.Tiles /= null then
         for X in Buffer.Tiles'Range (1) loop
            for Y in Buffer.Tiles'Range (2) loop
               if Use_Textures then
                  Buffer.Tiles (X, Y).Texture.Destroy;
                  Buffer.Tiles (X, Y).Material.Destroy;
               end if;
               Buffer.Tiles (X, Y).Object.Destroy;
            end loop;
         end loop;
         Free (Buffer.Tiles);
         Cairo.Surface_Destroy (Buffer.Surface);
      end if;
      Free (Buffer);
   end Destroy;

   --------------------
   -- Execute_Render --
   --------------------

   overriding procedure Execute_Render
     (Item   : in out Rho_Buffer_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
      Current_Depth_Test : constant Boolean := Target.Depth_Test;
   begin
      --        Target.Push_Shader (Shader);

      Target.Set_Depth_Test (False);
      Target.Matrix_Mode (Rho.Matrices.Projection);
      Target.Push_Matrix;
      Target.Load_Identity;
      Target.Ortho (0.0, Target.Viewport.Width,
                    0.0, Target.Viewport.Height, 1.0, -1.0);
      Target.Clear_Matrix_Saved;

      Target.Matrix_Mode (Rho.Matrices.Model_View);
      Target.Push_Matrix;
      Target.Load_Identity;

      --        declare
      --           TX : constant Rho_Float := Item.X;
      --           TY : constant Rho_Float :=
      --                  Target.Height - Item.Y - Item.Height;
      --        begin
      --           Target.Translate (TX, TY, 0.0);
      --        end;

      for X in Item.Tiles'Range (1) loop
         for Y in Item.Tiles'Range (2) loop
            declare
               Tile : Buffer_Tile_Record renames
                        Item.Tiles (X, Y);
            begin
               Target.Push_Matrix;
               Target.Translate
                 (Item.X + Rho_Float (X * Tile_Size),
                  Target.Viewport.Height - Item.Y
                  - Rho_Float ((Y + 1) * Tile_Size),
                  0.0);
               Tile.Object.Render (Target);
               Target.Pop_Matrix;
            end;
         end loop;
      end loop;

      Target.Pop_Matrix;
      Target.Matrix_Mode (Rho.Matrices.Projection);
      Target.Pop_Matrix;
      Target.Set_Depth_Test (Current_Depth_Test);
      --        Target.Pop_Shader;
   end Execute_Render;

   First_Render : Boolean := True;

   -----------------
   -- Finish_Draw --
   -----------------

   procedure Finish_Draw
     (Buffer : in out Rho_Buffer_Record)
   is
   begin
      Cairo.Destroy (Buffer.Context);
      if First_Render then
         declare
            Status : constant Cairo.Cairo_Status :=
                       Cairo.Png.Write_To_Png
                         (Buffer.Surface, Filename => "tiles.png");
            pragma Unreferenced (Status);
         begin
            First_Render := False;
         end;
      end if;

      for X in Buffer.Tiles'Range (1) loop
         for Y in Buffer.Tiles'Range (2) loop
            declare
               Tile : Buffer_Tile_Record renames Buffer.Tiles (X, Y);
            begin
               if Use_Textures and then Tile.Reload then
                  Tile.Texture.Set_Surface
                    (Buffer.Surface, Tile.Region);
                  --  Tile.Reload := False;
               end if;
            end;
         end loop;
      end loop;

      if not Ada.Directories.Exists ("texture.png") then
         declare
            Status : constant Cairo.Cairo_Status :=
                       Cairo.Png.Write_To_Png (Buffer.Surface, "texture.png");
         begin
            Ada.Text_IO.Put_Line (Cairo.Cairo_Status'Image (Status));
         end;
      end if;

   end Finish_Draw;

   -------------------
   -- Get_Rectangle --
   -------------------

   overriding function Get_Rectangle
     (Item     : Rho_Buffer_Record)
      return Rho.Rectangle.Rho_Rectangle
   is
   begin
      return Item.Rectangle;
   end Get_Rectangle;

   ----------------
   -- Invalidate --
   ----------------

   procedure Invalidate
     (Buffer    : in out Rho_Buffer_Record;
      Rectangle : Rho.Rectangle.Rho_Rectangle)
   is
      Left  : constant Natural := Natural (Rectangle.X) / Tile_Size;
      Right : constant Natural :=
                (Natural (Rectangle.X + Rectangle.Width)
                 + Tile_Size - 1)
                / Tile_Size;
      Low   : constant Natural := Natural (Rectangle.Y) / Tile_Size;
      High  : constant Natural :=
                (Natural (Rectangle.Y + Rectangle.Height)
                 + Tile_Size - 1)
                / Tile_Size;
   begin
      for X in Left .. Right loop
         for Y in Low .. High loop
            if X in Buffer.Tiles'Range (1)
              and then Y in Buffer.Tiles'Range (2)
            then
               Buffer.Tiles (X, Y).Reload := True;
            end if;
         end loop;
      end loop;
   end Invalidate;

   -------------------
   -- Set_Rectangle --
   -------------------

   overriding procedure Set_Rectangle
     (Item     : in out Rho_Buffer_Record;
      Rectangle : Rho.Rectangle.Rho_Rectangle)
   is
      Width  : constant Natural := Natural (Rectangle.Width);
      Height : constant Natural := Natural (Rectangle.Height);
      Across : constant Natural := Width / Tile_Size
        + (if Width mod Tile_Size /= 0 then 1 else 0);
      Down   : constant Natural := Height / Tile_Size
        + (if Height mod Tile_Size /= 0 then 1 else 0);
   begin
      Item.Rectangle := Rectangle;
      if Item.Tiles /= null then
         for X in Item.Tiles'Range (1) loop
            for Y in Item.Tiles'Range (2) loop
               if Use_Textures then
                  Item.Tiles (X, Y).Texture.Destroy;
               end if;
               Item.Tiles (X, Y).Object.Destroy;
            end loop;
         end loop;
         Free (Item.Tiles);
         Cairo.Surface_Destroy (Item.Surface);
      end if;

      Item.Tiles := new Buffer_Tile_Array (0 .. Across - 1, 0 .. Down - 1);

      for X in Item.Tiles'Range (1) loop
         for Y in Item.Tiles'Range (2) loop
            declare
               use type Rho.Materials.Material.Rho_Material;
               Tile : Buffer_Tile_Record renames Item.Tiles (X, Y);
            begin
               if Use_Textures then
                  Tile.Texture :=
                    Rho.Texture.Create
                      ("buffer" & Natural'Image (X) & Natural'Image (Y));
               end if;

               if Tile.Material = null then
                  Tile.Material :=
                    Rho.Materials.Material.Rho_New_With_Texture
                      (Tile.Texture.Name, Tile.Texture,
                       Lighting => False);
                  Tile.Material.Technique (1).Pass (1).Set_Shader (Shader);
               else
                  Tile.Material.Technique (1).Pass (1).Set_Texture
                    (Tile.Texture);
               end if;

               Tile.Object :=
                 Create_Tile_Entity
                   (X, Y,
                    Buffer_Height  => Natural (Item.Height),
                    Surface_Height => Down * Tile_Size,
                    Material       => Tile.Material);
               Tile.Reload := True;
               Tile.Region :=
                 (Rho_Float (X * Tile_Size),
                  Rho_Float (Y * Tile_Size),
                  Rho_Float (Tile_Size),
                  Rho_Float (Tile_Size));
            end;
         end loop;
      end loop;

      declare
         use Glib;
         Surface_Width : Gint := Gint (Rectangle.Width);
         Surface_Height : Gint := Gint (Rectangle.Height);
      begin
         if Surface_Width mod Tile_Size /= 0 then
            Surface_Width := Surface_Width
              + Tile_Size - Surface_Width mod Tile_Size;
         end if;
         if Surface_Height mod Tile_Size /= 0 then
            Surface_Height := Surface_Height
              + Tile_Size - Surface_Height mod Tile_Size;
         end if;

         Item.Surface :=
           Cairo.Image_Surface.Create
             (Format => Cairo.Image_Surface.Cairo_Format_ARGB32,
              Width  => Surface_Width,
              Height => Surface_Height);
      end;

   end Set_Rectangle;

   -------------------------
   -- Set_Screen_Position --
   -------------------------

   procedure Set_Screen_Position
     (Buffer : in out Rho_Buffer_Record;
      X, Y   : Rho_Float)
   is
   begin
      Buffer.X := X;
      Buffer.Y := Y;
   end Set_Screen_Position;

   ----------------
   -- Start_Draw --
   ----------------

   function Start_Draw
     (Buffer : in out Rho_Buffer_Record)
      return Cairo.Cairo_Context
   is
   begin
      Buffer.Context := Cairo.Create (Buffer.Surface);
      Cairo.Save (Buffer.Context);
      Cairo.Set_Operator (Buffer.Context, Cairo.Cairo_Operator_Clear);
      Cairo.Paint (Buffer.Context);
      Cairo.Restore (Buffer.Context);
      return Buffer.Context;
   end Start_Draw;

end Rho.Toolkit.Buffer;
