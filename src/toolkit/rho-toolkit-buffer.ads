private with Rho.Entity;
private with Rho.Materials.Material;
private with Rho.Texture;

with Cairo;

with Rho.Rectangle;
with Rho.Render_Target;
with Rho.Renderable;

package Rho.Toolkit.Buffer is

   type Rho_Buffer_Record is
     new Rho.Rectangle.Rho_Rectangle_Interface
     and Rho.Renderable.Rho_Renderable
       with private;

   overriding procedure Execute_Render
     (Item   : in out Rho_Buffer_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   overriding procedure Set_Rectangle
     (Item     : in out Rho_Buffer_Record;
      Rectangle : Rho.Rectangle.Rho_Rectangle);

   overriding function Get_Rectangle
     (Item     : Rho_Buffer_Record)
      return Rho.Rectangle.Rho_Rectangle;

   procedure Invalidate
     (Buffer    : in out Rho_Buffer_Record;
      Rectangle : Rho.Rectangle.Rho_Rectangle);

   function Start_Draw
     (Buffer : in out Rho_Buffer_Record)
      return Cairo.Cairo_Context;

   procedure Finish_Draw
     (Buffer : in out Rho_Buffer_Record);

   procedure Set_Screen_Position
     (Buffer : in out Rho_Buffer_Record;
      X, Y   : Rho_Float);

   type Rho_Buffer is access all Rho_Buffer_Record'Class;

   function Create
     (Geometry : Rho.Rectangle.Rho_Rectangle)
      return Rho_Buffer;

   procedure Destroy (Buffer : in out Rho_Buffer);

private

   type Buffer_Tile_Record is
      record
         Region   : Rho.Rectangle.Rho_Rectangle;
         Material : Rho.Materials.Material.Rho_Material;
         Texture  : Rho.Texture.Rho_Texture;
         Object   : Rho.Entity.Rho_Entity;
         Reload   : Boolean;
      end record;

   type Buffer_Tile_Array is
     array (Natural range <>, Natural range <>) of Buffer_Tile_Record;

   type Buffer_Tile_Array_Access is
     access Buffer_Tile_Array;

   type Rho_Buffer_Record is
     new Rho.Rectangle.Rho_Rectangle_Interface
     and Rho.Renderable.Rho_Renderable with
      record
         Rectangle : Rho.Rectangle.Rho_Rectangle;
         Tiles     : Buffer_Tile_Array_Access;
         Surface   : Cairo.Cairo_Surface;
         Context   : Cairo.Cairo_Context;
         X, Y      : Rho_Float;
      end record;

   overriding function Loaded
     (Buffer : Rho_Buffer_Record)
      return Boolean
   is (True);

end Rho.Toolkit.Buffer;
