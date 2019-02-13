private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

with Cairo;
with Pango.Font;
with Pango.Layout;

with Rho.Object;
with Rho.Rectangle;
with Rho.Texture;

package Rho.Font is

   Rho_Font_Class_Name : constant String := "font";

   type Rho_Font_Size is new Rho_Float range 0.0 .. 999.0;

   type Rho_Font_Slant is (Normal, Italic, Oblique);
   type Rho_Font_Weight is (Normal, Bold);

   function Pango_Cairo_Create_Layout
      (Context : Cairo.Cairo_Context)
       return Pango.Layout.Pango_Layout;

   function Get_Font
     (Specification : String)
      return Pango.Font.Pango_Font_Description;

   type Rho_Font_Record is new Rho.Object.Rho_Resource_Record with private;

   overriding function Class_Name
     (Font : Rho_Font_Record)
      return String
   is (Rho_Font_Class_Name);

   function Texture (Font : Rho_Font_Record'Class;
                     Ch   : Wide_Wide_Character)
                     return Rho.Texture.Rho_Texture;

   function Glyph (Font : Rho_Font_Record'Class;
                   Ch   : Wide_Wide_Character)
                   return Rho.Rectangle.Rho_Rectangle;

   procedure Glyph_Size (Font : Rho_Font_Record;
                         Ch   : Wide_Wide_Character;
                         Width, Height : out Rho_Float);

   procedure Cairo_Font
     (Font    : Rho_Font_Record;
      Context : Cairo.Cairo_Context);

   type Rho_Font is access all Rho_Font_Record'Class;

   function Get_Font
     (Font_Name : String;
      Font_Size : Rho_Font_Size;
      Slant     : Rho_Font_Slant  := Normal;
      Weight    : Rho_Font_Weight := Normal)
      return Rho_Font;

   procedure Measure_String
     (Font          : Rho_Font;
      Text          : String;
      Width, Height : out Rho_Float);

private

   type Glyph_Record is
      record
         Pixel_Location   : Rho.Rectangle.Rho_Rectangle;
         Texture_Location : Rho.Rectangle.Rho_Rectangle;
      end record;

   type Array_Of_Glyphs is
     array (Wide_Wide_Character range <>) of Glyph_Record;

   type Array_Of_Glyphs_Access is access Array_Of_Glyphs;

   type Code_Point_Range_Record is
      record
         Texture : Rho.Texture.Rho_Texture;
         Glyphs  : Array_Of_Glyphs_Access;
      end record;

   package Code_Point_Range_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Code_Point_Range_Record);

   type Code_Point_Range_Access is access Code_Point_Range_Lists.List;

   type Rho_Font_Record is new Rho.Object.Rho_Resource_Record with
      record
         Font_Name      : Ada.Strings.Unbounded.Unbounded_String;
         Font_Size      : Rho_Font_Size;
         Font_Slant     : Rho_Font_Slant;
         Font_Weight    : Rho_Font_Weight;
         Glyphs         : Code_Point_Range_Access;
      end record;

   function Get_Character_Block
     (Font : Rho_Font_Record'Class;
      Ch   : Wide_Wide_Character)
      return Code_Point_Range_Record;

   function Get_Glyph_Record
     (Font : Rho_Font_Record'Class;
      Ch   : Wide_Wide_Character)
      return Glyph_Record;

   procedure Create_Code_Point_Range
     (Font  : Rho_Font_Record'Class;
      Base  : Wide_Wide_Character;
      Bound : Wide_Wide_Character);

end Rho.Font;
