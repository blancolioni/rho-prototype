with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash_Case_Insensitive;
with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with Ada.Characters.Handling;

with System;
with Glib.Object;
with Cairo.Image_Surface;
with Cairo.Png;

package body Rho.Font is

   Code_Point_Length : constant := 256;

   package Font_Description_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Pango.Font.Pango_Font_Description,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive,
        "="             => Pango.Font."=");

   package Texture_Font_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Rho.Font.Rho_Font,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   Cached_Pango_Fonts : Font_Description_Maps.Map;
   Texture_Fonts      : Texture_Font_Maps.Map;

   function Get_Font_Specification
     (Font_Name : String;
      Font_Size : Rho_Font_Size;
      Slant     : Rho_Font_Slant;
      Weight    : Rho_Font_Weight)
      return String;

   function Create_Font
     (Font_Name : String;
      Font_Size : Rho_Font_Size;
      Slant     : Rho_Font_Slant;
      Weight    : Rho_Font_Weight)
      return Rho_Font;

   function To_UTF_8 (Ch : Wide_Wide_Character) return String
   is (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode
       (Item          => (1 => Ch),
        Output_Scheme => Ada.Strings.UTF_Encoding.UTF_8,
        Output_BOM    => False));

   ----------------
   -- Cairo_Font --
   ----------------

   procedure Cairo_Font
     (Font    : Rho_Font_Record;
      Context : Cairo.Cairo_Context)
   is
      use Ada.Strings.Unbounded;
   begin
      Cairo.Select_Font_Face (Context, To_String (Font.Font_Name),
                              Slant => (case Font.Font_Slant is
                                        when Normal =>
                                        Cairo.Cairo_Font_Slant_Normal,
                                        when Italic =>
                                        Cairo.Cairo_Font_Slant_Italic,
                                        when Oblique =>
                                        Cairo.Cairo_Font_Slant_Oblique),
                              Weight => (case Font.Font_Weight is
                                         when Normal =>
                                         Cairo.Cairo_Font_Weight_Normal,
                                         when Bold   =>
                                         Cairo.Cairo_Font_Weight_Bold));
      Cairo.Set_Font_Size (Context, Size => Glib.Gdouble (Font.Font_Size));
   end Cairo_Font;

   -----------------------------
   -- Create_Code_Point_Range --
   -----------------------------

   procedure Create_Code_Point_Range
     (Font  : Rho_Font_Record'Class;
      Base  : Wide_Wide_Character;
      Bound : Wide_Wide_Character)
   is
      function Sqrt (X : Natural) return Natural;
      function Minimum_Power_Of_2 (X : Positive) return Positive;

      ------------------------
      -- Minimum_Power_Of_2 --
      ------------------------

      function Minimum_Power_Of_2 (X : Positive) return Positive is
         Result : Positive := 1;
      begin
         while Result < X loop
            Result := Result * 2;
         end loop;
         return Result;
      end Minimum_Power_Of_2;

      ----------
      -- Sqrt --
      ----------

      function Sqrt (X : Natural) return Natural is
         Result : Natural :=
                    (if X = 0 then 0
                     elsif X = 1 then 1
                     elsif X < 9 then 2
                     else 3);
         Acc    : Positive := Result * Result;
      begin
         while Acc <= X loop
            Acc := Acc + 2 * Result + 1;
            Result := Result + 1;
         end loop;
         Result := Result - 1;
         return Result;
      end Sqrt;

      Max_Character_Width  : constant Positive :=
                               Minimum_Power_Of_2
                                 (Positive (Font.Font_Size + 0.5));
      Max_Character_Height : constant Positive :=
                               Minimum_Power_Of_2
                                 (Positive (Font.Font_Size * 1.5 + 0.5));
      Number_Of_Code_Points : constant Positive :=
                                Wide_Wide_Character'Pos (Bound)
                                - Wide_Wide_Character'Pos (Base);
      Characters_Across     : constant Positive :=
                                Minimum_Power_Of_2
                                  (Sqrt (Number_Of_Code_Points));
      Characters_Down       : constant Positive :=
                                Minimum_Power_Of_2
                                  (Number_Of_Code_Points / Characters_Across);
      Texture_Width        : constant Positive :=
                               Max_Character_Width * Characters_Across;
      Texture_Height       : constant Positive :=
                               Max_Character_Height * Characters_Down;
      Width_Factor         : constant Rho_Float :=
                               1.0 / Rho_Float (Texture_Width);
      Height_Factor        : constant Rho_Float :=
                               1.0 / Rho_Float (Texture_Height);

      Font_Name : constant String :=
                    Ada.Strings.Unbounded.To_String (Font.Font_Name);

      Surface : constant Cairo.Cairo_Surface :=
                  Cairo.Image_Surface.Create
                    (Format => Cairo.Image_Surface.Cairo_Format_ARGB32,
                     Width  => Glib.Gint (Texture_Width),
                     Height => Glib.Gint (Texture_Height));

      Context : constant Cairo.Cairo_Context := Cairo.Create (Surface);

      Glyphs  : constant Array_Of_Glyphs_Access :=
                  new Array_Of_Glyphs
                    (Base .. Wide_Wide_Character'Pred (Bound));
   begin

      Cairo.Set_Source_Rgba (Context, 0.0, 0.0, 0.0, 0.0);
      Cairo.Rectangle (Context, 0.0, 0.0,
                       Glib.Gdouble (Texture_Width),
                       Glib.Gdouble (Texture_Height));
      Cairo.Fill (Context);

      Cairo.Select_Font_Face (Context, Font_Name,
                              Slant  => (case Font.Font_Slant is
                                            when Normal  =>
                                               Cairo.Cairo_Font_Slant_Normal,
                                            when Italic  =>
                                               Cairo.Cairo_Font_Slant_Italic,
                                            when Oblique =>
                                               Cairo.Cairo_Font_Slant_Oblique),
                              Weight => (case Font.Font_Weight is
                                            when Normal =>
                                               Cairo.Cairo_Font_Weight_Normal,
                                            when Bold   =>
                                               Cairo.Cairo_Font_Weight_Bold));
      Cairo.Set_Font_Size (Context, Size => Glib.Gdouble (Font.Font_Size));
      Cairo.Set_Source_Rgb (Context, 1.0, 1.0, 1.0);

      for Ch in Glyphs'Range loop
         declare
            Char_Index : constant Natural :=
                           Wide_Wide_Character'Pos (Ch)
                           - Wide_Wide_Character'Pos (Base);
            Index_X    : constant Natural := Char_Index mod Characters_Across;
            Index_Y    : constant Natural :=
                           Char_Index / Characters_Across + 1;
            X          : constant Rho_Float :=
                           Rho_Float (Index_X * Max_Character_Width);
            Y          : constant Rho_Float :=
                           Rho_Float (Index_Y * Max_Character_Height);

         begin
            Cairo.Move_To (Context,
                           Glib.Gdouble (X),
                           Glib.Gdouble (Y));
            Cairo.Text_Path (Context, To_UTF_8 (Ch));
            declare
               New_X, New_Y : aliased Glib.Gdouble;
            begin
               Cairo.Get_Current_Point (Context, New_X'Access, New_Y'Access);
               Glyphs (Ch) := Glyph_Record'
                 (Pixel_Location   =>
                    (X      => X,
                     Y      =>
                       Rho_Float (Texture_Height) - Y,
                     Width  => Rho_Float (New_X) - X,
                     Height => Rho_Float (Max_Character_Height)),
                  Texture_Location =>
                       (X      => X * Width_Factor,
                        Y      => 1.0 - Y * Height_Factor,
                        Width  => (Rho_Float (New_X) - X) * Width_Factor,
                        Height =>
                          Rho_Float (Max_Character_Height) * Height_Factor));
            end;
            Cairo.Fill (Context);
         end;
      end loop;

      Cairo.Destroy (Context);

      declare
         Result : Cairo.Cairo_Status :=
                    Cairo.Png.Write_To_Png
                      (Surface,
                       Font_Name
                       & Integer'Image
                         (-Wide_Wide_Character'Pos (Base) / 256 + 1)
                       & ".png");
         pragma Unreferenced (Result);
      begin
         null;
      end;

      declare
         Texture : constant Rho.Texture.Rho_Texture :=
                     Rho.Texture.Create_From_Surface (Surface);
      begin
         Font.Glyphs.Append ((Texture, Glyphs));
      end;

   end Create_Code_Point_Range;

   -----------------
   -- Create_Font --
   -----------------

   function Create_Font
     (Font_Name : String;
      Font_Size : Rho_Font_Size;
      Slant     : Rho_Font_Slant;
      Weight    : Rho_Font_Weight)
      return Rho_Font
   is
      use Ada.Strings.Unbounded;

      Font : constant Rho_Font :=
               new Rho_Font_Record'
                 (Rho.Object.Rho_Resource_Record with
                  Font_Name   => To_Unbounded_String (Font_Name),
                  Font_Size   => Font_Size,
                  Font_Slant  => Slant,
                  Font_Weight => Weight,
                  Glyphs      => new Code_Point_Range_Lists.List);
   begin
      return Font;
   end Create_Font;

   -------------------------
   -- Get_Character_Block --
   -------------------------

   function Get_Character_Block
     (Font : Rho_Font_Record'Class;
      Ch   : Wide_Wide_Character)
      return Code_Point_Range_Record
   is
   begin
      for Rec of Font.Glyphs.all loop
         if Ch in Rec.Glyphs'Range then
            return Rec;
         end if;
      end loop;

      declare
         Base : constant Wide_Wide_Character :=
                  Wide_Wide_Character'Val
                    (Wide_Wide_Character'Pos (Ch)
                     - Wide_Wide_Character'Pos (Ch) mod Code_Point_Length);
         Bound : constant Wide_Wide_Character :=
                   Wide_Wide_Character'Val
                     (Wide_Wide_Character'Pos (Base) + Code_Point_Length);
      begin
         Font.Create_Code_Point_Range (Base, Bound);
         return Font.Glyphs.Last_Element;
      end;
   end Get_Character_Block;

   --------------
   -- Get_Font --
   --------------

   function Get_Font
     (Font_Name : String;
      Font_Size : Rho_Font_Size;
      Slant     : Rho_Font_Slant  := Normal;
      Weight    : Rho_Font_Weight := Normal)
      return Rho_Font
   is
      Specification : constant String :=
                        Get_Font_Specification
                          (Font_Name, Font_Size, Slant, Weight);
      Position      : constant Texture_Font_Maps.Cursor :=
                        Texture_Fonts.Find (Specification);
   begin
      if Texture_Font_Maps.Has_Element (Position) then
         return Texture_Font_Maps.Element (Position);
      else
         declare
            Font : constant Rho_Font :=
                     Create_Font
                       (Font_Name, Font_Size, Slant, Weight);
         begin
            Font.Set_Name (Specification);
            Texture_Fonts.Insert (Specification, Font);
            return Font;
         end;
      end if;
   end Get_Font;

   --------------
   -- Get_Font --
   --------------

   function Get_Font
     (Specification : String)
      return Pango.Font.Pango_Font_Description
   is
      use Font_Description_Maps;
      Position : constant Cursor := Cached_Pango_Fonts.Find (Specification);
      Result   : Pango.Font.Pango_Font_Description;
   begin
      if Has_Element (Position) then
         Result := Element (Position);
      else
         Result := Pango.Font.From_String (Specification);
         Cached_Pango_Fonts.Insert (Specification, Result);
      end if;
      return Result;
   end Get_Font;

   ----------------------------
   -- Get_Font_Specification --
   ----------------------------

   function Get_Font_Specification
     (Font_Name : String;
      Font_Size : Rho_Font_Size;
      Slant     : Rho_Font_Slant;
      Weight    : Rho_Font_Weight)
      return String
   is
      Size_Image : constant String :=
                     Ada.Strings.Fixed.Trim
                       (Natural'Image (Natural (Font_Size * 10.0)),
                        Ada.Strings.Left);
      Slant_Image : constant String :=
                      Ada.Characters.Handling.To_Lower
                        (Rho_Font_Slant'Image (Slant));
      Weight_Image : constant String :=
                       Ada.Characters.Handling.To_Lower
                         (Rho_Font_Weight'Image (Weight));
      Specification : constant String :=
                        Font_Name
                        & "-" & Size_Image
                        & "-" & Slant_Image
                        & "-" & Weight_Image;
   begin
      return Specification;
   end Get_Font_Specification;

   ----------------------
   -- Get_Glyph_Record --
   ----------------------

   function Get_Glyph_Record
     (Font : Rho_Font_Record'Class;
      Ch   : Wide_Wide_Character)
      return Glyph_Record
   is
      Arr : constant Array_Of_Glyphs_Access :=
              Font.Get_Character_Block (Ch).Glyphs;
   begin
      return Arr (Ch);
   end Get_Glyph_Record;

   -----------
   -- Glyph --
   -----------

   function Glyph (Font : Rho_Font_Record'Class;
                   Ch   : Wide_Wide_Character)
                   return Rho.Rectangle.Rho_Rectangle
   is
   begin
      return Font.Get_Glyph_Record (Ch).Texture_Location;
   end Glyph;

   ----------------
   -- Glyph_Size --
   ----------------

   procedure Glyph_Size
     (Font          : Rho_Font_Record;
      Ch            : Wide_Wide_Character;
      Width, Height : out Rho_Float)
   is
      Rec : Glyph_Record renames Font.Get_Glyph_Record (Ch);
   begin
      Width := Rec.Pixel_Location.Width;
      Height := Rec.Pixel_Location.Height;
   end Glyph_Size;

   --------------------
   -- Measure_String --
   --------------------

   procedure Measure_String
     (Font          : Rho_Font;
      Text          : String;
      Width, Height : out Rho_Float)
   is
      Wide : constant Wide_Wide_String :=
               Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode
                 (Item         => Text,
                  Input_Scheme => Ada.Strings.UTF_Encoding.UTF_8);
   begin
      Width := 0.0;
      Height := 0.0;
      for Ch of Wide loop
         declare
            W, H : Rho_Float;
         begin
            Font.Glyph_Size (Ch, W, H);
            Width := Width + W + 1.0;
            Height := Rho_Float'Max (Height, H);
         end;
      end loop;
   end Measure_String;

   -------------------------------
   -- Pango_Cairo_Create_Layout --
   -------------------------------

   function Pango_Cairo_Create_Layout
      (Context : Cairo.Cairo_Context)
       return Pango.Layout.Pango_Layout
   is
      use Glib.Object;
      use Pango.Layout;
      function Internal (Context : Cairo.Cairo_Context) return System.Address;
      pragma Import (C, Internal, "pango_cairo_create_layout");
      Stub_Pango_Layout : Pango_Layout_Record;
   begin
      return Pango.Layout.Pango_Layout
        (Get_User_Data (Internal (Context),
         Stub_Pango_Layout));
   end Pango_Cairo_Create_Layout;

   -------------
   -- Texture --
   -------------

   function Texture (Font : Rho_Font_Record'Class;
                     Ch   : Wide_Wide_Character)
                     return Rho.Texture.Rho_Texture
   is
   begin
      return Font.Get_Character_Block (Ch).Texture;
   end Texture;

end Rho.Font;
