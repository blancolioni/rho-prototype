with Rho.Render_Operation;
with Rho.Rectangle;
--  with Rho.Rectangle;
--  with Rho.Render_Operation;

package body Rho.Label is

   ------------------
   -- Create_Label --
   ------------------

   function Create_Label
     (Name      : String;
      Text      : String;
      Font      : Rho.Font.Rho_Font)
      return Rho_Label
   is
      Result : constant Rho_Label := new Rho_Label_Record;
      X, Y   : Rho_Float := 0.0;
      First  : Boolean := True;
   begin
      Result.Set_Name (Name);
      Result.Text := Ada.Strings.Unbounded.To_Unbounded_String (Text);
      Result.Font := Font;
      Result.Set_Texture (Font.Texture ('a'));

      Result.Begin_Operation (Rho.Render_Operation.Triangle_List);

      for Ch of Text loop
         declare
            W_Ch : constant Wide_Wide_Character :=
                     Wide_Wide_Character'Val (Character'Pos (Ch));
            Glyph_Rect : constant Rho.Rectangle.Rho_Rectangle :=
                           Result.Font.Glyph (W_Ch);
            W, H       : Rho_Float;
         begin
            Result.Font.Glyph_Size (W_Ch, W, H);

            if First then
               Y := -H / 2.0;
               First := False;
            end if;

            Result.Normal (0.0, 0.0, 1.0);
            Result.Texture_Coordinate (Glyph_Rect.X,
                                       Glyph_Rect.Y + Glyph_Rect.Height);
            Result.Vertex (X, Y + H, 0.0);

            Result.Normal (0.0, 0.0, 1.0);
            Result.Texture_Coordinate (Glyph_Rect.X,
                                       Glyph_Rect.Y);
            Result.Vertex (X, Y, 0.0);

            Result.Normal (0.0, 0.0, 1.0);
            Result.Texture_Coordinate (Glyph_Rect.X + Glyph_Rect.Width,
                                       Glyph_Rect.Y + Glyph_Rect.Height);
            Result.Vertex (X + W, Y + H, 0.0);

            Result.Normal (0.0, 0.0, 1.0);
            Result.Texture_Coordinate (Glyph_Rect.X,
                                       Glyph_Rect.Y);
            Result.Vertex (X, Y, 0.0);

            Result.Normal (0.0, 0.0, 1.0);
            Result.Texture_Coordinate (Glyph_Rect.X + Glyph_Rect.Width,
                                       Glyph_Rect.Y);
            Result.Vertex (X + W, Y, 0.0);

            Result.Normal (0.0, 0.0, 1.0);
            Result.Texture_Coordinate (Glyph_Rect.X + Glyph_Rect.Width,
                                       Glyph_Rect.Y + Glyph_Rect.Height);
            Result.Vertex (X + W, Y + H, 0.0);

            X := X + W;
         end;
      end loop;

      Result.End_Operation;

      return Result;
   end Create_Label;

end Rho.Label;
