with GL;

with Rho.Matrices;

package body Rho.Text is

   -----------------
   -- Create_Text --
   -----------------

   function Create_Text
     (Name : String;
      Text : String;
      Font : Rho.Fonts.Rho_Font)
      return Rho_Text
   is
      Result : constant Rho_Text := new Rho_Text_Record;
   begin
      Result.Set_Name (Name);
      Result.Text := Ada.Strings.Unbounded.To_Unbounded_String (Text);
      Result.Font := Font;
      return Result;
   end Create_Text;

   ------------------
   -- Get_Position --
   ------------------

   overriding
   procedure Get_Position (Text     : in     Rho_Text_Record;
                           X, Y, Z  :    out Float)
   is
   begin
      X := Text.X;
      Y := Text.Y;
      Z := Text.Z;
   end Get_Position;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Item : in Rho_Text_Record)
   is
   begin

      Rho.Matrices.Push_Matrix;

      GL.RasterPos3f (GL.Float (Item.X),
                      GL.Float (Item.Y),
                      GL.Float (Item.Z));
      declare
         Text : constant String :=
                  Ada.Strings.Unbounded.To_String (Item.Text);
      begin
         for I in Text'Range loop
            Item.Font.Glyph (Text (I)).Render;
         end loop;
      end;

      Rho.Matrices.Pop_Matrix;

   end Render;

   ------------------
   -- Set_Position --
   ------------------

   overriding
   procedure Set_Position (Text     : in out Rho_Text_Record;
                           X, Y, Z  : in     Float)
   is
   begin
      Text.X := X;
      Text.Y := Y;
      Text.Z := Z;
   end Set_Position;

end Rho.Text;
