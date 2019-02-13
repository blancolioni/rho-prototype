with Ada.Characters.Handling;
with Ada.Text_IO;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Strings.Fixed.Hash_Case_Insensitive;

with Rho.Paths;

package body Rho.Color.Parser is

   package Named_Color_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Rho_Color,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   Named_Colors : Named_Color_Maps.Map;
   Got_Colors   : Boolean := False;

   procedure Read_Colors;

   function Hex_To_Unit_Float
     (Hex_Digits : String)
      return Unit_Float;

   -----------------------
   -- Hex_To_Unit_Float --
   -----------------------

   function Hex_To_Unit_Float
     (Hex_Digits : String)
      return Unit_Float
   is
      function Hex_Digit_Value (Ch : Character) return Natural;

      ---------------------
      -- Hex_Digit_Value --
      ---------------------

      function Hex_Digit_Value (Ch : Character) return Natural is
      begin
         case Ch is
            when '0' .. '9' =>
               return Character'Pos (Ch) - Character'Pos ('0');
            when 'a' .. 'f' =>
               return Character'Pos (Ch) - Character'Pos ('a') + 10;
            when 'A' .. 'F' =>
               return Character'Pos (Ch) - Character'Pos ('A') + 10;
            when others =>
               raise Constraint_Error;
         end case;
      end Hex_Digit_Value;

      Max   : Positive := 1;
      Value : Natural := 0;
   begin
      for I in Hex_Digits'Range loop
         Value := Value * 16 + Hex_Digit_Value (Hex_Digits (I));
         Max   := Max * 16;
      end loop;

      return Rho_Float (Value) / Rho_Float (Max - 1);

   end Hex_To_Unit_Float;

   ----------------------
   -- Parse_Html_Color --
   ----------------------

   function Parse_Html_Color
     (Text : String)
      return Rho.Color.Rho_Color
   is
   begin
      if Text (Text'First) = '#' then
         if Text'Length = 4 then
            return Color : Rho.Color.Rho_Color do
               Color.Red :=
                 Hex_To_Unit_Float
                   (Text (Text'First + 1 .. Text'First + 1));
               Color.Green :=
                 Hex_To_Unit_Float
                   (Text (Text'First + 2 .. Text'First + 2));
               Color.Blue :=
                 Hex_To_Unit_Float
                   (Text (Text'First + 3 .. Text'First + 3));
               Color.Alpha := 1.0;
            end return;
         else
            return Color : Rho.Color.Rho_Color do
               Color.Red :=
                 Hex_To_Unit_Float
                   (Text (Text'First + 1 .. Text'First + 2));
               Color.Green :=
                 Hex_To_Unit_Float
                   (Text (Text'First + 3 .. Text'First + 4));
               Color.Blue :=
                 Hex_To_Unit_Float
                   (Text (Text'First + 5 .. Text'First + 6));
               Color.Alpha :=
                 (if Text'Length < 9
                  then 1.0
                  else Hex_To_Unit_Float
                    (Text (Text'First + 7 .. Text'First + 8)));
            end return;
         end if;
      else
         if not Got_Colors then
            Read_Colors;
         end if;
         declare
            use Named_Color_Maps;
            Position : constant Cursor := Named_Colors.Find (Text);
         begin
            if Has_Element (Position) then
               return Element (Position);
            else
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Maas: unknown color: " & Text);
               return (1.0, 0.0, 1.0, 1.0);
            end if;
         end;
      end if;

   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "cannot parse color '" & Text & "'";
   end Parse_Html_Color;

   -----------------
   -- Read_Colors --
   -----------------

   procedure Read_Colors is
      use Ada.Text_IO;
      File : File_Type;

      function Is_Space (Ch : Character) return Boolean
      is (Ch = ' ' or else Character'Pos (Ch) in 9 | 10 | 13);

   begin

      Named_Colors.Insert ("transparent", (1.0, 1.0, 1.0, 0.0));

      Open (File, In_File, Rho.Paths.Config_Path & "/rgb.txt");
      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
         begin
            if Line (Line'First) = '!' then
               null;
            else
               declare
                  use Ada.Characters.Handling;
                  R, G, B : Integer := -1;
                  X       : Natural;
                  Start   : Positive := Line'First;
                  At_Number : Boolean := False;
               begin
                  for I in Line'Range loop
                     if not Is_Space (Line (I))
                       and then B >= 0
                     then
                        Named_Colors.Insert (Line (I .. Line'Last),
                                             (Rho_Float (R) / 255.0,
                                              Rho_Float (G) / 255.0,
                                              Rho_Float (B) / 255.0,
                                              1.0));
                        exit;
                     end if;

                     if Is_Digit (Line (I)) then
                        if not At_Number then
                           At_Number := True;
                           Start := I;
                        end if;
                     elsif Is_Space (Line (I)) then
                        if At_Number then
                           X := Natural'Value (Line (Start .. I - 1));
                           if R < 0 then
                              R := X;
                           elsif G < 0 then
                              G := X;
                           elsif B < 0 then
                              B := X;
                           end if;
                           At_Number := False;
                        end if;
                     end if;
                  end loop;
               end;
            end if;
         end;
      end loop;
      Close (File);
      Got_Colors := True;

   end Read_Colors;

end Rho.Color.Parser;
