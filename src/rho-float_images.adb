with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body Rho.Float_Images is

   Default_Significant_Digits : constant := 4;

   function Significant_Digits_Image
     (Value : Non_Negative_Float;
      Sig   : Positive)
      return String;

   -----------
   -- Image --
   -----------

   function Image (Value : Rho_Float) return String is
   begin
      if Value < 0.0 then
         return "-"
           & Significant_Digits_Image (-Value, Default_Significant_Digits);
      else
         return Significant_Digits_Image (Value, Default_Significant_Digits);
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Value : Rho.Float_Arrays.Real_Vector)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for V of Value loop
         if Result = Null_Unbounded_String then
            Result := Result & "(";
         else
            Result := Result & ", ";
         end if;
         Result := Result & Image (V);
      end loop;
      return To_String (Result) & ")";
   end Image;

   ------------------------------
   -- Significant_Digits_Image --
   ------------------------------

   function Significant_Digits_Image
     (Value : Non_Negative_Float;
      Sig   : Positive)
      return String
   is
      Result    : String (1 .. Sig);
      Point     : Natural := 0;
      Acc       : Non_Negative_Float := Value;
      Boundary  : constant Non_Negative_Float := 10.0 ** Sig;
   begin
      if Value < 1.0 / Boundary then
         return "0.0";
      end if;

      if Value >= Boundary then
         if Value >= 1.0e6 then
            return Ada.Strings.Fixed.Trim
              (Non_Negative_Float'Image (Value),
               Ada.Strings.Left);
         else
            return Ada.Strings.Fixed.Trim
              (Integer'Image (Integer (Value)),
               Ada.Strings.Left);
         end if;
      else
         while abs Acc * 10.0 < Boundary loop
            Acc := Acc * 10.0;
            Point := Point + 1;
         end loop;

         Result :=
           Ada.Strings.Fixed.Trim
             (Integer'Image (Integer (Acc - 0.5)),
              Ada.Strings.Left);

         if Point < Sig then
            if Point = 0 then
               return Result;
            else
               return Result (1 .. Result'Last - Point) & "." &
                 Result (Result'Last - Point + 1 .. Result'Last);
            end if;
         else
            declare
               Zeroes : constant String (1 .. Point - Sig) :=
                          (others => '0');
            begin
               return "0." & Zeroes & Result;
            end;
         end if;
      end if;
   end Significant_Digits_Image;

end Rho.Float_Images;
