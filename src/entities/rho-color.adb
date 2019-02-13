package body Rho.Color is

   -----------
   -- Scale --
   -----------

   function Scale
     (Colors        : Rho_Color_1D_Array;
      To_Size       : Positive;
      Interpolation : Interpolation_Type := Linear)
      return Rho_Color_1D_Array
   is
      Map        : array (Colors'Range) of Positive;
      Result     : Rho_Color_1D_Array (1 .. To_Size);
      From_Index : Positive := 1;
   begin
      case Interpolation is
         when Source =>
            for I in Map'Range loop
               Map (I) := (I - 1) * To_Size / Colors'Length + 1;
            end loop;
         when Destination =>
            for I in Map'Range loop
               Map (I) := I * To_Size / Colors'Length;
            end loop;
         when Linear =>
            declare
               Src : constant Non_Negative_Float :=
                       Non_Negative_Float (Colors'Length - 1);
               Dst : constant Non_Negative_Float :=
                       Non_Negative_Float (To_Size);
               Factor : constant Non_Negative_Float :=
                          Dst / Src;
            begin
               for I in Map'Range loop
                  Map (I) := Natural (Rho_Float (I - 1) * Factor) + 1;
               end loop;
            end;
      end case;

      for I in Result'Range loop
         if From_Index < Colors'Last
           and then Map (From_Index + 1) = I
         then
            From_Index := From_Index + 1;
         end if;

         case Interpolation is
            when Source | Destination =>
               Result (I) := Colors (From_Index);
            when Linear =>
               if I = 1 then
                  Result (I) := Colors (From_Index);
               elsif I <= Map (From_Index) then
                  Result (I) :=
                    Interpolate
                      (Colors (From_Index - 1),
                       Colors (From_Index),
                       Rho_Float (I - Map (From_Index - 1))
                       / Rho_Float (Map (From_Index) - Map (From_Index - 1)));
               else
                  Result (I) :=
                    Interpolate
                      (Colors (From_Index),
                       Colors (From_Index + 1),
                       Rho_Float (I - Map (From_Index))
                       / Rho_Float (Map (From_Index + 1) - Map (From_Index)));
               end if;
         end case;
      end loop;

      return Result;
   end Scale;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color (Item  : in out Rho_Has_Color'Class;
                        Red   : Unit_Float;
                        Green : Unit_Float;
                        Blue  : Unit_Float;
                        Alpha : Unit_Float := 1.0)
   is
   begin
      Item.Set_Color ((Red, Green, Blue, Alpha));
   end Set_Color;

   -----------
   -- Shade --
   -----------

   function Shade
     (Color : Rho_Color;
      Factor : Rho_Float)
      return Rho_Color
   is
   begin
      return (Red   => Rho_Float'Min (Color.Red * Factor, 1.0),
              Green => Rho_Float'Min (Color.Green * Factor, 1.0),
              Blue  => Rho_Float'Min (Color.Green * Factor, 1.0),
              Alpha => Color.Alpha);
   end Shade;

end Rho.Color;
