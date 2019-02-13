with WL.Bitmap_IO;

package body Rho.Color.Export is

   ------------------
   -- Write_Bitmap --
   ------------------

   procedure Write_Bitmap
     (Data : Rho_Color_2D_Array;
      Path : String)
   is
      use WL.Bitmap_IO;
      BM : Bitmap_Type := New_Bitmap (Data'Length (1), Data'Length (2));
   begin
      for Y in Data'Range (2) loop
         for X in Data'Range (1) loop
            declare
               From_Color : constant Rho_Color :=
                              Data (X, Y);
               To_Color   : constant Color_Type :=
                              (R => Color_Element (255.0 * From_Color.Red),
                               G => Color_Element (255.0 * From_Color.Green),
                               B => Color_Element (255.0 * From_Color.Blue),
                               Alpha =>
                                 Color_Element (255.0 * From_Color.Alpha));
            begin
               Set_Color (BM, X - Data'First (1), Y - Data'First (2),
                           To_Color);
            end;
         end loop;
      end loop;
      Write (BM, Path);
      Close (BM);

   end Write_Bitmap;

end Rho.Color.Export;
