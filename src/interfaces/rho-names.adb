package body Rho.Names is

   protected Name_Source is
      procedure Next_Index (Index : out Positive);
   private
      Name_Index : Natural := 0;
   end Name_Source;

   -----------------
   -- Name_Source --
   -----------------

   protected body Name_Source is

      ----------------
      -- Next_Index --
      ----------------

      procedure Next_Index (Index : out Positive) is
      begin
         Name_Index := Name_Index + 1;
         Index := Name_Index;
      end Next_Index;

   end Name_Source;

   --------------
   -- New_Name --
   --------------

   function New_Name (Base_Name : String) return String is
      Index : Positive;
   begin
      Name_Source.Next_Index (Index);
      return Base_Name & Integer'Image (-Index);
   end New_Name;

end Rho.Names;
