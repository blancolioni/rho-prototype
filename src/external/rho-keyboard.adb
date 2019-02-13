package body Rho.Keyboard is

   Max_Simultaneous_Keys : constant := 8;

   type Key_Array is array (Rho_Key_Code) of Boolean;

   Keys_Pressed : Key_Array := (others => False);
   Keys_Changed : Boolean := False;

   Current_Keys_Pressed : Array_Of_Key_Codes (1 .. Max_Simultaneous_Keys) :=
                            (others => No_Key);

   -------------
   -- Changed --
   -------------

   function Changed return Boolean is
   begin
      return Keys_Changed;
   end Changed;

   -------------------
   -- Character_Key --
   -------------------

   function Character_Key (Ch : Character) return Rho_Key_Code is
   begin
      return Character'Pos (Ch);
   end Character_Key;

   -------------------
   -- Clear_Changed --
   -------------------

   procedure Clear_Changed is
   begin
      Keys_Changed := False;
   end Clear_Changed;

   --------------------
   -- First_Key_Down --
   --------------------

   function First_Key_Down return Rho_Key_Code is
   begin
      for K of Current_Keys_Pressed loop
         if K /= No_Key then
            return K;
         end if;
      end loop;
      return No_Key;
   end First_Key_Down;

   --------------
   -- Key_Down --
   --------------

   function Key_Down (Code : Rho_Key_Code) return Boolean is
   begin
      return Keys_Pressed (Code);
   end Key_Down;

   --------------
   -- Key_Down --
   --------------

   procedure Key_Down (Code : Rho_Key_Code) is
   begin
      if not Keys_Pressed (Code) then
         Keys_Changed := True;
         Keys_Pressed (Code) := True;
         for K of Current_Keys_Pressed loop
            if K = No_Key then
               K := Code;
               exit;
            end if;
         end loop;
      end if;
   end Key_Down;

   --------------
   -- Key_Down --
   --------------

   procedure Key_Down (Ch : Character) is
   begin
      Key_Down (Character_Key (Ch));
   end Key_Down;

   ------------
   -- Key_Up --
   ------------

   procedure Key_Up (Code : Rho_Key_Code) is
   begin
      if Keys_Pressed (Code) then
         Keys_Changed := True;
         Keys_Pressed (Code) := False;
         for K of Current_Keys_Pressed loop
            if K = Code then
               K := No_Key;
            end if;
         end loop;
      end if;
   end Key_Up;

   ------------
   -- Key_Up --
   ------------

   procedure Key_Up (Ch : Character) is
   begin
      Key_Up (Character_Key (Ch));
   end Key_Up;

   ---------------
   -- Keys_Down --
   ---------------

   function Keys_Down return Array_Of_Key_Codes is
      Result : Array_Of_Key_Codes (1 .. Max_Simultaneous_Keys);
      Count  : Natural := 0;
   begin
      for K of Current_Keys_Pressed loop
         if K /= No_Key then
            Count := Count + 1;
            Result (Count) := K;
         end if;
      end loop;
      return Result (1 .. Count);
   end Keys_Down;

end Rho.Keyboard;
