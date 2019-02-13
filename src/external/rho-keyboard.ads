package Rho.Keyboard is

   type Control_Key is (Shift, Control, Alt, Meta);

   type Control_Mask is array (Control_Key) of Boolean;

   All_False : constant Control_Mask := (others => False);

   type Rho_Key_Code is private;

   type Array_Of_Key_Codes is array (Positive range <>) of Rho_Key_Code;

   function Key_Down (Code : Rho_Key_Code) return Boolean;

   function First_Key_Down return Rho_Key_Code;

   function Keys_Down return Array_Of_Key_Codes;

   procedure Key_Down (Code : Rho_Key_Code);
   procedure Key_Down (Ch : Character);

   procedure Key_Up (Code : Rho_Key_Code);
   procedure Key_Up (Ch : Character);

   function Character_Key (Ch : Character) return Rho_Key_Code;

   function Is_Graphic (Item : Rho_Key_Code) return Boolean;

   function To_Character
     (Item : Rho_Key_Code)
      return Character;

   No_Key : constant Rho_Key_Code;

   Key_Esc : constant Rho_Key_Code;

   Key_F1  : constant Rho_Key_Code;
   Key_F2  : constant Rho_Key_Code;
   Key_F3  : constant Rho_Key_Code;
   Key_F4  : constant Rho_Key_Code;
   Key_F5  : constant Rho_Key_Code;
   Key_F6  : constant Rho_Key_Code;
   Key_F7  : constant Rho_Key_Code;
   Key_F8  : constant Rho_Key_Code;
   Key_F9  : constant Rho_Key_Code;
   Key_F10  : constant Rho_Key_Code;
   Key_F11  : constant Rho_Key_Code;
   Key_F12  : constant Rho_Key_Code;

   Key_Back     : constant Rho_Key_Code;
   Key_New_Line : constant Rho_Key_Code;

   function Changed return Boolean;
   procedure Clear_Changed;

private

   type Rho_Key_Code is range 0 .. 65535;

   No_Key  : constant Rho_Key_Code := Rho_Key_Code'Last;

   Key_Back     : constant Rho_Key_Code := 16#0008#;
   Key_New_Line : constant Rho_Key_Code := 16#000A#;
   Key_Esc      : constant Rho_Key_Code := 16#001B#;

   Key_F1   : constant Rho_Key_Code := 16#F0F1#;
   Key_F2   : constant Rho_Key_Code := 16#F0F2#;
   Key_F3   : constant Rho_Key_Code := 16#F0F3#;
   Key_F4   : constant Rho_Key_Code := 16#F0F4#;
   Key_F5   : constant Rho_Key_Code := 16#F0F5#;
   Key_F6   : constant Rho_Key_Code := 16#F0F6#;
   Key_F7   : constant Rho_Key_Code := 16#F0F7#;
   Key_F8   : constant Rho_Key_Code := 16#F0F8#;
   Key_F9   : constant Rho_Key_Code := 16#F0F9#;
   Key_F10  : constant Rho_Key_Code := 16#F0FA#;
   Key_F11  : constant Rho_Key_Code := 16#F0FB#;
   Key_F12  : constant Rho_Key_Code := 16#F0FC#;

   function Is_Graphic (Item : Rho_Key_Code) return Boolean
   is (Item in 32 .. 127);

   function To_Character
     (Item : Rho_Key_Code)
      return Character
   is (Character'Val (Item));

end Rho.Keyboard;
