package Rho.Toolkit.Text is

   type Rho_Text_Wrap_Mode is (No_Wrap, Character_Wrap, Word_Wrap);

   type Rho_Text_View_Interface is interface;

   procedure Text_Changed
     (View : in out Rho_Text_View_Interface)
   is null;

   type Rho_Text_Iter is private;

   type Rho_Line_Count is new Natural;
   subtype Rho_Line_Index is Rho_Line_Count range 1 .. Rho_Line_Count'Last;

   type Rho_Character_Offset is new Natural;

private

   type Rho_Text_Iter is new Natural;

end Rho.Toolkit.Text;
