package body Rho.Toolkit.Button is

   ------------
   -- Create --
   ------------

   procedure Create
     (Item       : in out Rho_Button_Record;
      Label      : String;
      Element_Id : String)
   is
   begin
      Rho.Toolkit.Label.Rho_Label_Record (Item)
        .Create_With_Label (Label, Element_Id);
   end Create;

   ------------
   -- Rho_New --
   ------------

   procedure Rho_New
     (Item       : in out Rho_Button;
      Text       : String;
      Element_Id : String)
   is
   begin
      Item := new Rho_Button_Record;
      Item.Create (Text, Element_Id);
   end Rho_New;

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New
     (Item       : in out Rho_Button)
   is
   begin
      Item := new Rho_Button_Record;
      Item.Create ("", "");
   end Rho_New;

end Rho.Toolkit.Button;
