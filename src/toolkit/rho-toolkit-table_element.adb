package body Rho.Toolkit.Table_Element is

   -----------------
   -- Append_Cell --
   -----------------

   procedure Append_Cell
     (Row  : not null access Rho_Table_Row_Record'Class;
      Cell : not null access Rho_Table_Data_Cell_Record'Class)
   is
   begin
      Row.Add_Child (Cell);
   end Append_Cell;

   ----------------
   -- Append_Row --
   ----------------

   procedure Append_Row
     (Table : not null access Rho_Table_Record'Class;
      Row   : not null access Rho_Table_Row_Record'Class)
   is
   begin
      Table.Add_Child (Row);
   end Append_Row;

   ----------
   -- Cell --
   ----------

   function Cell
     (Row        : Rho_Table_Row_Record'Class;
      Cell_Index : Positive)
      return Rho_Table_Cell
   is
   begin
      return Rho_Table_Cell (Row.Child (Cell_Index));
   end Cell;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Table : not null access Rho_Table_Record'Class)
   is
   begin
      Table.Delete_Children;
   end Clear;

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New
     (Table_Body : in out Rho_Table_Body)
   is
   begin
      Table_Body := new Rho_Table_Body_Record;
      Table_Body.Set_Tag ("tbody");
   end Rho_New;

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New
     (Table_Data : in out Rho_Table_Data_Cell)
   is
   begin
      Table_Data := new Rho_Table_Data_Cell_Record;
      Table_Data.Set_Tag ("td");
   end Rho_New;

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New
     (Table_Header : in out Rho_Table_Header)
   is
   begin
      Table_Header := new Rho_Table_Header_Record;
      Table_Header.Set_Tag ("thead");
   end Rho_New;

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New
     (Table_Heading : in out Rho_Table_Heading_Cell)
   is
   begin
      Table_Heading := new Rho_Table_Heading_Cell_Record;
      Table_Heading.Set_Tag ("th");
   end Rho_New;

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New
     (Table_Row : in out Rho_Table_Row)
   is
   begin
      Table_Row := new Rho_Table_Row_Record;
      Table_Row.Set_Tag ("tr");
   end Rho_New;

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New
     (Table : in out Rho_Table)
   is
   begin
      Table := new Rho_Table_Record;
      Table.Set_Tag ("table");
   end Rho_New;

   -------------
   -- Rho_New --
   -------------

   function Rho_New
     return Rho_Table_Body
   is
      Table_Body : Rho_Table_Body;
   begin
      Rho_New (Table_Body);
      return Table_Body;
   end Rho_New;

   -------------
   -- Rho_New --
   -------------

   function Rho_New
     return Rho_Table_Data_Cell
   is
      Table_Data : Rho_Table_Data_Cell;
   begin
      Rho_New (Table_Data);
      return Table_Data;
   end Rho_New;

   -------------
   -- Rho_New --
   -------------

   function Rho_New
     return Rho_Table_Header
   is
      Table_Header : Rho_Table_Header;
   begin
      Rho_New (Table_Header);
      return Table_Header;
   end Rho_New;

   -------------
   -- Rho_New --
   -------------

   function Rho_New
     return Rho_Table_Heading_Cell
   is
      Table_Heading : Rho_Table_Heading_Cell;
   begin
      Rho_New (Table_Heading);
      return Table_Heading;
   end Rho_New;

   -------------
   -- Rho_New --
   -------------

   function Rho_New
     return Rho_Table_Row
   is
      Table_Row : Rho_Table_Row;
   begin
      Rho_New (Table_Row);
      return Table_Row;
   end Rho_New;

   -------------
   -- Rho_New --
   -------------

   function Rho_New
     return Rho_Table
   is
      Table : Rho_Table;
   begin
      Rho_New (Table);
      return Table;
   end Rho_New;

   ---------
   -- Row --
   ---------

   function Row
     (Table : Rho_Table_Record'Class;
      Row   : Positive)
      return Rho_Table_Row
   is
   begin
      return Rho_Table_Row (Table.Child (Row));
   end Row;

end Rho.Toolkit.Table_Element;
