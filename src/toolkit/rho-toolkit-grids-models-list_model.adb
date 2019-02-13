package body Rho.Toolkit.Grids.Models.List_Model is

   ----------------
   -- Append_Row --
   ----------------

   procedure Append_Row
     (List_Model : in out Rho_List_Model_Record)
   is
      use Ada.Containers;
      New_Row : Rho.Toolkit.Values.Vectors.Vector;
   begin
      New_Row.Set_Length (Count_Type (List_Model.Col_Count));
      List_Model.Rows.Append (New_Row);
      Rho_List_Model_Record'Class (List_Model).Row_Added
        (List_Model.Rows.Last_Index - 1);
   end Append_Row;

   ----------------
   -- Cell_Value --
   ----------------

   overriding function Cell_Value
     (Model : Rho_List_Model_Record;
      Row   : Positive;
      Col   : Positive)
      return Rho.Toolkit.Values.Rho_Value_Interface'Class
   is
      Model_Row : constant Rho.Toolkit.Values.Vectors.Vector :=
                    Model.Rows.Element (Row);
   begin
      return Model_Row.Element (Col);
   end Cell_Value;

   ------------
   -- Create --
   ------------

   procedure Create
     (List_Model : in out Rho_List_Model_Record;
      Col_Count  : Positive)
   is
   begin
      List_Model.Col_Count := Col_Count;
      List_Model.Rows.Clear;
   end Create;

   ----------------
   -- Delete_Row --
   ----------------

   procedure Delete_Row
     (List_Model : in out Rho_List_Model_Record;
      Row        : Positive)
   is
   begin
      List_Model.Rows.Delete (Row);
      Rho_List_Model_Record'Class (List_Model).Row_Deleted (Row);
   end Delete_Row;

   ----------------
   -- Insert_Row --
   ----------------

   procedure Insert_Row
     (List_Model : in out Rho_List_Model_Record;
      Before     : Positive)
   is
      use Ada.Containers;
      New_Row : Rho.Toolkit.Values.Vectors.Vector;
   begin
      New_Row.Set_Length (Count_Type (List_Model.Col_Count));
      List_Model.Rows.Insert (Before, New_Row);
      Rho_List_Model_Record'Class (List_Model).Row_Added (Before - 1);
   end Insert_Row;

   --------------------
   -- Set_Cell_Value --
   --------------------

   overriding procedure Set_Cell_Value
     (Model : in out Rho_List_Model_Record;
      Row   : Positive;
      Col   : Positive;
      Value : Rho.Toolkit.Values.Rho_Value_Interface'Class)
   is
   begin
      Model.Rows (Row).Replace_Element (Col, Value);
      Rho_List_Model_Record'Class (Model).Cell_Changed (Row, Col);
   end Set_Cell_Value;

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New
     (List_Model : in out Rho_List_Model;
      Col_Count  : Positive)
   is
   begin
      List_Model := new Rho_List_Model_Record;
      List_Model.Create (Col_Count);
   end Rho_New;

end Rho.Toolkit.Grids.Models.List_Model;
