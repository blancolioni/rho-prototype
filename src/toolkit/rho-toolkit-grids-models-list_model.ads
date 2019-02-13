private with Ada.Containers.Vectors;
private with Rho.Toolkit.Values.Vectors;

package Rho.Toolkit.Grids.Models.List_Model is

   type Rho_List_Model_Record is
     new Rho_Grid_Model_Record with private;

   type Rho_List_Model is access all Rho_List_Model_Record'Class;

   overriding function Row_Count
     (Model : Rho_List_Model_Record)
      return Natural;

   overriding function Col_Count
     (Model : Rho_List_Model_Record)
      return Natural;

   overriding function Cell_Value
     (Model : Rho_List_Model_Record;
      Row   : Positive;
      Col   : Positive)
      return Rho.Toolkit.Values.Rho_Value_Interface'Class;

   overriding procedure Set_Cell_Value
     (Model : in out Rho_List_Model_Record;
      Row   : Positive;
      Col   : Positive;
      Value : Rho.Toolkit.Values.Rho_Value_Interface'Class);

   procedure Create
     (List_Model : in out Rho_List_Model_Record;
      Col_Count  : Positive);

   procedure Rho_New
     (List_Model : in out Rho_List_Model;
      Col_Count  : Positive);

   procedure Insert_Row
     (List_Model : in out Rho_List_Model_Record;
      Before     : Positive);

   procedure Append_Row
     (List_Model : in out Rho_List_Model_Record);

   procedure Delete_Row
     (List_Model : in out Rho_List_Model_Record;
      Row        : Positive);

private

   package Row_Vectors is
     new Ada.Containers.Vectors
       (Positive, Rho.Toolkit.Values.Vectors.Vector, Rho.Toolkit.Values.Vectors."=");

   type Rho_List_Model_Record is
     new Rho_Grid_Model_Record with
      record
         Col_Count : Natural := 0;
         Rows      : Row_Vectors.Vector;
      end record;

   overriding function Row_Count
     (Model : Rho_List_Model_Record)
      return Natural
   is (Model.Rows.Last_Index);

   overriding function Col_Count
     (Model : Rho_List_Model_Record)
      return Natural
   is (Model.Col_Count);

end Rho.Toolkit.Grids.Models.List_Model;
