private with Ada.Containers.Doubly_Linked_Lists;

with Rho.Toolkit.Values;

package Rho.Toolkit.Grids.Models is

   type Rho_Grid_Model_Record is abstract tagged private;

   type Rho_Grid_Model is access all Rho_Grid_Model_Record'Class;

   function Row_Count (Model : Rho_Grid_Model_Record) return Natural
                       is abstract;

   function Col_Count (Model : Rho_Grid_Model_Record) return Natural
                       is abstract;

   function Cell_Value
     (Model : Rho_Grid_Model_Record;
      Row   : Positive;
      Col   : Positive)
      return Rho.Toolkit.Values.Rho_Value_Interface'Class
      is abstract
     with Pre'Class => Row <= Model.Row_Count
     and then Col <= Model.Col_Count;

   procedure Set_Cell_Value
     (Model : in out Rho_Grid_Model_Record;
      Row   : Positive;
      Col   : Positive;
      Value : Rho.Toolkit.Values.Rho_Value_Interface'Class)
   is abstract
     with Pre'Class => Row <= Model.Row_Count
     and then Col <= Model.Col_Count;

   procedure Layout_Changed
     (Model : in out Rho_Grid_Model_Record'Class);

   procedure Cell_Changed
     (Model : in out Rho_Grid_Model_Record'Class;
      Row     : Positive;
      Col     : Positive);

   procedure Row_Added
     (Model     : in out Rho_Grid_Model_Record'Class;
      After_Row : Natural);

   procedure Row_Deleted
     (Model     : in out Rho_Grid_Model_Record'Class;
      Row       : Positive);

   type Grid_Watcher_Interface is interface;

   procedure Model_Layout_Changed
     (Watcher : not null access Grid_Watcher_Interface)
   is abstract;

   procedure Model_Cell_Changed
     (Watcher : not null access Grid_Watcher_Interface;
      Row     : Positive;
      Col     : Positive)
   is abstract;

   procedure Model_Row_Added
     (Watcher   : not null access Grid_Watcher_Interface;
      After_Row : Natural)
   is abstract;

   procedure Model_Row_Deleted
     (Watcher   : not null access Grid_Watcher_Interface;
      Row       : Positive)
   is abstract;

   procedure Add_Watcher
     (Model : in out Rho_Grid_Model_Record'Class;
      Watcher : not null access Grid_Watcher_Interface'Class);

   procedure Remove_Watcher
     (Model   : in out Rho_Grid_Model_Record'Class;
      Watcher : not null access Grid_Watcher_Interface'Class);

private

   type Grid_Watcher is access all Grid_Watcher_Interface'Class;

   package Grid_Watcher_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Grid_Watcher);

   type Rho_Grid_Model_Record is abstract tagged
      record
         Watchers : Grid_Watcher_Lists.List;
      end record;

end Rho.Toolkit.Grids.Models;
