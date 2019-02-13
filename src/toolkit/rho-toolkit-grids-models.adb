package body Rho.Toolkit.Grids.Models is

   -----------------
   -- Add_Watcher --
   -----------------

   procedure Add_Watcher
     (Model : in out Rho_Grid_Model_Record'Class;
      Watcher : not null access Grid_Watcher_Interface'Class)
   is
   begin
      Model.Watchers.Append (Grid_Watcher (Watcher));
   end Add_Watcher;

   ------------------
   -- Cell_Changed --
   ------------------

   procedure Cell_Changed
     (Model   : in out Rho_Grid_Model_Record'Class;
      Row     : Positive;
      Col     : Positive)
   is
   begin
      for Watcher of Model.Watchers loop
         Watcher.Model_Cell_Changed (Row, Col);
      end loop;
   end Cell_Changed;

   --------------------
   -- Layout_Changed --
   --------------------

   procedure Layout_Changed
     (Model : in out Rho_Grid_Model_Record'Class)
   is
   begin
      for Watcher of Model.Watchers loop
         Watcher.Model_Layout_Changed;
      end loop;
   end Layout_Changed;

   --------------------
   -- Remove_Watcher --
   --------------------

   procedure Remove_Watcher
     (Model   : in out Rho_Grid_Model_Record'Class;
      Watcher : not null access Grid_Watcher_Interface'Class)
   is
      use Grid_Watcher_Lists;
      Position : Cursor := Model.Watchers.Find (Grid_Watcher (Watcher));
   begin
      if Has_Element (Position) then
         Model.Watchers.Delete (Position);
      end if;
   end Remove_Watcher;

   ---------------
   -- Row_Added --
   ---------------

   procedure Row_Added
     (Model     : in out Rho_Grid_Model_Record'Class;
      After_Row : Natural)
   is
   begin
      for Watcher of Model.Watchers loop
         Watcher.Model_Row_Added (After_Row);
      end loop;
   end Row_Added;

   -----------------
   -- Row_Deleted --
   -----------------

   procedure Row_Deleted
     (Model : in out Rho_Grid_Model_Record'Class;
      Row   : Positive)
   is
   begin
      for Watcher of Model.Watchers loop
         Watcher.Model_Row_Deleted (Row);
      end loop;
   end Row_Deleted;

end Rho.Toolkit.Grids.Models;
