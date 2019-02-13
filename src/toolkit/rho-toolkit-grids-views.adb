package body Rho.Toolkit.Grids.Views is

   procedure Create_Grid
     (Grid : not null access Rho_Grid_View_Record);

   procedure Create_Row
     (Grid      : not null access Rho_Grid_View_Record;
      Row_Index : Positive);

   -------------------
   -- Append_Column --
   -------------------

   procedure Append_Column
     (View          : in out Rho_Grid_View_Record;
      Source_Column : Positive;
      Renderer      : Rho.Toolkit.Values.Renderers.Rho_Value_Renderer)
   is
   begin
      View.Columns.Append
        ((Source_Column, Renderer_Holder.To_Holder (Renderer)));
   end Append_Column;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item  : in out Rho_Grid_View_Record)
   is
   begin
      null;
   end Create;

   -----------------
   -- Create_Grid --
   -----------------

   procedure Create_Grid
     (Grid : not null access Rho_Grid_View_Record)
   is
   begin
      Grid.Clear;
      for Row_Index in 1 .. Grid.Model.Row_Count loop
         Create_Row (Grid, Row_Index);
      end loop;
   end Create_Grid;

   ----------------
   -- Create_Row --
   ----------------

   procedure Create_Row
     (Grid      : not null access Rho_Grid_View_Record;
      Row_Index : Positive)
   is
      pragma Unreferenced (Row_Index);
      Row : constant Rho.Toolkit.Table_Element.Rho_Table_Row :=
              Rho.Toolkit.Table_Element.Rho_New;
   begin
      if Grid.Columns.Is_Empty then
         for Col_Index in 1 .. Grid.Model.Col_Count loop
            declare
               Cell  : constant Table_Element.Rho_Table_Data_Cell :=
                         Rho.Toolkit.Table_Element.Rho_New;
            begin
               Row.Append_Cell (Cell);
            end;
         end loop;
      else
         for Col_Map of Grid.Columns loop
            declare
               Cell  : constant Table_Element.Rho_Table_Data_Cell :=
                         Rho.Toolkit.Table_Element.Rho_New;
            begin
               Row.Append_Cell (Cell);
            end;
         end loop;
      end if;
      Grid.Append_Row (Row);
      Grid.Show_All;
   end Create_Row;

   ------------------------
   -- Model_Cell_Changed --
   ------------------------

   overriding procedure Model_Cell_Changed
     (View : not null access Rho_Grid_View_Record;
      Row  : Positive;
      Col  : Positive)
   is
      New_Value : constant Rho.Toolkit.Values.Rho_Value_Interface'Class :=
                    View.Model.Cell_Value (Row, Col);
   begin
      if View.Columns.Is_Empty then
         declare
            Cell    : constant Rho.Toolkit.Table_Element.Rho_Table_Cell :=
                        View.Row (Row).Cell (Col);
         begin
            if Cell.Has_Children then
               Rho.Toolkit.Values.Renderers.Text_Renderer.Update
                 (Cell, New_Value);
            else
               Cell.Add_Child
                 (Rho.Toolkit.Values.Renderers.Text_Renderer.Render
                    (New_Value));
            end if;
         end;
      else
         for View_Col_Index in 1 .. View.Columns.Last_Index loop
            declare
               Col_Map : constant Column_Info :=
                           View.Columns.Element (View_Col_Index);
               Cell    : constant Rho.Toolkit.Table_Element.Rho_Table_Cell :=
                           View.Row (Row).Cell (View_Col_Index);
            begin
               if Col_Map.Model_Column = Col then
                  if Cell.Has_Children then
                     Col_Map.Renderer.Element.Update
                       (Cell.First_Child,
                        New_Value);
                  else
                     Cell.Add_Child (Col_Map.Renderer.Element.Render
                                     (New_Value));
                  end if;
               end if;
            end;
         end loop;
      end if;
   end Model_Cell_Changed;

   --------------------------
   -- Model_Layout_Changed --
   --------------------------

   overriding procedure Model_Layout_Changed
     (View : not null access Rho_Grid_View_Record)
   is
   begin
      Create_Grid (View);
   end Model_Layout_Changed;

   ---------------------
   -- Model_Row_Added --
   ---------------------

   overriding procedure Model_Row_Added
     (View      : not null access Rho_Grid_View_Record;
      After_Row : Natural)
   is
   begin
      Create_Row (View, After_Row + 1);
   end Model_Row_Added;

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New
     (Item       : in out Rho_Grid_View;
      Model      : access Rho.Toolkit.Grids.Models.Rho_Grid_Model_Record'Class)
   is
   begin
      Item := new Rho_Grid_View_Record;
      Item.Create;
      if Model /= null then
         Item.Set_Model (Rho.Toolkit.Grids.Models.Rho_Grid_Model (Model));
      end if;
   end Rho_New;

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New
     (Item       : in out Rho_Grid_View)
   is
   begin
      Rho_New (Item, null);
   end Rho_New;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
     (Item  : not null access Rho_Grid_View_Record;
      Model : access Rho.Toolkit.Grids.Models.Rho_Grid_Model_Record'Class)
   is
      use Rho.Toolkit.Grids.Models;
   begin
      if Item.Model /= null then
         Item.Model.Remove_Watcher (Item);
         Item.Model := null;
      end if;

      if Model /= null then
         Item.Model := Rho_Grid_Model (Model);
         Item.Model.Add_Watcher (Item);
      end if;

      Create_Grid (Item);

   end Set_Model;

end Rho.Toolkit.Grids.Views;
