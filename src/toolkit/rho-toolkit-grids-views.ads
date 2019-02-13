private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Vectors;

with Rho.Toolkit.Values.Renderers;
with Rho.Toolkit.Widget;
with Rho.Toolkit.Grids.Models;

private with Rho.Toolkit.Table_Element;

package Rho.Toolkit.Grids.Views is

   type Rho_Grid_View_Record is
     new Rho.Toolkit.Widget.Rho_Widget_Record
     and Rho.Toolkit.Grids.Models.Grid_Watcher_Interface
   with private;

   type Rho_Grid_View is access all Rho_Grid_View_Record'Class;

   procedure Create
     (Item  : in out Rho_Grid_View_Record);

   procedure Rho_New
     (Item       : in out Rho_Grid_View;
      Model      : access
        Rho.Toolkit.Grids.Models.Rho_Grid_Model_Record'Class);

   procedure Rho_New
     (Item       : in out Rho_Grid_View);

   procedure Append_Column
     (View : in out Rho_Grid_View_Record;
      Source_Column : Positive;
      Renderer      : Rho.Toolkit.Values.Renderers.Rho_Value_Renderer);

   procedure Set_Model
     (Item  : not null access Rho_Grid_View_Record;
      Model : access Rho.Toolkit.Grids.Models.Rho_Grid_Model_Record'Class);

   function Model
     (Item : Rho_Grid_View_Record)
      return Rho.Toolkit.Grids.Models.Rho_Grid_Model;

private

   package Renderer_Holder is
     new Ada.Containers.Indefinite_Holders
       (Element_Type => Rho.Toolkit.Values.Renderers.Rho_Value_Renderer,
        "="          => Rho.Toolkit.Values.Renderers."=");

   type Column_Info is
      record
         Model_Column : Positive;
         Renderer     : Renderer_Holder.Holder;
      end record;

   package Column_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Column_Info);

   type Rho_Grid_View_Record is
     new Rho.Toolkit.Table_Element.Rho_Table_Record
     and Rho.Toolkit.Grids.Models.Grid_Watcher_Interface with
      record
         Model   : Rho.Toolkit.Grids.Models.Rho_Grid_Model;
         Columns : Column_Info_Vectors.Vector;
      end record;

   overriding procedure Model_Layout_Changed
     (View : not null access Rho_Grid_View_Record);

   overriding procedure Model_Cell_Changed
     (View : not null access Rho_Grid_View_Record;
      Row  : Positive;
      Col  : Positive);

   overriding procedure Model_Row_Added
     (View      : not null access Rho_Grid_View_Record;
      After_Row : Natural);

   overriding procedure Model_Row_Deleted
     (View : not null access Rho_Grid_View_Record;
      Row  : Positive)
   is null;

   function Model
     (Item : Rho_Grid_View_Record)
      return Rho.Toolkit.Grids.Models.Rho_Grid_Model
   is (Item.Model);

end Rho.Toolkit.Grids.Views;
