private with Ada.Containers.Doubly_Linked_Lists;

with Css;

with Rho.Toolkit.Container;
with Rho.Toolkit.Widget;

package Rho.Toolkit.Table_Element is

   type Rho_Table_Cell_Record is
     abstract new Rho.Toolkit.Container.Rho_Container_Record with private;

   type Rho_Table_Cell is access all Rho_Table_Cell_Record'Class;

   type Rho_Table_Heading_Cell_Record is
     new Rho_Table_Cell_Record with private;

   type Rho_Table_Heading_Cell is
     access all Rho_Table_Heading_Cell_Record'Class;

   procedure Rho_New
     (Table_Heading : in out Rho_Table_Heading_Cell);

   function Rho_New
     return Rho_Table_Heading_Cell;

   type Rho_Table_Data_Cell_Record is
     new Rho_Table_Cell_Record with private;

   type Rho_Table_Data_Cell is access all Rho_Table_Data_Cell_Record'Class;

   procedure Rho_New
     (Table_Data : in out Rho_Table_Data_Cell);

   function Rho_New
     return Rho_Table_Data_Cell;

   type Rho_Table_Row_Record is
     new Rho.Toolkit.Widget.Rho_Widget_Record with private;

   type Rho_Table_Row is access all Rho_Table_Row_Record'Class;

   procedure Rho_New
     (Table_Row : in out Rho_Table_Row);

   function Rho_New
     return Rho_Table_Row;

   function Cell
     (Row        : Rho_Table_Row_Record'Class;
      Cell_Index : Positive)
      return Rho_Table_Cell;

   procedure Append_Cell
     (Row : not null access Rho_Table_Row_Record'Class;
      Cell : not null access Rho_Table_Data_Cell_Record'Class);

   type Rho_Table_Header_Record is
     new Rho.Toolkit.Widget.Rho_Widget_Record with private;

   type Rho_Table_Header is access all Rho_Table_Header_Record'Class;

   procedure Rho_New
     (Table_Header : in out Rho_Table_Header);

   function Rho_New
     return Rho_Table_Header;

   type Rho_Table_Body_Record is
     new Rho.Toolkit.Widget.Rho_Widget_Record with private;

   type Rho_Table_Body is access all Rho_Table_Body_Record'Class;

   procedure Rho_New
     (Table_Body : in out Rho_Table_Body);

   function Rho_New
     return Rho_Table_Body;

   type Rho_Table_Record is
     new Rho.Toolkit.Widget.Rho_Widget_Record with private;

--     overriding function Minimum_Size
--       (Table      : Rho_Table_Record;
--        Constraint : Css.Layout_Size)
--        return Css.Layout_Size;

   type Rho_Table is access all Rho_Table_Record'Class;

   overriding function Is_Table
     (Table : Rho_Table_Record)
      return Boolean
   is (True);

   function Row
     (Table : Rho_Table_Record'Class;
      Row   : Positive)
      return Rho_Table_Row;

   procedure Clear
     (Table : not null access Rho_Table_Record'Class);

   procedure Append_Row
     (Table : not null access Rho_Table_Record'Class;
      Row   : not null access Rho_Table_Row_Record'Class);

   procedure Rho_New
     (Table : in out Rho_Table);

   function Rho_New
     return Rho_Table;

private

   type Rho_Table_Cell_Record is
     abstract new Rho.Toolkit.Container.Rho_Container_Record with
      record
         null;
      end record;

   package Table_Cell_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Rho_Table_Cell);

   type Rho_Table_Heading_Cell_Record is
     new Rho_Table_Cell_Record with null record;

   type Rho_Table_Data_Cell_Record is
     new Rho_Table_Cell_Record with null record;

   type Rho_Table_Row_Record is
     new Rho.Toolkit.Container.Rho_Container_Record with
      record
         Cells : Table_Cell_Lists.List;
      end record;

   package Table_Row_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Rho_Table_Row);

   type Rho_Table_Row_Container_Record is
     abstract new Rho.Toolkit.Container.Rho_Container_Record with
      record
         Rows : Table_Row_Lists.List;
      end record;

   type Rho_Table_Header_Record is
     new Rho_Table_Row_Container_Record with null record;

   type Rho_Table_Body_Record is
     new Rho_Table_Row_Container_Record with null record;

   type Rho_Table_Record is
     new Rho.Toolkit.Container.Rho_Container_Record with
      record
         Table_Header : Rho_Table_Header;
         Table_Body   : Rho_Table_Body;
      end record;

end Rho.Toolkit.Table_Element;
