with Css;

with Rho.Toolkit.Bin;

with Rho.Rectangle;
with Rho.Render_Target;
with Rho.Viewport;

with Rho.Toolkit.Buffer;
with Rho.Toolkit.Widget;

package Rho.Toolkit.Panel is

   type Rho_Panel_Record is
     new Rho.Toolkit.Bin.Rho_Bin_Record
     and Rho_Top_Level_Interface
       with private;

   procedure Create
     (Panel      : not null access Rho_Panel_Record;
      Top        : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class;
      Element_Id : String := "");

   overriding procedure Adjust (Panel : in out Rho_Panel_Record);

   overriding procedure Execute_Render
     (Panel  : in out Rho_Panel_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   overriding procedure Show
     (Panel : in out Rho_Panel_Record);

   overriding procedure Hide
     (Panel : in out Rho_Panel_Record);

   overriding procedure Invalidate_Region
     (Panel  : in out Rho_Panel_Record;
      Region : Rho.Rectangle.Rho_Rectangle);

   overriding function Widget_Hierarchy_Tags
     (Panel : Rho_Panel_Record)
      return String
   is ("Panel "
       & Rho.Toolkit.Bin.Rho_Bin_Record (Panel).Widget_Hierarchy_Tags);

   overriding function Viewport
     (Panel : Rho_Panel_Record)
      return Rho.Viewport.Rho_Viewport;

   overriding procedure Set_Viewport
     (Panel    : in out Rho_Panel_Record;
      Viewport : Rho.Viewport.Rho_Viewport);

   type Rho_Panel is access all Rho_Panel_Record'Class;

   procedure Rho_New
     (Panel  : in out Rho_Panel;
      Top    : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class);

   procedure Rho_New
     (Panel  : in out Rho_Panel);

private

   type Rho_Panel_Record is
     new Rho.Toolkit.Bin.Rho_Bin_Record
     and Rho_Top_Level_Interface with
      record
         Buffer          : Rho.Toolkit.Buffer.Rho_Buffer;
         Viewport        : Rho.Viewport.Rho_Viewport;
      end record;

   overriding function Loaded
     (Panel : Rho_Panel_Record)
      return Boolean
   is (True);

end Rho.Toolkit.Panel;
