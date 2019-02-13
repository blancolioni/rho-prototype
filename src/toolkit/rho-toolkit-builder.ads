private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;
private with Partoe.DOM;

private with Rho.String_Maps;

with Rho.Toolkit.Widget;
with Rho.Toolkit.Page;

package Rho.Toolkit.Builder is

   type Rho_Builder_Record is tagged private;
   type Rho_Builder is access all Rho_Builder_Record'Class;

   function Rho_New_From_File
     (Path : String)
      return Rho_Builder;

   function Get
     (Builder : Rho_Builder_Record'Class;
      Name    : String)
      return Rho.Toolkit.Widget.Rho_Widget;

   function Get_Page
     (Builder : Rho_Builder_Record'Class)
      return Rho.Toolkit.Page.Rho_Page;

private

   package Widget_Maps is
     new Rho.String_Maps
       (Rho.Toolkit.Widget.Rho_Widget, Rho.Toolkit.Widget."=");

   package Widget_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Rho.Toolkit.Widget.Rho_Widget, Rho.Toolkit.Widget."=");

   type Rho_Builder_Record is tagged
      record
         Containing_Path : Ada.Strings.Unbounded.Unbounded_String;
         Document        : Partoe.DOM.Partoe_Document;
         Widgets         : Widget_Maps.Map;
         Page            : Rho.Toolkit.Page.Rho_Page;
      end record;

   procedure Load
     (Builder : in out Rho_Builder_Record'Class;
      Node    : not null access Partoe.DOM.Root_Partoe_Node'Class;
      Parent  : Rho.Toolkit.Widget.Rho_Widget;
      Index   : Natural := 0);

   procedure Load_Children
     (Builder : in out Rho_Builder_Record'Class;
      Node    : not null access Partoe.DOM.Root_Partoe_Node'Class;
      Parent  : Rho.Toolkit.Widget.Rho_Widget);

   procedure Load_Link
     (Builder : in out Rho_Builder_Record'Class;
      Rel     : String;
      Href    : String);

   function Full_Path
     (Builder : Rho_Builder_Record'Class;
      Path    : String)
      return String;

   function Get_Page
     (Builder : Rho_Builder_Record'Class)
      return Rho.Toolkit.Page.Rho_Page
   is (Builder.Page);

end Rho.Toolkit.Builder;
