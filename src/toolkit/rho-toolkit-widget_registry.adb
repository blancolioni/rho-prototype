with Rho.String_Maps;

with Rho.Toolkit.Button;
with Rho.Toolkit.Div_Element;
with Rho.Toolkit.Grids.Views;
with Rho.Toolkit.Label;
with Rho.Toolkit.Panel;
with Rho.Toolkit.Table_Element;
with Rho.Toolkit.Text.View;

package body Rho.Toolkit.Widget_Registry is

   package Creator_Registry_Maps is
     new Rho.String_Maps (Creator_Function);

   Registry : Creator_Registry_Maps.Map;

   function Create_Button
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget;

   function Create_Div
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget;

   function Create_Grid
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget;

   function Create_Label
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget;

   function Create_Table
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget;

   function Create_Table_Header
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget;

   function Create_Table_Body
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget;

   function Create_Table_Row
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget;

   function Create_Table_Heading
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget;

   function Create_Table_Data
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget;

   function Create_Text_View
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget;

   ------------
   -- Create --
   ------------

   function Create
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget
   is
   begin
      if Registry.Contains (Tag) then
         declare
            use type Rho.Toolkit.Widget.Rho_Widget;
            Widget : constant Rho.Toolkit.Widget.Rho_Widget :=
                       Registry.Element (Tag) (Tag, Parent, Top_Level,
                                               Inner_Text);
         begin
            Widget.Set_Tag (Tag);
            if Parent /= null then
               if Widget.Required_Parent_Tag /= ""
                 and then Widget.Required_Parent_Tag /= Parent.Tag
               then
                  declare
                     Auto_Parent : constant Rho.Toolkit.Widget.Rho_Widget :=
                                     Create (Widget.Required_Parent_Tag,
                                             Parent, False, "");
                  begin
                     Auto_Parent.Add_Child (Widget);
                  end;
               else
                  Parent.Add_Child (Widget);
               end if;
            end if;

            return Widget;
         end;
      else
         return null;
      end if;
   end Create;

   -------------------
   -- Create_Button --
   -------------------

   function Create_Button
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget
   is
      pragma Unreferenced (Tag, Parent, Top_Level);
      Result : Rho.Toolkit.Button.Rho_Button;
   begin
      Rho.Toolkit.Button.Rho_New (Result, Inner_Text, "");
      return Rho.Toolkit.Widget.Rho_Widget (Result);
   end Create_Button;

   ----------------
   -- Create_Div --
   ----------------

   function Create_Div
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget
   is
      pragma Unreferenced (Tag, Parent, Inner_Text);
   begin
      if Top_Level then
         declare
            Panel : Rho.Toolkit.Panel.Rho_Panel;
         begin
            Rho.Toolkit.Panel.Rho_New (Panel);
            return Rho.Toolkit.Widget.Rho_Widget (Panel);
         end;
      else
         declare
            Div : Rho.Toolkit.Div_Element.Rho_Div_Element;
         begin
            Rho.Toolkit.Div_Element.Rho_New (Div);
            return Rho.Toolkit.Widget.Rho_Widget (Div);
         end;
      end if;
   end Create_Div;

   -----------------
   -- Create_Grid --
   -----------------

   function Create_Grid
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget
   is
      pragma Unreferenced (Tag, Parent, Top_Level, Inner_Text);
      Result : Rho.Toolkit.Grids.Views.Rho_Grid_View;
   begin
      Rho.Toolkit.Grids.Views.Rho_New (Result);
      return Rho.Toolkit.Widget.Rho_Widget (Result);
   end Create_Grid;

   ------------------
   -- Create_Label --
   ------------------

   function Create_Label
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget
   is
      pragma Unreferenced (Tag, Parent, Top_Level);
      Result : Rho.Toolkit.Label.Rho_Label;
   begin
      Rho.Toolkit.Label.Rho_New (Result, Inner_Text);
      return Rho.Toolkit.Widget.Rho_Widget (Result);
   end Create_Label;

   ------------------
   -- Create_Table --
   ------------------

   function Create_Table
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget
   is
      pragma Unreferenced (Tag, Parent, Top_Level, Inner_Text);
      Result : Rho.Toolkit.Table_Element.Rho_Table;
   begin
      Rho.Toolkit.Table_Element.Rho_New (Result);
      return Rho.Toolkit.Widget.Rho_Widget (Result);
   end Create_Table;

   -----------------------
   -- Create_Table_Body --
   -----------------------

   function Create_Table_Body
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget
   is
      pragma Unreferenced (Tag, Parent, Top_Level, Inner_Text);
      Result : Rho.Toolkit.Table_Element.Rho_Table_Body;
   begin
      Rho.Toolkit.Table_Element.Rho_New (Result);
      return Rho.Toolkit.Widget.Rho_Widget (Result);
   end Create_Table_Body;

   -----------------------
   -- Create_Table_Data --
   -----------------------

   function Create_Table_Data
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget
   is
      pragma Unreferenced (Tag, Parent, Top_Level);
      Result : Rho.Toolkit.Table_Element.Rho_Table_Data_Cell;
   begin
      Rho.Toolkit.Table_Element.Rho_New (Result);
      if Inner_Text /= "" then
         declare
            Label : constant Rho.Toolkit.Label.Rho_Label :=
                      Rho.Toolkit.Label.Rho_New (Inner_Text);
         begin
            Result.Add_Child (Label);
         end;
      end if;
      return Rho.Toolkit.Widget.Rho_Widget (Result);
   end Create_Table_Data;

   -------------------------
   -- Create_Table_Header --
   -------------------------

   function Create_Table_Header
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget
   is
      pragma Unreferenced (Tag, Parent, Top_Level, Inner_Text);
      Result : Rho.Toolkit.Table_Element.Rho_Table_Header;
   begin
      Rho.Toolkit.Table_Element.Rho_New (Result);
      return Rho.Toolkit.Widget.Rho_Widget (Result);
   end Create_Table_Header;

   --------------------------
   -- Create_Table_Heading --
   --------------------------

   function Create_Table_Heading
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget
   is
      pragma Unreferenced (Tag, Parent, Top_Level);
      Result : Rho.Toolkit.Table_Element.Rho_Table_Heading_Cell;
   begin
      Rho.Toolkit.Table_Element.Rho_New (Result);
      if Inner_Text /= "" then
         declare
            Label : constant Rho.Toolkit.Label.Rho_Label :=
                      Rho.Toolkit.Label.Rho_New (Inner_Text);
         begin
            Result.Add_Child (Label);
         end;
      end if;
      return Rho.Toolkit.Widget.Rho_Widget (Result);
   end Create_Table_Heading;

   ----------------------
   -- Create_Table_Row --
   ----------------------

   function Create_Table_Row
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget
   is
      pragma Unreferenced (Tag, Parent, Top_Level, Inner_Text);
      Result : Rho.Toolkit.Table_Element.Rho_Table_Row;
   begin
      Rho.Toolkit.Table_Element.Rho_New (Result);
      return Rho.Toolkit.Widget.Rho_Widget (Result);
   end Create_Table_Row;

   ----------------------
   -- Create_Text_View --
   ----------------------

   function Create_Text_View
     (Tag        : String;
      Parent     : Rho.Toolkit.Widget.Rho_Widget;
      Top_Level  : Boolean;
      Inner_Text : String)
      return Rho.Toolkit.Widget.Rho_Widget
   is
      pragma Unreferenced (Tag, Parent, Top_Level);
      Result : Rho.Toolkit.Text.View.Rho_Text_View;
   begin
      Rho.Toolkit.Text.View.Rho_New (Result);
      Result.Text_Buffer.Append (Inner_Text);
      return Rho.Toolkit.Widget.Rho_Widget (Result);
   end Create_Text_View;

   --------------
   -- Register --
   --------------

   procedure Register
     (Tag     : String;
      Creator : Creator_Function)
   is
      pragma Assert (not Registry.Contains (Tag));
   begin
      Registry.Insert (Tag, Creator);
   end Register;

   -------------------------------
   -- Register_Standard_Widgets --
   -------------------------------

   procedure Register_Standard_Widgets is
   begin
      if not Registry.Is_Empty then
         return;
      end if;

      Register ("button", Create_Button'Access);
      Register ("div", Create_Div'Access);
      Register ("grid", Create_Grid'Access);
      Register ("label", Create_Label'Access);
      Register ("table", Create_Table'Access);
      Register ("thead", Create_Table_Header'Access);
      Register ("tbody", Create_Table_Body'Access);
      Register ("tr", Create_Table_Row'Access);
      Register ("th", Create_Table_Heading'Access);
      Register ("td", Create_Table_Data'Access);
      Register ("textarea", Create_Text_View'Access);
   end Register_Standard_Widgets;

end Rho.Toolkit.Widget_Registry;
