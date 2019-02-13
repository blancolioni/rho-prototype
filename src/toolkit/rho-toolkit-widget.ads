private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

private with WL.Nullable;
private with WL.String_Maps;

with Ada.Finalization;

with Cairo;

with Rho.Color;
with Rho.Font;
with Rho.Keyboard;
with Rho.Mouse;
with Rho.Rectangle;

with Rho.Toolkit.Buildable;
with Rho.Toolkit.Events;
with Rho.Toolkit.Signals;

private with Rho.Toolkit.Signals.Maps;

with Css;

package Rho.Toolkit.Widget is

   type Widget_State is
     (Active, Hover, Insensitive, Selected, Focused, Inconsistent);

   type Border_Style_Type is (None, Solid);

   type Rho_Widget_Record is
     abstract new Ada.Finalization.Controlled
     and Css.Layout_Interface
     and Css.Css_Element_Interface
     and Rho.Toolkit.Buildable.Rho_Buildable_Interface
       with private;

   type Rho_Widget is access all Rho_Widget_Record'Class;

   overriding function Tag
     (Widget : Rho_Widget_Record)
      return String;

   overriding procedure Initialize (Widget : in out Rho_Widget_Record);
   overriding procedure Finalize (Widget : in out Rho_Widget_Record);
   overriding procedure Adjust (Widget : in out Rho_Widget_Record);

   function Default_Tag
     (Widget : Rho_Widget_Record)
      return String
      is abstract;

   overriding procedure Set_Tag
     (Widget : in out Rho_Widget_Record;
      Tag    : String);

   overriding function Required_Parent_Tag
     (Widget : Rho_Widget_Record)
      return String
   is ("");

   function Widget_Hierarchy_Tags
     (Widget : Rho_Widget_Record)
      return String
   is ("Widget");

   overriding function Classes
     (Widget : Rho_Widget_Record)
      return String;

   overriding function Inline_Style_Rules
     (Widget : Rho_Widget_Record)
      return Css.Css_Rule;

   overriding function Id
     (Widget : Rho_Widget_Record)
      return String;

   overriding function Is_Table
     (Widget : Rho_Widget_Record)
      return Boolean
   is (False);

   overriding procedure Set_Style
     (Widget  : in out Rho_Widget_Record;
      Name    : String;
      State   : String;
      Value   : Css.Css_Element_Value);

   overriding function Style
     (Widget : Rho_Widget_Record;
      Name   : String)
      return Css.Css_Element_Value;

   overriding function Default_Style_Value
     (Widget : Rho_Widget_Record;
      Name   : String)
      return Css.Css_Element_Value
   is (Css.Default_Style_Value (Name));

   overriding procedure Create_Style
     (Widget : in out Rho_Widget_Record;
      Name   : String);

   overriding function Minimum_Size
     (Widget     : Rho_Widget_Record;
      Constraint : Css.Layout_Size)
      return Css.Layout_Size;

   overriding function Child_Elements
     (Widget     : Rho_Widget_Record)
      return Css.Array_Of_Elements
   is (Css.No_Elements);

   procedure Set_Id
     (Widget     : in out Rho_Widget_Record'Class;
      Element_Id : String);

   overriding procedure Set_Attribute
     (Widget    : in out Rho_Widget_Record;
      Name      : String;
      Value     : String);

   overriding procedure Set_Text
     (Widget    : in out Rho_Widget_Record;
      Text      : String)
   is null;

   overriding procedure Add_Child
     (Widget    : not null access Rho_Widget_Record;
      Child     : not null access
        Rho.Toolkit.Buildable.Rho_Buildable_Interface'Class)
   is null;

   procedure Invalidate_Region
     (Widget : in out Rho_Widget_Record;
      Region : Rho.Rectangle.Rho_Rectangle);

   procedure Queue_Draw
     (Widget : in out Rho_Widget_Record);

   procedure Queue_Resize
     (Widget : in out Rho_Widget_Record);

   function Needs_Redraw
     (Widget : Rho_Widget_Record)
      return Boolean;

   function Needs_Resize
     (Widget : Rho_Widget_Record)
      return Boolean;

   overriding function Get_Layout_Position
     (Widget : Rho_Widget_Record)
      return Css.Layout_Position;

   overriding function Get_Layout_Size
     (Widget : Rho_Widget_Record)
      return Css.Layout_Size;

   overriding procedure Set_Layout_Position
     (Widget   : in out Rho_Widget_Record;
      Position : Css.Layout_Position);

   overriding procedure Set_Layout_Size
     (Widget   : in out Rho_Widget_Record;
      Size     : in     Css.Layout_Size);

   overriding function Contents_Layout_Size
     (Widget : Rho_Widget_Record)
      return Css.Layout_Size;

   overriding procedure Set_Contents_Layout_Size
     (Widget   : in out Rho_Widget_Record;
      Size     : in     Css.Layout_Size);

   function Has_Background_Color
     (Widget : Rho_Widget_Record)
      return Boolean;

   function Background_Color
     (Widget : Rho_Widget_Record)
      return Rho.Color.Rho_Color;

   function Foreground_Color
     (Widget : Rho_Widget_Record)
      return Rho.Color.Rho_Color;

   function Font
     (Widget : Rho_Widget_Record)
      return Rho.Font.Rho_Font;

   function Current_State
     (Widget : Rho_Widget_Record)
      return Widget_State;

   procedure Set_State
     (Widget : in out Rho_Widget_Record;
      State  : Widget_State);

   procedure Create
     (Widget     : in out Rho_Widget_Record;
      Element_Id : String);

   procedure Destroy (Widget : in out Rho_Widget);

   function Get_Child_Widget
     (Widget : Rho_Widget_Record;
      X, Y   : Rho_Float)
      return Rho_Widget;

   function Child_Count
     (Widget : Rho_Widget_Record)
      return Natural
   is (0);

   function Child
     (Widget : Rho_Widget_Record;
      Index  : Positive)
      return Rho.Toolkit.Widget.Rho_Widget;

   function Parent
     (Widget : Rho_Widget_Record)
      return Rho_Widget;

   procedure Set_Parent
     (Widget : in out Rho_Widget_Record;
      New_Parent : not null access Rho_Widget_Record'Class);

   function Top_Widget
     (Widget : Rho_Widget_Record'Class)
      return Rho_Widget;

   overriding function Parent_Element
     (Widget : Rho_Widget_Record)
      return access Css.Css_Element_Interface'Class
   is (Widget.Parent);

   overriding function Child_Index
     (Widget : Rho_Widget_Record)
      return Natural;

   procedure Set_Child_Index
     (Widget : in out Rho_Widget_Record'Class;
      Index  : Natural);

   function Get_Child_Widget_By_Id
     (Widget   : Rho_Widget_Record;
      Id       : String)
      return Rho_Widget
   is (Rho_Widget
         (Rho_Widget_Record'Class (Widget).Get_Child_With_Id (Id)));

   procedure Mouse_Over
     (Widget : in out Rho_Widget_Record;
      X, Y   : Rho_Float);

   procedure Mouse_Out
     (Widget : in out Rho_Widget_Record);

   function Shown
     (Widget : Rho_Widget_Record'Class)
      return Boolean;

   procedure Show
     (Widget : in out Rho_Widget_Record);

   procedure Hide
     (Widget : in out Rho_Widget_Record);

   function Visible
     (Widget : in out Rho_Widget_Record)
      return Boolean;

   procedure Show_All
     (Widget : in out Rho_Widget_Record);

   procedure Initialize_Styles (Widget : in out Rho_Widget_Record);

   type Style_Handler is access
     procedure (Widget : in out Rho_Widget_Record'Class;
                Value  : Css.Css_Element_Value);

   function Style_Sheet
     (Widget : Rho_Widget_Record'Class)
      return Css.Style_Sheet;

   procedure Set_Style_Sheet
     (Widget      : in out Rho_Widget_Record'Class;
      Style_Sheet : Css.Style_Sheet);

   function Layout_Rectangle
     (Widget : Rho_Widget_Record'Class)
      return Rho.Rectangle.Rho_Rectangle;

   type Signal_Handler is access
     function (Widget     : not null access Rho_Widget_Record'Class;
               Event_Data : Rho.Toolkit.Events.Signal_Data_Interface'Class;
               User_Data  : Rho.Toolkit.Events.User_Data)
               return Rho.Toolkit.Events.Event_Response;

   procedure Add_Handler
     (Widget      : in out Rho_Widget_Record'Class;
      Signal      : Rho.Toolkit.Signals.Signal_Type;
      Handler     : Signal_Handler;
      User_Data   : access Rho.Toolkit.Events.User_Data_Interface'Class;
      After       : Boolean);

   procedure Draw
     (Widget : in out Rho_Widget_Record'Class;
      Region : Rho.Rectangle.Rho_Rectangle);

   procedure After_Resize
     (Widget : in out Rho_Widget_Record'Class);

   procedure Emit
     (Widget : in out Rho_Widget_Record'Class;
      Signal : Rho.Toolkit.Signals.Signal_Type;
      Data   : Rho.Toolkit.Events.Signal_Data_Interface'Class);

   function Draw_Surface
     (Widget : Rho_Widget_Record'Class)
      return Cairo.Cairo_Surface;

   type Root_Signal_Handler is abstract tagged private;

   function Handle
     (Handler    : Root_Signal_Handler;
      Widget     : not null access Rho_Widget_Record'Class;
      Event_Data : Rho.Toolkit.Events.Signal_Data_Interface'Class)
      return Rho.Toolkit.Events.Event_Response
      is abstract;

   procedure Add_Handler
     (Widget      : in out Rho_Widget_Record'Class;
      Signal      : Rho.Toolkit.Signals.Signal_Type;
      Handler     : Root_Signal_Handler'Class;
      After       : Boolean);

   type Configure_Handler is access
     function (Widget : not null access Rho_Widget_Record'Class;
               Width, Height : Rho.Non_Negative_Float)
               return Rho.Toolkit.Events.Event_Response;

   procedure On_Configure
     (Widget  : in out Rho_Widget_Record'Class;
      Handler : Configure_Handler;
      After   : Boolean := False);

   type Draw_Handler is access
     function (Widget  : not null access Rho_Widget_Record'Class;
               Context : Cairo.Cairo_Context)
               return Rho.Toolkit.Events.Event_Response;

   procedure On_Draw
     (Widget  : in out Rho_Widget_Record'Class;
      Handler : Draw_Handler;
      After   : Boolean := True);

   type User_Draw_Handler is access
     function (Widget    : not null access Rho_Widget_Record'Class;
               Context   : Cairo.Cairo_Context;
               User_Data : Rho.Toolkit.Events.User_Data)
               return Rho.Toolkit.Events.Event_Response;

   procedure On_Draw
     (Widget    : in out Rho_Widget_Record'Class;
      Handler   : User_Draw_Handler;
      User_Data : access Rho.Toolkit.Events.User_Data_Interface'Class;
      After     : Boolean := True);

   type Key_Handler is access
     function (Widget : not null access Rho_Widget_Record'Class;
               Key    : Rho.Keyboard.Rho_Key_Code)
               return Rho.Toolkit.Events.Event_Response;

   procedure On_Key_Press
     (Widget  : in out Rho_Widget_Record'Class;
      Handler : Key_Handler;
      After   : Boolean := False);

   type User_Key_Handler is access
     function (Widget    : not null access Rho_Widget_Record'Class;
               Key       : Rho.Keyboard.Rho_Key_Code;
               User_Data : Rho.Toolkit.Events.User_Data)
               return Rho.Toolkit.Events.Event_Response;

   procedure On_Key_Press
     (Widget    : in out Rho_Widget_Record'Class;
      Handler   : User_Key_Handler;
      User_Data : access Rho.Toolkit.Events.User_Data_Interface'Class;
      After     : Boolean := False);

   type Button_Handler is access
     function (Widget : not null access Rho_Widget_Record'Class;
               X, Y   : Rho_Float;
               Button : Rho.Mouse.Mouse_Button;
               Mask   : Rho.Keyboard.Control_Mask)
               return Rho.Toolkit.Events.Event_Response;

   procedure On_Button_Press
     (Widget  : in out Rho_Widget_Record'Class;
      Handler   : Button_Handler;
      After     : Boolean := False);

   type User_Button_Handler is access
     function (Widget    : not null access Rho_Widget_Record'Class;
               X, Y      : Rho_Float;
               Button    : Rho.Mouse.Mouse_Button;
               Mask      : Rho.Keyboard.Control_Mask;
               User_Data : Rho.Toolkit.Events.User_Data)
               return Rho.Toolkit.Events.Event_Response;

   procedure On_Button_Press
     (Widget    : in out Rho_Widget_Record'Class;
      Handler   : User_Button_Handler;
      User_Data : access Rho.Toolkit.Events.User_Data_Interface'Class;
      After     : Boolean := False);

private

   package Style_Handler_Map is
     new WL.String_Maps (Style_Handler);

   package Nullable_Rho_Floats is
     new WL.Nullable (Rho_Float);

   type Root_Signal_Handler is abstract tagged
      record
         User_Data    : Rho.Toolkit.Events.User_Data;
      end record;

   package List_Of_Signal_Handlers is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Root_Signal_Handler'Class);

   package Signal_Maps is
      new Rho.Toolkit.Signals.Maps
       (Element_Type    => List_Of_Signal_Handlers.List,
        "="             => List_Of_Signal_Handlers."=");

   type Cached_Widget_Style_Record is
      record
         Font : Rho.Font.Rho_Font;
      end record;

   type Cached_Widget_Style_Access is access Cached_Widget_Style_Record;

   type Rho_Widget_Record is
     abstract new Ada.Finalization.Controlled
     and Css.Layout_Interface
     and Css.Css_Element_Interface
     and Rho.Toolkit.Buildable.Rho_Buildable_Interface with
      record
         Widget_Id          : Ada.Strings.Unbounded.Unbounded_String;
         Tag                : Ada.Strings.Unbounded.Unbounded_String;
         Inline_Style       : Css.Css_Rule;
         Visible            : Boolean := False;
         Invalidated        : Boolean := True;
         Needs_Resize       : Boolean := False;
         Styles_Loaded      : Boolean := False;
         Styles             : Css.Css_Style_Map;
         Layout_Position    : Css.Layout_Position := (0.0, 0.0);
         Layout_Size        : Css.Layout_Size;
         Contents_Size      : Css.Layout_Size;
         Screen_Width       : Rho.Non_Negative_Float;
         Screen_Height      : Rho.Non_Negative_Float;
         Cached_Styles      : Cached_Widget_Style_Access;
         Parent             : Rho_Widget;
         Child_Index        : Natural := 0;
         Local_Style_Sheet  : Css.Style_Sheet;
         Style_Handlers     : Style_Handler_Map.Map;
         Css_Classes        : Ada.Strings.Unbounded.Unbounded_String;
         Signal_Handlers    : Signal_Maps.Map;
         Surface            : Cairo.Cairo_Surface := Cairo.Null_Surface;
      end record;

   procedure Key_Press
     (Widget  : in out Rho_Widget_Record'Class;
      Key     : Rho.Keyboard.Rho_Key_Code;
      Control : Rho.Keyboard.Control_Mask);

   procedure Key_Release
     (Widget  : in out Rho_Widget_Record'Class;
      Key     : Rho.Keyboard.Rho_Key_Code;
      Control : Rho.Keyboard.Control_Mask);

   overriding function Tag
     (Widget : Rho_Widget_Record)
      return String
   is (Ada.Strings.Unbounded.To_String (Widget.Tag));

   overriding function Inline_Style_Rules
     (Widget : Rho_Widget_Record)
      return Css.Css_Rule
   is (Widget.Inline_Style);

   overriding function Get_Layout_Position
     (Widget : Rho_Widget_Record)
      return Css.Layout_Position
   is (Widget.Layout_Position);

   overriding function Get_Layout_Size
     (Widget : Rho_Widget_Record)
      return Css.Layout_Size
   is (Widget.Layout_Size);

   overriding function Contents_Layout_Size
     (Widget : Rho_Widget_Record)
      return Css.Layout_Size
   is (Widget.Contents_Size);

   function Configure_Widget
     (Widget : not null access Rho_Widget_Record'Class;
      Width, Height : Rho.Non_Negative_Float)
      return Rho.Toolkit.Events.Event_Response;

   function Draw_Widget
     (Widget : not null access Rho_Widget_Record'Class;
      Cr     : Cairo.Cairo_Context)
      return Rho.Toolkit.Events.Event_Response;

   procedure Draw
     (Widget  : in out Rho_Widget_Record'Class;
      Context : in     Cairo.Cairo_Context);

   procedure Draw_Image
     (Widget    : in out Rho_Widget_Record'Class;
      Context   : Cairo.Cairo_Context;
      Rectangle : Rho.Rectangle.Rho_Rectangle;
      Image     : Css.Css_Element_Value;
      Radius    : Natural);

   function Has_Background_Color
     (Widget : Rho_Widget_Record)
      return Boolean
   is (Widget.Has_Style ("background-color")
       or else Widget.Has_Style ("background"));

   overriding function Child_Index
     (Widget : Rho_Widget_Record)
      return Natural
   is (Widget.Child_Index);

   function Draw_Surface
     (Widget : Rho_Widget_Record'Class)
      return Cairo.Cairo_Surface
   is (Widget.Surface);

end Rho.Toolkit.Widget;
