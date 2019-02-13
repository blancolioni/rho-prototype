with Rho.Event;
with Rho.Object;
with Rho.Renderable;

package Rho.Widget is

   type Rho.Toolkit_Widget_Record is
     abstract new Rho.Object.Rho_Object_Record
     and Rho.Renderable.Rho_Renderable
     with private;

   procedure Set_Position
     (Widget   : in out Rho.Toolkit_Widget_Record;
      X, Y     : in     Integer);

   procedure Set_Size_Request
     (Widget : in out Rho.Toolkit_Widget_Record;
      Width  : in     Positive;
      Height : in     Positive);

   procedure Set_Size
     (Widget : in out Rho.Toolkit_Widget_Record;
      Width  : in     Positive;
      Height : in     Positive);

   procedure Set_Event_Mask
     (Widget  : in out Rho.Toolkit_Widget_Record;
      Mask    : in     Rho.Event.Rho_Event_Mask);

   procedure Set_Label
     (Widget  : in out Rho.Toolkit_Widget_Record;
      Label   : in     String)
   is null;

   function Label
     (Widget  : Rho.Toolkit_Widget_Record)
     return String;

   procedure Show
     (Widget  : not null access Rho.Toolkit_Widget_Record);

   overriding
   procedure Render
     (Widget  : Rho.Toolkit_Widget_Record)
     is null;

   procedure Queue_Draw
     (Widget : in out Rho.Toolkit_Widget_Record);

   function Get_Width
     (Widget : Rho.Toolkit_Widget_Record)
      return Positive;

   function Get_Height
     (Widget : Rho.Toolkit_Widget_Record)
      return Positive;

   function Can_Resize (Widget : Rho.Toolkit_Widget_Record)
                       return Boolean
      is abstract;

   procedure On_Resize (Widget   : in out Rho.Toolkit_Widget_Record;
                        Width    : in     Positive;
                        Height   : in     Positive)
   is null;

   procedure On_Mouse_Down (Widget    : in out Rho.Toolkit_Widget_Record;
                            Button    : in     Rho.Event.Rho.Toolkit_Button;
                            Modifiers : in     Rho.Event.Rho_Key_Modifiers;
                            X, Y      : in     Natural)
   is null;

   procedure On_Mouse_Up (Widget    : in out Rho.Toolkit_Widget_Record;
                          Button    : in     Rho.Event.Rho.Toolkit_Button;
                          Modifiers : in     Rho.Event.Rho_Key_Modifiers;
                          X, Y      : in     Natural)
   is null;

   procedure On_Mouse_Drag (Widget    : in out Rho.Toolkit_Widget_Record;
                            X, Y      : in     Natural)
   is null;

   procedure On_Key_Down (Widget    : in out Rho.Toolkit_Widget_Record;
                          Key       : in     Rho.Event.Rho_Key_Type;
                          Modifiers : in     Rho.Event.Rho_Key_Modifiers;
                          X, Y      : in     Integer)
   is null;

   procedure On_Key_Up (Widget    : in out Rho.Toolkit_Widget_Record;
                        Key       : in     Rho.Event.Rho_Key_Type;
                        Modifiers : in     Rho.Event.Rho_Key_Modifiers;
                        X, Y      : in     Integer)
   is null;

   procedure On_Key_Press (Widget    : in out Rho.Toolkit_Widget_Record;
                           Key       : in     Rho.Event.Rho_Key_Type;
                           Modifiers : in     Rho.Event.Rho_Key_Modifiers;
                           X, Y      : in     Integer)
   is null;

   procedure On_Close (Widget    : in out Rho.Toolkit_Widget_Record)
   is null;

   type Rho.Toolkit_Widget is access all Rho.Toolkit_Widget_Record'Class;

private

   type Widget_Events is array (Rho.Event.Rho_Event_Class) of Boolean;

   type Rho.Toolkit_Widget_Record is
     abstract new Rho.Object.Rho_Object_Record
     and Rho.Renderable.Rho_Renderable with
      record
         X, Y        : Integer;
         Width       : Positive;
         Height      : Positive;
         Event_Mask  : Widget_Events;
      end record;

end Rho.Widget;
