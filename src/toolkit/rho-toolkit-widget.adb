with Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Directories;
with Ada.Numerics;
with Ada.Unchecked_Deallocation;

with Glib;

with Cairo.Image_Surface;
with Cairo.Pattern;
--  with Cairo.Png;

with Rho.Elementary_Functions;

with Rho.Toolkit.Errors;

with Css.Images;
with Css.Parser;

package body Rho.Toolkit.Widget is

   type Default_Handler_Record is
     new Root_Signal_Handler with
      record
         Handler_Function      : Signal_Handler;
      end record;

   overriding function Handle
     (Handler    : Default_Handler_Record;
      Widget     : not null access Rho_Widget_Record'Class;
      Event_Data : Rho.Toolkit.Events.Signal_Data_Interface'Class)
      return Rho.Toolkit.Events.Event_Response
   is (Handler.Handler_Function
       (Widget, Event_Data, Handler.User_Data));

   type Configure_Handler_Record is
     new Root_Signal_Handler with
      record
         Handler_Function      : Configure_Handler;
      end record;

   overriding function Handle
     (Handler    : Configure_Handler_Record;
      Widget     : not null access Rho_Widget_Record'Class;
      Event_Data : Rho.Toolkit.Events.Signal_Data_Interface'Class)
      return Rho.Toolkit.Events.Event_Response;

   type Draw_Handler_Record is
     new Root_Signal_Handler with
      record
         Handler_Function      : Draw_Handler;
         User_Handler_Function : User_Draw_Handler;
      end record;

   overriding function Handle
     (Handler    : Draw_Handler_Record;
      Widget     : not null access Rho_Widget_Record'Class;
      Event_Data : Rho.Toolkit.Events.Signal_Data_Interface'Class)
      return Rho.Toolkit.Events.Event_Response;

   type Key_Handler_Record is
     new Root_Signal_Handler with
      record
         Handler_Function      : Key_Handler;
         User_Handler_Function : User_Key_Handler;
      end record;

   overriding function Handle
     (Handler    : Key_Handler_Record;
      Widget     : not null access Rho_Widget_Record'Class;
      Event_Data : Rho.Toolkit.Events.Signal_Data_Interface'Class)
      return Rho.Toolkit.Events.Event_Response
   is (if Handler.User_Handler_Function /= null
       then Handler.User_Handler_Function
         (Widget, Rho.Toolkit.Events.Key_Event_Data (Event_Data).Key,
          Handler.User_Data)
       else Handler.Handler_Function
         (Widget, Rho.Toolkit.Events.Key_Event_Data (Event_Data).Key));

   type Button_Handler_Record is
     new Root_Signal_Handler with
      record
         Handler_Function      : Button_Handler;
         User_Handler_Function : User_Button_Handler;
      end record;

   overriding function Handle
     (Handler    : Button_Handler_Record;
      Widget     : not null access Rho_Widget_Record'Class;
      Event_Data : Rho.Toolkit.Events.Signal_Data_Interface'Class)
      return Rho.Toolkit.Events.Event_Response;

   Default_Font : Rho.Font.Rho_Font;

   function To_Lower (S : String) return String
                      renames Ada.Characters.Handling.To_Lower;

   function Widget_State_Name
     (State : Widget_State)
      return String
   is ((case State is
           when Active       => "active",
           when Hover        => "hover",
           when Insensitive  => "insensitive",
           when Selected     => "selected",
           when Focused      => "focused",
           when Inconsistent => "inconsistent"))
   with Unreferenced;

   function To_Rho_Color
     (Value : Css.Css_Element_Value)
      return Rho.Color.Rho_Color;

   function Pixels
     (Spec : String)
      return Natural
     with Unreferenced;

   procedure Specify_X
     (Left, Right   : Rho_Float;
      Specification : String;
      X             : in out Rho_Float);

   procedure Specify_Y
     (Top, Bottom   : Rho_Float;
      Specification : String;
      Y             : in out Rho_Float);

   type Color_Stop_Record is
      record
         Position : Rho.Unit_Float;
         Color    : Rho.Color.Rho_Color;
      end record;

   function Get_Color_Stop
     (Widget  : Rho_Widget_Record'Class;
      Value   : Css.Css_Element_Value;
      Default : Rho_Float;
      Length  : Rho_Float)
      return Color_Stop_Record;

   function Get_Gradient_Direction
     (Value     : Css.Css_Element_Value;
      Rectangle : Rho.Rectangle.Rho_Rectangle)
      return Rho.Rectangle.Rho_Rectangle;

   package Color_Stop_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Color_Stop_Record);

   type Gradient_Spec is
      record
         Rectangle   : Rho.Rectangle.Rho_Rectangle;
         Color_Stops : Color_Stop_Lists.List;
      end record;

   procedure Draw_Gradient
     (Context  : Cairo.Cairo_Context;
      Region   : Rho.Rectangle.Rho_Rectangle;
      Gradient : Gradient_Spec;
      Radius   : Natural);

   procedure Draw_Rounded_Rectangle
     (Context   : Cairo.Cairo_Context;
      Rectangle : Rho.Rectangle.Rho_Rectangle;
      Radius    : Natural);

   procedure Check_Font (Widget : Rho_Widget_Record);

   package Font_Slant_Maps is
     new WL.String_Maps (Rho.Font.Rho_Font_Slant, Rho.Font."=");

   package Font_Weight_Maps is
     new WL.String_Maps (Rho.Font.Rho_Font_Weight, Rho.Font."=");

   Font_Slant_Map : Font_Slant_Maps.Map;
   Font_Weight_Map : Font_Weight_Maps.Map;

   -----------------
   -- Add_Handler --
   -----------------

   procedure Add_Handler
     (Widget      : in out Rho_Widget_Record'Class;
      Signal      : Rho.Toolkit.Signals.Signal_Type;
      Handler     : Root_Signal_Handler'Class;
      After       : Boolean)
   is
   begin
      if not Widget.Signal_Handlers.Contains (Signal) then
         Widget.Signal_Handlers.Insert
           (Signal, List_Of_Signal_Handlers.Empty_List);
      end if;

      declare
         List : List_Of_Signal_Handlers.List renames
                  Widget.Signal_Handlers (Signal);
      begin
         if After then
            List.Append (Handler);
         else
            List.Insert (List.First, Handler);
         end if;
      end;
   end Add_Handler;

   -----------------
   -- Add_Handler --
   -----------------

   procedure Add_Handler
     (Widget      : in out Rho_Widget_Record'Class;
      Signal      : Rho.Toolkit.Signals.Signal_Type;
      Handler     : Signal_Handler;
      User_Data   : access Rho.Toolkit.Events.User_Data_Interface'Class;
      After       : Boolean)
   is
   begin
      Add_Handler
        (Widget  => Widget,
         Signal  => Signal,
         Handler =>
           Default_Handler_Record'
             (User_Data        => User_Data,
              Handler_Function => Handler),
         After   => After);
   end Add_Handler;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Widget : in out Rho_Widget_Record) is
   begin
      if Widget.Cached_Styles /= null then
         Widget.Cached_Styles :=
           new Cached_Widget_Style_Record'(Widget.Cached_Styles.all);
      end if;
      Widget.Surface := Cairo.Null_Surface;
      Widget.Invalidated := True;
      Widget.Screen_Width := 0.0;
      Widget.Screen_Height := 0.0;
   end Adjust;

   ------------------
   -- After_Resize --
   ------------------

   procedure After_Resize
     (Widget : in out Rho_Widget_Record'Class)
   is
   begin
      Widget.Needs_Resize := False;
      for Child of Widget.Child_Elements loop
         Rho_Widget (Child).After_Resize;
      end loop;
   end After_Resize;

   ----------------------
   -- Background_Color --
   ----------------------

   function Background_Color
     (Widget : Rho_Widget_Record)
      return Rho.Color.Rho_Color
   is
      Background : constant Css.Css_Element_Value :=
                     (if Widget.Has_Style ("background-color")
                      then Widget.Style ("background-color")
                      else Widget.Style ("background"));
   begin
      return To_Rho_Color (Background);
   end Background_Color;

   ----------------
   -- Check_Font --
   ----------------

   procedure Check_Font (Widget : Rho_Widget_Record) is
      use type Rho.Font.Rho_Font;

      function To_Slant
        (Font_Style_Name : String)
         return Rho.Font.Rho_Font_Slant;

      function To_Weight
        (Font_Weight_Name : String)
         return Rho.Font.Rho_Font_Weight;

      --------------
      -- To_Slant --
      --------------

      function To_Slant
        (Font_Style_Name : String)
         return Rho.Font.Rho_Font_Slant
      is
      begin
         if Font_Slant_Map.Is_Empty then
            Font_Slant_Map.Insert ("normal", Rho.Font.Normal);
            Font_Slant_Map.Insert ("italic", Rho.Font.Italic);
            Font_Slant_Map.Insert ("oblique", Rho.Font.Oblique);
         end if;
         if Font_Slant_Map.Contains (Font_Style_Name) then
            return Font_Slant_Map.Element (Font_Style_Name);
         else
            return Rho.Font.Normal;
         end if;
      end To_Slant;

      ---------------
      -- To_Weight --
      ---------------

      function To_Weight
        (Font_Weight_Name : String)
         return Rho.Font.Rho_Font_Weight
      is
      begin
         if Font_Weight_Map.Is_Empty then
            Font_Weight_Map.Insert ("normal", Rho.Font.Normal);
            Font_Weight_Map.Insert ("bold", Rho.Font.Bold);
         end if;
         if Font_Weight_Map.Contains (Font_Weight_Name) then
            return Font_Weight_Map.Element (Font_Weight_Name);
         else
            return Rho.Font.Normal;
         end if;
      end To_Weight;

   begin
      if Widget.Cached_Styles.Font = null
        and then Widget.Has_Style ("font-family")
      then
         declare
            Font_Name : constant String :=
                          Widget.Style_To_String ("font-family");
            Font_Size : constant Float :=
                          (if Widget.Has_Style ("font-size")
                           then Widget.Measure
                             (Widget.Style ("font-size"),
                              Css.Left)
                           else 10.0);
            Font_Slant : constant Rho.Font.Rho_Font_Slant :=
                           (if Widget.Has_Style ("font-style")
                            then To_Slant
                              (Widget.Style_To_String ("font-style"))
                            else Rho.Font.Normal);
            Font_Weight : constant Rho.Font.Rho_Font_Weight :=
                            (if Widget.Has_Style ("font-weight")
                             then To_Weight
                               (Widget.Style_To_String ("font-weight"))
                             else Rho.Font.Normal);
         begin
            Widget.Cached_Styles.Font :=
              Rho.Font.Get_Font
                (Font_Name, Rho.Font.Rho_Font_Size (Font_Size),
                 Font_Slant, Font_Weight);
         end;
      end if;
   end Check_Font;

   -----------
   -- Child --
   -----------

   function Child
     (Widget : Rho_Widget_Record;
      Index  : Positive)
      return Rho.Toolkit.Widget.Rho_Widget
   is
      pragma Unreferenced (Index);
   begin
      return (raise Constraint_Error with
                Rho_Widget_Record'Class (Widget).Id
              & " does not have children");
   end Child;

   -------------
   -- Classes --
   -------------

   overriding function Classes
     (Widget : Rho_Widget_Record)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Widget.Css_Classes);
   end Classes;

   ----------------------
   -- Configure_Widget --
   ----------------------

   function Configure_Widget
     (Widget        : not null access Rho_Widget_Record'Class;
      Width, Height : Rho.Non_Negative_Float)
      return Rho.Toolkit.Events.Event_Response
   is
      use type Cairo.Cairo_Surface;
   begin
      if Width /= Widget.Screen_Width
        or else Height /= Widget.Screen_Height
        or else Widget.Surface = Cairo.Null_Surface
      then
         if Widget.Surface /= Cairo.Null_Surface then
            Cairo.Surface_Destroy (Widget.Surface);
         end if;
         Widget.Surface :=
           Cairo.Image_Surface.Create
             (Format => Cairo.Image_Surface.Cairo_Format_ARGB32,
              Width  => Glib.Gint (Width),
              Height => Glib.Gint (Height));
         Widget.Screen_Width := Width;
         Widget.Screen_Height := Height;
         Widget.Queue_Draw;
      end if;

      return Rho.Toolkit.Events.Propagate_Event;

   end Configure_Widget;

   ------------
   -- Create --
   ------------

   procedure Create
     (Widget     : in out Rho_Widget_Record;
      Element_Id : String)
   is
   begin
      Widget.Set_Id (Element_Id);
   end Create;

   ------------------
   -- Create_Style --
   ------------------

   overriding procedure Create_Style
     (Widget : in out Rho_Widget_Record;
      Name   : String)
   is
   begin
      Widget.Styles.Create_Style (Name);
   end Create_Style;

   -------------------
   -- Current_State --
   -------------------

   function Current_State
     (Widget : Rho_Widget_Record)
      return Widget_State
   is (Active);

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Widget : in out Rho_Widget) is null;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Widget : in out Rho_Widget_Record'Class;
      Region : Rho.Rectangle.Rho_Rectangle)
   is
      Context : constant Cairo.Cairo_Context :=
                  Cairo.Create (Widget.Surface);
   begin

      Cairo.Save (Context);
      Cairo.Set_Operator (Context, Cairo.Cairo_Operator_Clear);
      Cairo.Paint (Context);
      Cairo.Restore (Context);

      Widget.Emit
        (Signal => Rho.Toolkit.Signals.Signal_Draw_Event,
         Data   =>
           Rho.Toolkit.Events.Draw_Event_Data'
             (Context => Context,
              Region  => Region));

      Widget.Invalidated := False;

   end Draw;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Widget  : in out Rho_Widget_Record'Class;
      Context : in     Cairo.Cairo_Context)
   is
      use Glib;
      Border_Radius : Float := 0.0;
      Border_Style  : constant String :=
                        (if Widget.Has_Style ("border-style")
                         then Css.To_String (Widget.Style ("border-style"))
                         else "none");
      Border_Width : constant Float :=
                       (if Widget.Has_Style ("border-width")
                        then Widget.Measure (Widget.Style ("border-width"),
                          Css.Left)
                        else 0.0);
      Region : constant Rho.Rectangle.Rho_Rectangle :=
                 (0.0, 0.0, Widget.Screen_Width, Widget.Screen_Height);
   begin

      Widget.Invalidated := False;

      if Widget.Screen_Width = 0.0 or else Widget.Screen_Height = 0.0 then
         return;
      end if;

      if Widget.Has_Style ("border-radius") then
         Border_Radius :=
           Widget.Measure (Widget.Style ("border-radius"),
                           Css.Left);
      end if;

      if Widget.Has_Style ("background-image") then
         Widget.Draw_Image
           (Context, Region,
            Widget.Style ("background-image"),
            Integer (Border_Radius));
      elsif Widget.Has_Background_Color then
         Set_Color
           (Context, Widget.Background_Color);
         if Border_Radius > 0.0 then
            Draw_Rounded_Rectangle
              (Context,
               (Region.X, Region.Y, Region.Width, Region.Height),
               Natural (Border_Radius));
         else
            Cairo.Rectangle
              (Cr     => Context,
               X      => Glib.Gdouble (Region.X),
               Y      => Glib.Gdouble (Region.Y),
               Width  => Glib.Gdouble (Region.Width),
               Height => Glib.Gdouble (Region.Height));
         end if;
         Cairo.Fill (Context);
      end if;

      if Border_Style /= "none"
        and then Border_Width > 0.0
      then
         if Widget.Has_Style ("border-color") then
            Set_Color
              (Context,
               To_Rho_Color (Widget.Style ("border-color")));
         else
            Set_Color (Context, (0.0, 0.0, 0.0, 1.0));
         end if;

         Cairo.Set_Line_Width (Context, Gdouble (Border_Width));

         if Border_Radius = 0.0 then
            Cairo.Rectangle
              (Context,
               Gdouble (Region.X),
               Gdouble (Region.Y),
               Gdouble (Region.Width),
               Gdouble (Region.Height));
         else
            Draw_Rounded_Rectangle
              (Context,
               (Region.X,
                Region.Y,
                Region.Width,
                Region.Height),
               Natural (Border_Radius));
         end if;
         Cairo.Stroke (Context);
      end if;

   end Draw;

   -------------------
   -- Draw_Gradient --
   -------------------

   procedure Draw_Gradient
     (Context  : Cairo.Cairo_Context;
      Region   : Rho.Rectangle.Rho_Rectangle;
      Gradient : Gradient_Spec;
      Radius   : Natural)
   is
      use Glib;
      X1 : constant Rho_Float := Gradient.Rectangle.X;
      Y1 : constant Rho_Float := Gradient.Rectangle.Y;
      X2 : constant Rho_Float :=
             Gradient.Rectangle.X + abs Gradient.Rectangle.Width;
      Y2 : constant Rho_Float :=
             Gradient.Rectangle.Y + abs Gradient.Rectangle.Height;

      Pattern : constant Cairo.Cairo_Pattern :=
                  Cairo.Pattern.Create_Linear
                    (Gdouble (X1), Gdouble (Y1),
                     Gdouble (X2), Gdouble (Y2));
   begin
      if Gradient.Rectangle.Width < 0.0
        xor Gradient.Rectangle.Height < 0.0
      then
         for Stop of Gradient.Color_Stops loop
            Cairo.Pattern.Add_Color_Stop_Rgba
              (Pattern, Gdouble (1.0 - Stop.Position),
               Gdouble (Stop.Color.Red),
               Gdouble (Stop.Color.Green),
               Gdouble (Stop.Color.Blue),
               Gdouble (Stop.Color.Alpha));
         end loop;
      else
         for Stop of Gradient.Color_Stops loop
            Cairo.Pattern.Add_Color_Stop_Rgba
              (Pattern, Gdouble (Stop.Position),
               Gdouble (Stop.Color.Red),
               Gdouble (Stop.Color.Green),
               Gdouble (Stop.Color.Blue),
               Gdouble (Stop.Color.Alpha));
         end loop;
      end if;

      Cairo.Set_Source (Context, Pattern);
      if Radius = 0 then
         Cairo.Rectangle
           (Context,
            Gdouble (Region.X), Gdouble (Region.Y),
            Gdouble (abs Region.Width), Gdouble (abs Region.Height));
      else
         Draw_Rounded_Rectangle
           (Context, Region, Radius);
      end if;

      Cairo.Fill (Context);
      Cairo.Pattern_Destroy (Pattern);
   end Draw_Gradient;

   ----------------
   -- Draw_Image --
   ----------------

   procedure Draw_Image
     (Widget    : in out Rho_Widget_Record'Class;
      Context   : Cairo.Cairo_Context;
      Rectangle : Rho.Rectangle.Rho_Rectangle;
      Image     : Css.Css_Element_Value;
      Radius    : Natural)
   is
      use Css;
   begin
      if Is_String (Image) then
         if Ada.Directories.Exists
           (To_String (Image))
         then
            declare
               use Glib;
               Surface      : constant Cairo.Cairo_Surface :=
                                Css.Images.Get_Image_Surface
                                  (To_String (Image));
               Img_Height   : constant Gint :=
                                Cairo.Image_Surface.Get_Height (Surface);
               Img_Width    : constant Gint :=
                                Cairo.Image_Surface.Get_Width (Surface);
               Height_Ratio : constant Gdouble :=
                                Gdouble (Rectangle.Height)
                                / Gdouble (Img_Height);
               Width_Ratio  : constant Gdouble :=
                                Gdouble (Rectangle.Width)
                                / Gdouble (Img_Width);
            begin

               Cairo.Save (Context);
               Cairo.Translate
                 (Context,
                  Gdouble (Rectangle.X),
                  Gdouble (Rectangle.Y));
               Cairo.Scale (Context, Width_Ratio, Height_Ratio);
               Cairo.Set_Source_Surface (Context, Surface, 0.0, 0.0);
               Cairo.Paint (Context);
               Cairo.Restore (Context);
            end;
         end if;
      elsif Is_Function (Image) then
         if Function_Name (Image) = "-gtk-gradient" then
            declare
               Gradient_Type : constant String :=
                                 To_String (Argument (Image, 1));
               Start         : constant Css_Element_Value :=
                                 Argument (Image, 2);
               Stop          : constant Css_Element_Value :=
                                 Argument (Image, 3);
               From          : Rho.Color.Rho_Color := (0.0, 0.0, 0.0, 1.0);
               To            : Rho.Color.Rho_Color := (0.0, 0.0, 0.0, 1.0);
               X1            : Rho_Float := Rectangle.X;
               Y1            : Rho_Float := Rectangle.Y;
               X2            : Rho_Float :=
                                 Rectangle.X + Rectangle.Width;
               Y2            : Rho_Float :=
                                 Rectangle.Y + Rectangle.Height;
            begin
               if Gradient_Type /= "linear" then
                  Rho.Toolkit.Errors.Warning
                    ("gradient: only linear gradients supported");
               end if;

               for I in 4 .. Argument_Count (Image) loop
                  declare
                     Arg : constant Css_Element_Value := Argument (Image, I);
                  begin
                     if Is_Function (Arg) then
                        declare
                           Name : constant String := Function_Name (Arg);
                        begin
                           if Name = "from" then
                              From :=
                                To_Rho_Color
                                  (Widget.Evaluate (Argument (Arg, 1)));
                           elsif Name = "to" then
                              To :=
                                To_Rho_Color
                                  (Widget.Evaluate (Argument (Arg, 1)));
                           elsif Name = "stop" then
                              Rho.Toolkit.Errors.Warning
                                ("gradient: stop color is not supported");
                           else
                              Rho.Toolkit.Errors.Error
                                ("gradient: unknown argument type: " & Name);
                           end if;
                        end;
                     else
                        Rho.Toolkit.Errors.Error ("gradient: bad argument: "
                                          & To_String (Arg));
                     end if;
                  end;
               end loop;

               if Is_Function (Start) then
                  Specify_X (Rectangle.X, Rectangle.X + Rectangle.Width,
                             To_String (Argument (Start, 1)),
                             X1);
                  Specify_Y (Rectangle.Y, Rectangle.Y + Rectangle.Height,
                             To_String (Argument (Start, 2)),
                             Y1);
               else
                  Rho.Toolkit.Errors.Warning ("gradient: bad start position: "
                                      & To_String (Start));
               end if;

               if Is_Function (Stop) then
                  Specify_X (Rectangle.X, Rectangle.X + Rectangle.Width,
                             To_String (Argument (Stop, 1)),
                             X2);
                  Specify_Y (Rectangle.Y, Rectangle.Y + Rectangle.Height,
                             To_String (Argument (Stop, 2)),
                             Y2);
               else
                  Rho.Toolkit.Errors.Warning ("gradient: bad stop position: "
                                      & To_String (Stop));
               end if;

               declare
                  Gradient : Gradient_Spec;
               begin
                  Gradient.Rectangle := (X1, Y1, X2 - X1, Y2 - Y1);
                  Gradient.Color_Stops.Append
                    ((0.0, From));
                  Gradient.Color_Stops.Append
                    ((1.0, To));
                  Draw_Gradient (Context, Rectangle, Gradient, Radius);
               end;
            end;
         elsif Function_Name (Image) = "linear-gradient" then
            declare
               Gradient      : Gradient_Spec;
               From          : Color_Stop_Record;
               To            : Color_Stop_Record;
               Length        : Rho_Float := Rectangle.Height;
            begin

               if Argument_Count (Image) = 2 then
                  From :=
                    Get_Color_Stop
                      (Widget,
                       Widget.Evaluate (Argument (Image, 1)),
                       Default => 0.0, Length => Length);
                  To :=
                    Get_Color_Stop
                      (Widget,
                       Widget.Evaluate (Argument (Image, 2)),
                     Default => 1.0, Length => Length);
                  Gradient.Rectangle := Rectangle;
                  Gradient.Rectangle.Width := 0.0;
                  Gradient.Color_Stops.Append (From);
                  Gradient.Color_Stops.Append (To);
               else
                  Gradient.Rectangle :=
                    Get_Gradient_Direction
                      (Widget.Evaluate (Argument (Image, 1)),
                       Rectangle);
                  Length :=
                    Rho.Elementary_Functions.Sqrt
                      (Gradient.Rectangle.Width ** 2
                       + Gradient.Rectangle.Height ** 2);
                  for I in 2 .. Argument_Count (Image) loop
                     begin
                        Gradient.Color_Stops.Append
                          (Get_Color_Stop
                             (Widget,
                              Widget.Evaluate (Argument (Image, I)),
                              Default =>
                                Rho_Float (I - 2)
                              / Rho_Float (Argument_Count (Image) - 2),
                              Length  => Length));
                     exception
                        when others =>
                           Rho.Toolkit.Errors.Error
                             ("bad color stop: " &
                                To_String (Argument (Image, I)));
                     end;
                  end loop;
               end if;

               Draw_Gradient (Context, Rectangle, Gradient, Radius);
            end;
         end if;
      end if;

   end Draw_Image;

   ----------------------------
   -- Draw_Rounded_Rectangle --
   ----------------------------

   procedure Draw_Rounded_Rectangle
     (Context   : Cairo.Cairo_Context;
      Rectangle : Rho.Rectangle.Rho_Rectangle;
      Radius    : Natural)
   is
      use Glib;
      R  : constant Gdouble := Gdouble (Radius);
      Pi : constant := Ada.Numerics.Pi;
      X1 : constant Gdouble :=
             Gdouble (Rectangle.X) + R;
      X2 : constant Gdouble :=
             Gdouble (Rectangle.X + Rectangle.Width) - R;
      Y1 : constant Gdouble :=
             Gdouble (Rectangle.Y) + R;
      Y2 : constant Gdouble :=
             Gdouble (Rectangle.Y + Rectangle.Height) - R;
   begin
      Cairo.New_Path (Context);
      Cairo.Arc (Context, X1, Y1, R, 2.0 * Pi / 2.0, 3.0 * Pi / 2.0);
      Cairo.Arc (Context, X2, Y1, R, 3.0 * Pi / 2.0, 4.0 * Pi / 2.0);
      Cairo.Arc (Context, X2, Y2, R, 0.0 * Pi / 2.0, 1.0 * Pi / 2.0);
      Cairo.Arc (Context, X1, Y2, R, 1.0 * Pi / 2.0, 2.0 * Pi / 2.0);
      Cairo.Close_Path (Context);
   end Draw_Rounded_Rectangle;

   -----------------
   -- Draw_Widget --
   -----------------

   function Draw_Widget
     (Widget : not null access Rho_Widget_Record'Class;
      Cr     : Cairo.Cairo_Context)
      return Rho.Toolkit.Events.Event_Response
   is
   begin
      Widget.Draw (Cr);
      return Rho.Toolkit.Events.Propagate_Event;
   end Draw_Widget;

   ----------
   -- Emit --
   ----------

   procedure Emit
     (Widget : in out Rho_Widget_Record'Class;
      Signal : Rho.Toolkit.Signals.Signal_Type;
      Data   : Rho.Toolkit.Events.Signal_Data_Interface'Class)
   is
   begin
      if Widget.Signal_Handlers.Contains (Signal) then
         for Item of Widget.Signal_Handlers (Signal) loop
            declare
               Response : constant Rho.Toolkit.Events.Event_Response :=
                            Item.Handle
                              (Widget'Unchecked_Access, Data);
            begin
               case Response is
                  when Rho.Toolkit.Events.Propagate_Event =>
                     null;
                  when Rho.Toolkit.Events.Stop_Event =>
                     exit;
               end case;
            end;
         end loop;
      end if;
   end Emit;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Widget : in out Rho_Widget_Record) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Cached_Widget_Style_Record,
                                        Cached_Widget_Style_Access);
   begin
      if Widget.Cached_Styles /= null then
         Free (Widget.Cached_Styles);
         Widget.Cached_Styles := null;
      end if;
   end Finalize;

   ----------
   -- Font --
   ----------

   function Font
     (Widget : Rho_Widget_Record)
      return Rho.Font.Rho_Font
   is
      use type Rho.Font.Rho_Font;
   begin
      Check_Font (Widget);
      if Widget.Cached_Styles.Font = null then
         if Widget.Parent = null then
            if Default_Font = null then
               Default_Font := Rho.Font.Get_Font ("Courier New", 14.0);
            end if;
            return Default_Font;
         else
            return Font (Widget.Parent.all);
         end if;
      else
         return Widget.Cached_Styles.Font;
      end if;
   end Font;

   ----------------------
   -- Foreground_Color --
   ----------------------

   function Foreground_Color
     (Widget : Rho_Widget_Record)
      return Rho.Color.Rho_Color
   is
   begin
      if Widget.Has_Style ("foreground-color") then
         return To_Rho_Color (Widget.Style ("foreground-color"));
      elsif Widget.Has_Style ("foreground") then
         return To_Rho_Color (Widget.Style ("foreground"));
      elsif Widget.Has_Style ("color") then
         return To_Rho_Color (Widget.Style ("color"));
      else
         return (0.0, 0.0, 0.0, 1.0);
      end if;
   end Foreground_Color;

   ----------------------
   -- Get_Child_Widget --
   ----------------------

   function Get_Child_Widget
     (Widget : Rho_Widget_Record;
      X, Y   : Rho_Float)
      return Rho_Widget
   is
      pragma Unreferenced (Widget, X, Y);
   begin
      return null;
   end Get_Child_Widget;

   --------------------
   -- Get_Color_Stop --
   --------------------

   function Get_Color_Stop
     (Widget  : Rho_Widget_Record'Class;
      Value   : Css.Css_Element_Value;
      Default : Rho_Float;
      Length  : Rho_Float)
      return Color_Stop_Record
   is
      use Css;
   begin
      return Result : Color_Stop_Record do
         if Is_Tuple (Value) then
            Result.Color :=
              To_Rho_Color
                (Widget.Evaluate
                   (Argument (Value, 1)));
            Result.Position :=
              Rho_Float
                (Widget.Measure
                   (Argument (Value, 2), Float (Length)))
                / Length;

         else
            Result.Color := To_Rho_Color (Value);
            Result.Position := Default;
         end if;
      end return;
   end Get_Color_Stop;

   ----------------------------
   -- Get_Gradient_Direction --
   ----------------------------

   function Get_Gradient_Direction
     (Value     : Css.Css_Element_Value;
      Rectangle : Rho.Rectangle.Rho_Rectangle)
      return Rho.Rectangle.Rho_Rectangle
   is
      use Css;
      Result : Rho.Rectangle.Rho_Rectangle := Rectangle;
   begin
      if Is_Tuple (Value) then
         if To_String (Argument (Value, 1)) = "to" then
            declare
               Direction : constant String :=
                             To_String (Argument (Value, 2));
            begin
               if Direction = "right" then
                  Result.Height := 0.0;
               elsif Direction = "left" then
                  Result.Height := 0.0;
                  Result.Width := -Result.Width;
               elsif Direction = "bottom" then
                  Result.Width := 0.0;
               elsif Direction = "top" then
                  Result.Width := 0.0;
                  Result.Height := -Result.Height;
               else
                  Rho.Toolkit.Errors.Warning ("cannot understand direction: "
                                      & To_String (Value));
               end if;
            end;
         else
            Rho.Toolkit.Errors.Warning ("cannot understand direction: "
                                & To_String (Value));
         end if;
      elsif Is_String (Value) then
         declare
            Direction : constant String := To_String (Value);
         begin
            if Direction = "right" then
               Result.Height := 0.0;
            elsif Direction = "left" then
               Result.Height := 0.0;
               Result.X := Result.X + Result.Width;
               Result.Width := -Result.Width;
            elsif Direction = "down" then
               Result.Width := 0.0;
            elsif Direction = "up" then
               Result.Width := 0.0;
               Result.Y := Result.Y + Result.Width;
               Result.Height := -Result.Height;
            else
               Rho.Toolkit.Errors.Warning ("cannot understand direction: "
                                   & To_String (Value));
            end if;
         end;
      else
         Rho.Toolkit.Errors.Warning ("cannot understand direction: "
                             & To_String (Value));
      end if;
      return Result;
   end Get_Gradient_Direction;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Handler    : Button_Handler_Record;
      Widget     : not null access Rho_Widget_Record'Class;
      Event_Data : Rho.Toolkit.Events.Signal_Data_Interface'Class)
      return Rho.Toolkit.Events.Event_Response
   is
      Button_Event : Rho.Toolkit.Events.Button_Event_Data'Class renames
                       Rho.Toolkit.Events.Button_Event_Data'Class
                         (Event_Data);
   begin
      if Handler.User_Handler_Function /= null then
         return Handler.User_Handler_Function
           (Widget, Button_Event.X, Button_Event.Y,
            Button_Event.Button, Button_Event.Control,
            Handler.User_Data);
      else
         return Handler.Handler_Function
           (Widget, Button_Event.X, Button_Event.Y,
            Button_Event.Button, Button_Event.Control);
      end if;
   end Handle;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Handler    : Configure_Handler_Record;
      Widget     : not null access Rho_Widget_Record'Class;
      Event_Data : Rho.Toolkit.Events.Signal_Data_Interface'Class)
      return Rho.Toolkit.Events.Event_Response
   is
      Configure_Event : Rho.Toolkit.Events.Configure_Event_Data'Class renames
                          Rho.Toolkit.Events.Configure_Event_Data'Class
                            (Event_Data);
   begin
      return Handler.Handler_Function
        (Widget, Configure_Event.Width, Configure_Event.Height);
   end Handle;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Handler    : Draw_Handler_Record;
      Widget     : not null access Rho_Widget_Record'Class;
      Event_Data : Rho.Toolkit.Events.Signal_Data_Interface'Class)
      return Rho.Toolkit.Events.Event_Response
   is
      Draw_Event : Rho.Toolkit.Events.Draw_Event_Data'Class renames
                     Rho.Toolkit.Events.Draw_Event_Data'Class
                       (Event_Data);
      pragma Unreferenced (Draw_Event);
      Cr         : constant Cairo.Cairo_Context :=
                     Cairo.Create (Widget.Surface);
   begin
      return Response : constant Rho.Toolkit.Events.Event_Response :=
        (if Handler.User_Handler_Function /= null
         then Handler.User_Handler_Function (Widget, Cr, Handler.User_Data)
         else Handler.Handler_Function (Widget, Cr))
      do
         Cairo.Destroy (Cr);
      end return;
   end Handle;

   ----------
   -- Hide --
   ----------

   procedure Hide
     (Widget : in out Rho_Widget_Record)
   is
   begin
      Widget.Visible := False;
   end Hide;

   --------
   -- Id --
   --------

   overriding function Id
     (Widget : Rho_Widget_Record)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Widget.Widget_Id);
   end Id;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Widget : in out Rho_Widget_Record) is
   begin
      Widget.Cached_Styles := new Cached_Widget_Style_Record;
      Widget.On_Configure
        (Configure_Widget'Access);
      Widget.On_Draw
        (Draw_Widget'Access);
   end Initialize;

   -----------------------
   -- Initialize_Styles --
   -----------------------

   procedure Initialize_Styles (Widget : in out Rho_Widget_Record) is null;

   -----------------------
   -- Invalidate_Region --
   -----------------------

   procedure Invalidate_Region
     (Widget : in out Rho_Widget_Record;
      Region : Rho.Rectangle.Rho_Rectangle)
   is
   begin
      Widget.Invalidated := True;
      if Widget.Parent /= null then
         Widget.Parent.Invalidate_Region
           ((Region.X + Rho_Float (Widget.Get_Layout_Position.X),
            Region.Y + Rho_Float (Widget.Get_Layout_Position.Y),
            Region.Width, Region.Height));
      end if;
   end Invalidate_Region;

   ---------------
   -- Key_Press --
   ---------------

   procedure Key_Press
     (Widget  : in out Rho_Widget_Record'Class;
      Key     : Rho.Keyboard.Rho_Key_Code;
      Control : Rho.Keyboard.Control_Mask)
   is
      Data : constant Rho.Toolkit.Events.Key_Event_Data := (Key, Control);
   begin
      Widget.Emit (Rho.Toolkit.Signals.Signal_Key_Press_Event, Data);
   end Key_Press;

   -----------------
   -- Key_Release --
   -----------------

   procedure Key_Release
     (Widget  : in out Rho_Widget_Record'Class;
      Key     : Rho.Keyboard.Rho_Key_Code;
      Control : Rho.Keyboard.Control_Mask)
   is
      Data : constant Rho.Toolkit.Events.Key_Event_Data := (Key, Control);
   begin
      Widget.Emit (Rho.Toolkit.Signals.Signal_Key_Release_Event, Data);
   end Key_Release;

   ----------------------
   -- Layout_Rectangle --
   ----------------------

   function Layout_Rectangle
     (Widget : Rho_Widget_Record'Class)
      return Rho.Rectangle.Rho_Rectangle
   is
      Rectangle : Rho.Rectangle.Rho_Rectangle;
      Top_Left  : constant Css.Layout_Position := Widget.Get_Layout_Position;
      Size      : constant Css.Layout_Size     := Widget.Get_Layout_Size;
   begin
      Rectangle.X := Rho_Float (Top_Left.X);
      Rectangle.Y := Rho_Float (Top_Left.Y);
      Rectangle.Width := Rho_Float (Size.Width);
      Rectangle.Height := Rho_Float (Size.Height);
      return Rectangle;
   end Layout_Rectangle;

   ---------
   -- Log --
   ---------

--     procedure Log
--       (Widget  : Rho_Widget_Record'Class;
--        Message : String)
--     is
--        use Ada.Strings.Unbounded;
--     begin
--        Rho.Logging.Put_Line
--          (Widget.Widget_Hierarchy_Tags
--           & (if Widget.Id /= ""
--             then " (" & To_String (Widget.Id) & ")"
--             else "")
--           & (if Widget.Css_Classes /= Null_Unbounded_String
--             then " [" & To_String (Widget.Css_Classes) & "]"
--             else "")
--           & ": "
--           & Message);
--     end Log;

   ------------------
   -- Minimum_Size --
   ------------------

   overriding function Minimum_Size
     (Widget     : Rho_Widget_Record;
      Constraint : Css.Layout_Size)
      return Css.Layout_Size
   is
      pragma Unreferenced (Widget, Constraint);
   begin
      return (False, False, 0.0, 0.0);
   end Minimum_Size;

   ---------------
   -- Mouse_Out --
   ---------------

   procedure Mouse_Out
     (Widget : in out Rho_Widget_Record)
   is null;
--     begin
--        if Widget.Current_State = Hover then
--           Widget.Current_State := Active;
--        end if;
--     end Mouse_Out;

   ----------------
   -- Mouse_Over --
   ----------------

   procedure Mouse_Over
     (Widget : in out Rho_Widget_Record;
      X, Y   : Rho_Float)
   is null;
--        pragma Unreferenced (X);
--        pragma Unreferenced (Y);
--     begin
--        if Widget.Current_State = Active then
--           Widget.Current_State := Hover;
--        end if;
--     end Mouse_Over;

   ------------------
   -- Needs_Redraw --
   ------------------

   function Needs_Redraw
     (Widget : Rho_Widget_Record)
      return Boolean
   is
   begin
      return Widget.Invalidated;
   end Needs_Redraw;

   ------------------
   -- Needs_Resize --
   ------------------

   function Needs_Resize
     (Widget : Rho_Widget_Record)
      return Boolean
   is
   begin
      return Widget.Needs_Resize;
   end Needs_Resize;

   ---------------------
   -- On_Button_Press --
   ---------------------

   procedure On_Button_Press
     (Widget  : in out Rho_Widget_Record'Class;
      Handler : Button_Handler;
      After   : Boolean := False)
   is
      Button_Handler : constant Button_Handler_Record :=
                         Button_Handler_Record'
                           (User_Data             => null,
                            Handler_Function      => Handler,
                            User_Handler_Function => null);
   begin
      Widget.Add_Handler
        (Signal    => Rho.Toolkit.Signals.Signal_Button_Press_Event,
         Handler   => Button_Handler,
         After     => After);
   end On_Button_Press;

   ---------------------
   -- On_Button_Press --
   ---------------------

   procedure On_Button_Press
     (Widget    : in out Rho_Widget_Record'Class;
      Handler   : User_Button_Handler;
      User_Data : access Rho.Toolkit.Events.User_Data_Interface'Class;
      After     : Boolean := False)
   is
      Button_Handler : constant Button_Handler_Record :=
                         Button_Handler_Record'
                           (User_Data             => User_Data,
                            Handler_Function      => null,
                            User_Handler_Function => Handler);
   begin
      Widget.Add_Handler
        (Signal    => Rho.Toolkit.Signals.Signal_Button_Press_Event,
         Handler   => Button_Handler,
         After     => After);
   end On_Button_Press;

   ------------------
   -- On_Configure --
   ------------------

   procedure On_Configure
     (Widget  : in out Rho_Widget_Record'Class;
      Handler : Configure_Handler;
      After   : Boolean := False)
   is
      Configure_Handler : constant Configure_Handler_Record :=
                         Configure_Handler_Record'
                           (User_Data             => null,
                            Handler_Function      => Handler);
   begin
      Widget.Add_Handler
        (Signal    => Rho.Toolkit.Signals.Signal_Configure_Event,
         Handler   => Configure_Handler,
         After     => After);
   end On_Configure;

   -------------
   -- On_Draw --
   -------------

   procedure On_Draw
     (Widget  : in out Rho_Widget_Record'Class;
      Handler : Draw_Handler;
      After   : Boolean := True)
   is
      Draw_Handler : constant Draw_Handler_Record :=
                       Draw_Handler_Record'
                         (User_Data             => null,
                          Handler_Function      => Handler,
                          User_Handler_Function => null);
   begin
      Widget.Add_Handler
        (Signal    => Rho.Toolkit.Signals.Signal_Draw_Event,
         Handler   => Draw_Handler,
         After     => After);
   end On_Draw;

   -------------
   -- On_Draw --
   -------------

   procedure On_Draw
     (Widget    : in out Rho_Widget_Record'Class;
      Handler   : User_Draw_Handler;
      User_Data : access Rho.Toolkit.Events.User_Data_Interface'Class;
      After     : Boolean := True)
   is
      Draw_Handler : constant Draw_Handler_Record :=
                       Draw_Handler_Record'
                         (User_Data             =>
                                             Rho.Toolkit.Events.User_Data
                            (User_Data),
                          Handler_Function      => null,
                          User_Handler_Function => Handler);
   begin
      Widget.Add_Handler
        (Signal    => Rho.Toolkit.Signals.Signal_Draw_Event,
         Handler   => Draw_Handler,
         After     => After);
   end On_Draw;

   ------------------
   -- On_Key_Press --
   ------------------

   procedure On_Key_Press
     (Widget  : in out Rho_Widget_Record'Class;
      Handler : Key_Handler;
      After   : Boolean := False)
   is
      Key_Handler : constant Key_Handler_Record :=
                      Key_Handler_Record'
                        (User_Data             => null,
                         Handler_Function      => Handler,
                         User_Handler_Function => null);
   begin
      Widget.Add_Handler
        (Signal    => Rho.Toolkit.Signals.Signal_Key_Press_Event,
         Handler   => Key_Handler,
         After     => After);
   end On_Key_Press;

   ------------------
   -- On_Key_Press --
   ------------------

   procedure On_Key_Press
     (Widget    : in out Rho_Widget_Record'Class;
      Handler   : User_Key_Handler;
      User_Data : access Rho.Toolkit.Events.User_Data_Interface'Class;
      After     : Boolean := False)
   is
      Key_Handler : constant Key_Handler_Record :=
                      Key_Handler_Record'
                        (User_Data             => User_Data,
                         Handler_Function      => null,
                         User_Handler_Function => Handler);
   begin
      Widget.Add_Handler
        (Signal    => Rho.Toolkit.Signals.Signal_Key_Press_Event,
         Handler   => Key_Handler,
         After     => After);
   end On_Key_Press;

   ------------
   -- Parent --
   ------------

   function Parent
     (Widget : Rho_Widget_Record)
      return Rho_Widget
   is
   begin
      return Widget.Parent;
   end Parent;

   ------------
   -- Pixels --
   ------------

   function Pixels
     (Spec : String)
      return Natural
   is
      Index : Positive := Spec'First;
   begin
      while Index <= Spec'Length
        and then Spec (Index) in '0' .. '9'
      loop
         Index := Index + 1;
      end loop;
      if Index > Spec'First then
         return Natural'Value (Spec (Spec'First .. Index - 1));
      else
         raise Constraint_Error with "invalid pixel size: " & Spec;
      end if;
   end Pixels;

   ----------------
   -- Queue_Draw --
   ----------------

   procedure Queue_Draw
     (Widget : in out Rho_Widget_Record)
   is
   begin
      Rho_Widget_Record'Class (Widget).Invalidate_Region
        ((0.0, 0.0,
         Rho_Float (Widget.Layout_Width),
         Rho_Float (Widget.Layout_Height)));
   end Queue_Draw;

   ------------------
   -- Queue_Resize --
   ------------------

   procedure Queue_Resize
     (Widget : in out Rho_Widget_Record)
   is
   begin
      Widget.Needs_Resize := True;
      if Widget.Parent /= null then
         Widget.Parent.Queue_Resize;
      end if;
   end Queue_Resize;

   -------------------
   -- Set_Attribute --
   -------------------

   overriding procedure Set_Attribute
     (Widget    : in out Rho_Widget_Record;
      Name      : String;
      Value     : String)
   is
      use Ada.Strings.Unbounded;
   begin
      if Name = "class" then
         Widget.Css_Classes := To_Unbounded_String (To_Lower (Value));
      elsif Name = "id" then
         Widget.Set_Id (To_Lower (Value));
      elsif Name = "style" then
         Widget.Inline_Style := Css.Parser.Parse_Inline_Style (Value);
      end if;
   end Set_Attribute;

   ---------------------
   -- Set_Child_Index --
   ---------------------

   procedure Set_Child_Index
     (Widget : in out Rho_Widget_Record'Class;
      Index  : Natural)
   is
   begin
      Widget.Child_Index := Index;
   end Set_Child_Index;

   ------------------------------
   -- Set_Contents_Layout_Size --
   ------------------------------

   overriding procedure Set_Contents_Layout_Size
     (Widget   : in out Rho_Widget_Record;
      Size     : in     Css.Layout_Size)
   is
   begin
      Widget.Contents_Size := Size;
   end Set_Contents_Layout_Size;

   --------------------
   -- Set_Element_Id --
   --------------------

   procedure Set_Id
     (Widget     : in out Rho_Widget_Record'Class;
      Element_Id : String)
   is
   begin
      Widget.Widget_Id :=
        Ada.Strings.Unbounded.To_Unbounded_String (Element_Id);
      Widget.Invalidated := True;
   end Set_Id;

   -------------------------
   -- Set_Layout_Position --
   -------------------------

   overriding procedure Set_Layout_Position
     (Widget   : in out Rho_Widget_Record;
      Position : Css.Layout_Position)
   is
   begin
      Widget.Layout_Position := Position;
   end Set_Layout_Position;

   ---------------------
   -- Set_Layout_Size --
   ---------------------

   overriding procedure Set_Layout_Size
     (Widget   : in out Rho_Widget_Record;
      Size     : in     Css.Layout_Size)
   is
   begin
      Widget.Layout_Size := Size;
      if Size.Constrained_Width and then Size.Constrained_Height then
         Widget.Emit
           (Signal => Rho.Toolkit.Signals.Signal_Configure_Event,
            Data   =>
              Rho.Toolkit.Events.Configure_Event_Data'
                (Width  => Rho.Non_Negative_Float (Size.Width),
                 Height => Rho.Non_Negative_Float (Size.Height)));
      end if;
   end Set_Layout_Size;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent
     (Widget     : in out Rho_Widget_Record;
      New_Parent : not null access Rho_Widget_Record'Class)
   is
   begin
      Widget.Parent := Rho.Toolkit.Widget.Rho_Widget (New_Parent);
   end Set_Parent;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State
     (Widget : in out Rho_Widget_Record;
      State  : Widget_State)
   is null;
--     begin
--        if State /= Widget.Current_State then
--           Widget.Current_State := State;
--           Rho_Widget_Record'Class (Widget).Invalidate_Region
--             (Widget.Layout_Rectangle);
--           declare
--              Style : Widget_Style_Record renames
--                        Widget.State_Info (Widget.Current_State);
--           begin
--              Style_Sheet (Widget).Load_Style_Rules (Widget);
--              Style.Loaded := True;
--           end;
--        end if;
--     end Set_State;

   ---------------
   -- Set_Style --
   ---------------

   overriding procedure Set_Style
     (Widget  : in out Rho_Widget_Record;
      Name    : String;
      State   : String;
      Value   : Css.Css_Element_Value)
   is
   begin
      Widget.Styles.Set_Style (Name, State, Value);
   end Set_Style;

   ---------------------
   -- Set_Style_Sheet --
   ---------------------

   procedure Set_Style_Sheet
     (Widget      : in out Rho_Widget_Record'Class;
      Style_Sheet : Css.Style_Sheet)
   is
   begin
      Widget.Local_Style_Sheet := Style_Sheet;
   end Set_Style_Sheet;

   -------------
   -- Set_Tag --
   -------------

   overriding procedure Set_Tag
     (Widget : in out Rho_Widget_Record;
      Tag    : String)
   is
   begin
      Widget.Tag := Ada.Strings.Unbounded.To_Unbounded_String (Tag);
   end Set_Tag;

   ----------
   -- Show --
   ----------

   procedure Show
     (Widget : in out Rho_Widget_Record)
   is
   begin
      if not Widget.Styles_Loaded then
         Css.Current_Style_Sheet.Load_Style_Rules (Widget);
         Rho_Widget_Record'Class (Widget).Initialize_Styles;
         Widget.Styles_Loaded := True;
      end if;

      Widget.Visible := True;
   end Show;

   --------------
   -- Show_All --
   --------------

   procedure Show_All
     (Widget : in out Rho_Widget_Record)
   is
   begin
      Rho_Widget_Record'Class (Widget).Show;
   end Show_All;

   -----------
   -- Shown --
   -----------

   function Shown
     (Widget : Rho_Widget_Record'Class)
      return Boolean
   is
   begin
      return Widget.Visible;
   end Shown;

   ---------------
   -- Specify_X --
   ---------------

   procedure Specify_X
     (Left, Right   : Rho_Float;
      Specification : String;
      X             : in out Rho_Float)
   is
   begin
      if Specification = "left" then
         X := Left;
      elsif Specification = "right" then
         X := Right;
      elsif Specification = "centre" or else Specification = "center" then
         X := Left + (Right - Left) / 2.0;
      else
         begin
            X := Rho_Float'Value (Specification);
            X := Left + (Right - Left) * X;
         exception
            when Constraint_Error =>
               Rho.Toolkit.Errors.Warning
                 ("Specify_X: cannot understand " & Specification);
         end;
      end if;
   end Specify_X;

   ---------------
   -- Specify_Y --
   ---------------

   procedure Specify_Y
     (Top, Bottom   : Rho_Float;
      Specification : String;
      Y             : in out Rho_Float)
   is
   begin
      if Specification = "top" then
         Y := Top;
      elsif Specification = "bottom" then
         Y := Bottom;
      elsif Specification = "centre" or else Specification = "center" then
         Y := Top + (Bottom - Top) / 2.0;
      else
         begin
            Y := Rho_Float'Value (Specification);
            Y := Top + (Bottom - Top) * Y;
         exception
            when Constraint_Error =>
               Rho.Toolkit.Errors.Warning
                 ("Specify_Y: cannot understand " & Specification);
         end;
      end if;
   end Specify_Y;

   -----------
   -- Style --
   -----------

   overriding function Style
     (Widget : Rho_Widget_Record;
      Name   : String)
      return Css.Css_Element_Value
   is
      use Css;
      Result : Css.Css_Element_Value := Widget.Styles.Style (Name);
   begin
      if Result = Null_Element_Value
        and then Css.Is_Inherited (Name)
        and then Widget.Parent_Element /= null
      then
         Result := Widget.Parent_Element.Style (Name);
      end if;
      return Result;
   end Style;

   -----------------
   -- Style_Sheet --
   -----------------

   function Style_Sheet
     (Widget : Rho_Widget_Record'Class)
      return Css.Style_Sheet
   is
      use type Css.Style_Sheet;
   begin
      if Widget.Local_Style_Sheet /= null then
         return Widget.Local_Style_Sheet;
      elsif Widget.Parent /= null then
         return Widget.Parent.Style_Sheet;
      else
         return Css.Current_Style_Sheet;
      end if;
   end Style_Sheet;

   -----------------
   -- To_Rho_Color --
   -----------------

   function To_Rho_Color
     (Value : Css.Css_Element_Value)
      return Rho.Color.Rho_Color
   is
      Color : constant Css.Css_Color :=
                Css.To_Color (Value);
   begin
      return (Rho.Unit_Float (Rho_Float (Color.Red) / 255.0),
              Rho.Unit_Float (Rho_Float (Color.Green) / 255.0),
              Rho.Unit_Float (Rho_Float (Color.Blue) / 255.0),
              Rho.Unit_Float (Rho_Float (Color.Alpha) / 255.0));
   end To_Rho_Color;

   ----------------
   -- Top_Widget --
   ----------------

   function Top_Widget
     (Widget : Rho_Widget_Record'Class)
      return Rho_Widget
   is
      It : Rho_Widget := Widget.Parent;
   begin
      while It.Parent /= null loop
         It := It.Parent;
      end loop;
      return It;
   end Top_Widget;

   -------------
   -- Visible --
   -------------

   function Visible
     (Widget : in out Rho_Widget_Record)
      return Boolean
   is
   begin
      return Widget.Visible;
   end Visible;

end Rho.Toolkit.Widget;
