with Glib;

with Rho.Rectangle;

with Rho.Toolkit.Events;
with Rho.Toolkit.Signals;

package body Rho.Toolkit.Container is

   function Handle_Draw
     (Widget  : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class;
      Context : in     Cairo.Cairo_Context)
      return Rho.Toolkit.Events.Event_Response;

   ---------
   -- Add --
   ---------

   procedure Add
     (Container : not null access Rho_Container_Record;
      Child  : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class)
   is
   begin
      Container.Children.Append (Child);
      Child.Set_Parent (Container);
      Child.Set_Child_Index (Positive (Container.Children.Length));
      Rho_Container_Record'Class (Container.all).Invalidate_Region
        (Container.Layout_Rectangle);
      Rho_Container_Record'Class (Container.all).Queue_Resize;
   end Add;

   ---------------
   -- Add_Child --
   ---------------

   overriding procedure Add_Child
     (Container : not null access Rho_Container_Record;
      Child     : not null access
        Rho.Toolkit.Buildable.Rho_Buildable_Interface'Class)
   is
   begin
      Rho_Container_Record'Class (Container.all)'Access.Add
        (Rho.Toolkit.Widget.Rho_Widget (Child));
   end Add_Child;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust
     (Container : in out Rho_Container_Record)
   is
   begin
      Rho.Toolkit.Widget.Rho_Widget_Record (Container).Adjust;
      for W of Container.Children loop
         W := new Rho.Toolkit.Widget.Rho_Widget_Record'Class'(W.all);
      end loop;
   end Adjust;

   -----------
   -- Child --
   -----------

   overriding function Child
     (Container : Rho_Container_Record;
      Index     : Positive)
      return Rho.Toolkit.Widget.Rho_Widget
   is
      Current : Natural := 0;
   begin
      for Widget of Container.Children loop
         Current := Current + 1;
         if Current = Index then
            return Widget;
         end if;
      end loop;
      return null;
   end Child;

   --------------------
   -- Child_Elements --
   --------------------

   overriding function Child_Elements
     (Container : Rho_Container_Record)
      return Css.Array_Of_Elements
   is
      Index : Natural := 0;
   begin
      return Result : Css.Array_Of_Elements
        (1 .. Natural (Container.Children.Length))
      do
         for Child of Container.Children loop
            Index := Index + 1;
            Result (Index) := Css.Css_Element (Child);
         end loop;
      end return;
   end Child_Elements;

   ------------------
   -- Delete_Child --
   ------------------

   procedure Delete_Child
     (Container : not null access Rho_Container_Record;
      Child     : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class)
   is
      use Rho.Toolkit.Widget;
      New_Children : Rho.Toolkit.Widget.Lists.List;
      Found        : Boolean := False;
   begin
      for W of Container.Children loop
         if W /= Rho_Widget (Child) then
            New_Children.Append (W);
         else
            Found := True;
         end if;
      end loop;
      if not Found then
         raise Constraint_Error with
           "child " & Child.Id & " not found in container "
           & Container.Id;
      end if;
      Container.Children := New_Children;
      Rho_Container_Record'Class (Container.all).Queue_Resize;
      Container.Queue_Draw;
   end Delete_Child;

   ---------------------
   -- Delete_Children --
   ---------------------

   procedure Delete_Children
     (Container : not null access Rho_Container_Record)
   is
   begin
      Container.Children.Clear;
      Rho_Container_Record'Class (Container.all).Queue_Resize;
      Rho_Container_Record'Class (Container.all).Queue_Draw;
   end Delete_Children;

   -----------------
   -- First_Child --
   -----------------

   function First_Child
     (Container : Rho_Container_Record)
      return Rho.Toolkit.Widget.Rho_Widget
   is
   begin
      return Container.Children.First_Element;
   end First_Child;

   ----------------------
   -- Get_Child_Widget --
   ----------------------

   overriding function Get_Child_Widget
     (Container : Rho_Container_Record;
      X, Y      : Rho_Float)
      return Rho.Toolkit.Widget.Rho_Widget
   is
      Internal_X : constant Rho_Float := X - Container.Layout_Rectangle.X;
      Internal_Y : constant Rho_Float := Y - Container.Layout_Rectangle.Y;
   begin
      for Child of Container.Children loop
         if Rho.Rectangle.Contains_Point
           (Child.Layout_Rectangle, Internal_X, Internal_Y)
         then
            declare
               use type Rho.Toolkit.Widget.Rho_Widget;
               Result : constant Rho.Toolkit.Widget.Rho_Widget :=
                          Child.Get_Child_Widget (Internal_X, Internal_Y);
            begin
               if Result = null then
                  return Child;
               else
                  return Result;
               end if;
            end;
         end if;
      end loop;
      return null;
   end Get_Child_Widget;

   -----------------
   -- Handle_Draw --
   -----------------

   function Handle_Draw
     (Widget    : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class;
      Context   : in     Cairo.Cairo_Context)
      return Rho.Toolkit.Events.Event_Response
   is
      Container : constant Rho_Container := Rho_Container (Widget);
   begin
      for Child of Container.Children loop
         if Child.Visible then
            declare
               use Glib;
               Rec : constant Rho.Rectangle.Rho_Rectangle :=
                       Child.Layout_Rectangle;
               Cr  : constant Cairo.Cairo_Context :=
                       Cairo.Create (Child.Draw_Surface);
            begin

               Cairo.Save (Cr);
               Cairo.Set_Operator (Cr, Cairo.Cairo_Operator_Clear);
               Cairo.Paint (Cr);
               Cairo.Restore (Cr);

               Child.Emit (Rho.Toolkit.Signals.Signal_Draw_Event,
                           Rho.Toolkit.Events.Draw_Event_Data'
                             (Context => Cr,
                              Region  => Rec));
               Cairo.Destroy (Cr);
               Cairo.Rectangle
                 (Context, Gdouble (Rec.X), Gdouble (Rec.Y),
                  Gdouble (Rec.Width), Gdouble (Rec.Height));
               Cairo.Set_Source_Surface
                 (Context, Child.Draw_Surface,
                  Gdouble (Rec.X), Gdouble (Rec.Y));
               Cairo.Fill (Context);
            end;
         end if;
      end loop;
      return Rho.Toolkit.Events.Propagate_Event;
   end Handle_Draw;

   ------------------
   -- Has_Children --
   ------------------

   function Has_Children
     (Container : Rho_Container_Record)
      return Boolean
   is
   begin
      return not Container.Children.Is_Empty;
   end Has_Children;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Container : in out Rho_Container_Record)
   is
   begin
      Rho.Toolkit.Widget.Rho_Widget_Record (Container).Initialize;
      Container.On_Draw (Handle_Draw'Access);
   end Initialize;

   ----------------------
   -- Iterate_Children --
   ----------------------

   procedure Iterate_Children
     (Container : Rho_Container_Record;
      Callback : not null access
        procedure (Widget : Rho.Toolkit.Widget.Rho_Widget))
   is
   begin
      for Child of Container.Children loop
         Callback (Child);
      end loop;
   end Iterate_Children;

   ---------------
   -- Mouse_Out --
   ---------------

   overriding procedure Mouse_Out
     (Container : in out Rho_Container_Record)
   is
      use Rho.Toolkit.Widget;
   begin
      Rho_Widget_Record (Container).Mouse_Out;
      for Child of Container.Children loop
         if Child.Current_State = Hover then
            Child.Mouse_Out;
         end if;
      end loop;
   end Mouse_Out;

   ----------------
   -- Mouse_Over --
   ----------------

   overriding procedure Mouse_Over
     (Container : in out Rho_Container_Record;
      X, Y   : Rho_Float)
   is
      use Rho.Toolkit.Widget;
   begin
      Rho_Widget_Record (Container).Mouse_Over (X, Y);
      for Child of Container.Children loop
         if Rho.Rectangle.Contains_Point (Child.Layout_Rectangle, X, Y) then
            Child.Mouse_Over (X, Y);
         elsif Child.Current_State = Hover then
            Child.Mouse_Out;
         end if;
      end loop;
   end Mouse_Over;

   --------------
   -- Show_All --
   --------------

   overriding procedure Show_All
     (Container : in out Rho_Container_Record)
   is
   begin
      Rho.Toolkit.Widget.Rho_Widget_Record (Container).Show_All;
      for Child of Container.Children loop
         Child.Show_All;
      end loop;
   end Show_All;

end Rho.Toolkit.Container;
