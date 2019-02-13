with Rho.Mouse;

--  with Rho.Toolkit.Events;

package body Rho.Toolkit.Panel is

   procedure Clear_Layout_Size
     (Widget : in out Rho.Toolkit.Widget.Rho_Widget_Record'Class);

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Panel : in out Rho_Panel_Record) is
   begin
      Rho.Toolkit.Bin.Rho_Bin_Record (Panel).Adjust;
      Panel.Buffer := null;
   end Adjust;

   -----------------------
   -- Clear_Layout_Size --
   -----------------------

   procedure Clear_Layout_Size
     (Widget : in out Rho.Toolkit.Widget.Rho_Widget_Record'Class)
   is
   begin
      for Child of Widget.Child_Elements loop
         Child.Set_Layout_Size ((False, False, 0.0, 0.0));
         Clear_Layout_Size (Rho.Toolkit.Widget.Rho_Widget (Child).all);
      end loop;
   end Clear_Layout_Size;

   ------------
   -- Create --
   ------------

   procedure Create
     (Panel : not null access Rho_Panel_Record;
      Top   : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class;
      Element_Id : String := "")
   is
   begin
      Rho.Toolkit.Widget.Rho_Widget_Record (Panel.all).Create (Element_Id);
      Panel.Add (Top);
      Top.Set_Parent (Rho.Toolkit.Widget.Rho_Widget (Panel));
   end Create;

   --------------------
   -- Execute_Render --
   --------------------

   overriding procedure Execute_Render
     (Panel  : in out Rho_Panel_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
      use type Rho.Rectangle.Rho_Rectangle;
      use type Rho.Toolkit.Buffer.Rho_Buffer;

      Redraw    : Boolean := False;
      Resize    : Boolean := False;
      Position  : constant Css.Layout_Position :=
                    Panel.Get_Layout_Position;
      Size      : constant Css.Layout_Size :=
                    Panel.Get_Layout_Size;
      Rectangle : constant Rho.Rectangle.Rho_Rectangle :=
                    (X      => 0.0,
                     Y      => 0.0,
                     Width  => (if Size.Constrained_Width
                                then Rho_Float (Size.Width)
                                else Panel.Viewport.Width),
                     Height => (if Size.Constrained_Height
                                then Rho_Float (Size.Height)
                                else Panel.Viewport.Height));
   begin

      if not Panel.Visible then
         return;
      end if;

      if Panel.Buffer = null then
         Panel.Buffer :=
           Rho.Toolkit.Buffer.Create (Rectangle);
         Resize := True;
         Redraw := True;
      elsif Rectangle /= Panel.Buffer.Get_Rectangle then
         Panel.Buffer.Set_Rectangle (Rectangle);
         Resize := True;
         Redraw := True;
      end if;

      Panel.Buffer.Set_Screen_Position
        (Rho_Float (Position.X),
         Rho_Float (Position.Y));

      if Resize or else Panel.Needs_Resize
        or else (Panel.Has_Children and then Panel.Child.Needs_Resize)
      then
         Clear_Layout_Size (Panel);
         Panel.Apply_Layout;
         Panel.After_Resize;
         Redraw := True;
      end if;

      if Redraw or else Panel.Needs_Redraw
        or else (Panel.Has_Children and then Panel.Child.Needs_Redraw)
      then

         Panel.Draw (Rectangle);

         declare
            Context : constant Cairo.Cairo_Context :=
                        Panel.Buffer.Start_Draw;
         begin
            Cairo.Set_Source_Surface (Context, Panel.Draw_Surface, 0.0, 0.0);
            Cairo.Paint (Context);
            Panel.Buffer.Finish_Draw;
         end;
      end if;

      Panel.Buffer.Render (Target);

      --           declare
      --              use Maas;
      --              --           Bottom : constant Rho_Float :=
      --              --             Target.Height - Panel.Y - Panel.Height;
      --              --  Top    : constant Rho_Float := Bottom + Panel.Height;
      --           begin
      --              --           GL.Disable (GL.DEPTH_TEST);
      --              --           GL.Enable (GL.BLEND);
      --              --           GL.Blend_Func (GL.SRC_ALPHA,  GL.DST_ALPHA);
      --              Target.Matrix_Mode (Rho.Matrices.Projection);
      --              Target.Load_Identity;
      --              Target.Ortho (0.0, Target.Width,
      --                            0.0, Target.Height,
      --                            1.0, -1.0);
      --              Target.Matrix_Mode (Rho.Matrices.Model_View);
      --              Target.Load_Identity;
      --
      --              Target.Set_Output_Position
      --                (Panel.X, Target.Height - Panel.Y - Panel.Height);
      --
      --              Target.Render_Surface
      --                (Width    => Panel.Width,
      --                 Height   => Panel.Height,
      --                 X_Origin => 0.0,
      --                 Y_Origin => 0.0,
      --                 X_Move   => 0.0,
      --                 Y_Move   => 0.0,
      --                 Surface  => Panel.Surface);
      --              --           GL.Disable (GL.BLEND);
      --              --           GL.Enable (GL.DEPTH_TEST);
      --           end;
      --        end if;

   end Execute_Render;

--        pragma Unreferenced (Event);
--        use Maas;
--        use Rho.Toolkit.Widget;
--        X : constant Rho_Float :=
--              Rho.Mouse.Current_Mouse.State.X;
--        Y : constant Rho_Float :=
--              Rho.Mouse.Current_Mouse.State.Reverse_Y;
--        Rec : constant Rho.Rectangle.Rho_Rectangle :=
--                Listener.Panel.Layout_Rectangle;
--     begin
--        if Rho.Rectangle.Contains_Point (Rec, X, Y) then
--
--
--           declare
--              use Rho.Toolkit.Events;
--              use type Rho.Mouse.Button_State;
--              Panel_X : constant Rho_Float :=
--                          X - Listener.Panel.Layout_X;
--              Panel_Y : constant Rho_Float :=
--                          Y - Listener.Panel.Layout_Y;
--              New_Buttons : Rho.Mouse.Button_State_Array renames
--                              Rho.Mouse.Current_Mouse.State.Button;
--  --              Widget : constant Rho.Toolkit.Widget.Rho_Widget :=
--  --                         Listener.Panel.Child.Get_Child_Widget
--  --                           (Child_X, Child_Y);
--           begin
--              Listener.Panel.Mouse_Over (Child_X, Child_Y);
--
--              for Button in New_Buttons'Range loop
--                 if New_Buttons (Button)
--                   /= Listener.Last_Mouse.Button (Button)
--                 then
--                    case New_Buttons (Button) is
--                       when Rho.Mouse.Up =>
--                          if Widget.all in
--                            Mouse_Button_Event_Interface'Class
--                          then
--                             Mouse_Button_Event_Interface'Class
--                               (Widget.all).On_Mouse_Button_Released
--                               (Button, (others => False));
--                          end if;
--                          if Listener.Last_Widget = Widget
--                            and then Widget.all in
--                              Click_Event_Interface'Class
--                          then
--                       Click_Event_Interface'Class (Widget.all).On_Click
--                               (Button, (others => False));
--                          end if;
--                       when Rho.Mouse.Down =>
--                          if Widget.all in
--                            Mouse_Button_Event_Interface'Class
--                          then
--                             Mouse_Button_Event_Interface'Class
--                               (Widget.all).On_Mouse_Button_Pressed
--                               (Button, (others => False));
--                          end if;
--                       end case;
--                    end if;
--                 end loop;
--                 Listener.Last_Mouse.Button := New_Buttons;
--              end if;
--              Listener.Last_Widget := Widget;
--           end;
--        elsif Listener.Panel.Child.Current_State = Hover then
--           Listener.Panel.Child.Mouse_Out;
--        end if;

--     end Frame_Started;

   ----------
   -- Hide --
   ----------

   overriding procedure Hide
     (Panel : in out Rho_Panel_Record)
   is
   begin
      Rho.Toolkit.Widget.Rho_Widget_Record (Panel).Hide;
   end Hide;

   -----------------------
   -- Invalidate_Region --
   -----------------------

   overriding procedure Invalidate_Region
     (Panel  : in out Rho_Panel_Record;
      Region : Rho.Rectangle.Rho_Rectangle)
   is
      use type Rho.Toolkit.Buffer.Rho_Buffer;
   begin
      Rho.Toolkit.Widget.Rho_Widget_Record (Panel).Invalidate_Region (Region);
      if Panel.Buffer /= null then
         Panel.Buffer.Invalidate (Region);
      end if;
   end Invalidate_Region;

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New
     (Panel : in out Rho_Panel;
      Top   : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class)
   is
   begin
      Panel := new Rho_Panel_Record;
      Panel.Create (Top);
   end Rho_New;

   -------------
   -- Rho_New --
   -------------

   procedure Rho_New
     (Panel : in out Rho_Panel)
   is
   begin
      Panel := new Rho_Panel_Record;
      Panel.Create ("");
   end Rho_New;

   ------------------
   -- Set_Viewport --
   ------------------

   overriding procedure Set_Viewport
     (Panel    : in out Rho_Panel_Record;
      Viewport : Rho.Viewport.Rho_Viewport)
   is
   begin
      Panel.Viewport := Viewport;
   end Set_Viewport;

   ----------
   -- Show --
   ----------

   overriding procedure Show
     (Panel : in out Rho_Panel_Record)
   is
   begin
      Rho.Toolkit.Bin.Rho_Bin_Record (Panel).Show;
      Rho_Panel_Record'Class (Panel).Queue_Resize;
   end Show;

   --------------
   -- Viewport --
   --------------

   overriding function Viewport
     (Panel : Rho_Panel_Record)
      return Rho.Viewport.Rho_Viewport
   is
   begin
      return Panel.Viewport;
   end Viewport;

end Rho.Toolkit.Panel;
