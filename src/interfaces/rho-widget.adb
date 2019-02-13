with GLUT;

package body Rho.Widget is

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height
     (Widget : Rho.Toolkit_Widget_Record)
      return Positive
   is
   begin
      return Widget.Height;
   end Get_Height;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
     (Widget : Rho.Toolkit_Widget_Record)
      return Positive
   is
   begin
      return Widget.Width;
   end Get_Width;

   -----------
   -- Label --
   -----------

   function Label
     (Widget  : Rho.Toolkit_Widget_Record)
     return String
   is
      pragma Unreferenced (Widget);
   begin
      return "Rho.Toolkit_Widget";
   end Label;

   ----------------
   -- Queue_Draw --
   ----------------

   procedure Queue_Draw
     (Widget : in out Rho.Toolkit_Widget_Record)
   is
      pragma Unreferenced (Widget);
   begin
      GLUT.PostRedisplay;
   end Queue_Draw;

   --------------------
   -- Set_Event_Mask --
   --------------------

   procedure Set_Event_Mask
     (Widget  : in out Rho.Toolkit_Widget_Record;
      Mask    : in     Rho.Event.Rho_Event_Mask)
   is
   begin
      Widget.Event_Mask := (others => False);
      for I in Mask'Range loop
         Widget.Event_Mask (Mask (I)) := True;
      end loop;
   end Set_Event_Mask;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
     (Widget   : in out Rho.Toolkit_Widget_Record;
      X, Y     : in     Integer)
   is
   begin
      Widget.X   := X;
      Widget.Y   := Y;
   end Set_Position;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
     (Widget : in out Rho.Toolkit_Widget_Record;
      Width  : in     Positive;
      Height : in     Positive)
   is
   begin
      Widget.Width  := Width;
      Widget.Height := Height;
   end Set_Size;

   ----------------------
   -- Set_Size_Request --
   ----------------------

   procedure Set_Size_Request
     (Widget : in out Rho.Toolkit_Widget_Record;
      Width  : in     Positive;
      Height : in     Positive)
   is
   begin
      Widget.Width  := Width;
      Widget.Height := Height;
   end Set_Size_Request;

   ----------
   -- Show --
   ----------

   procedure Show
     (Widget  : not null access Rho.Toolkit_Widget_Record)
   is
      pragma Unreferenced (Widget);
   begin
      null;
   end Show;

end Rho.Widget;
