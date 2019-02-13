package body Rho.Toolkit.Fixed is

   ---------
   -- Add --
   ---------

   overriding procedure Add
     (Fixed : not null access Rho_Fixed_Record;
      Child  : not null access Rho.Toolkit.Widget.Rho_Widget_Record'Class)
   is
   begin
      Rho.Toolkit.Container.Rho_Container_Record (Fixed.all).Add (Child);
   end Add;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Fixed   : in out Rho_Fixed_Record;
      Context  : in     Cairo.Cairo_Context;
      X, Y     : in     Rho_Float;
      Region   : in     Rho.Rectangle.Rho_Rectangle)
   is
   begin
      Rho.Toolkit.Container.Rho_Container_Record (Fixed).Draw
        (Context, X, Y, Region);
   end Draw;

   overriding procedure Get_Preferred_Size
     (Fixed : Rho_Fixed_Record;
      Width, Height : out Rho_Float)
   is
      procedure Process_Child (Widget : Rho.Toolkit.Widget.Rho_Widget);

      -------------------
      -- Process_Child --
      -------------------

      procedure Process_Child (Widget : Rho.Toolkit.Widget.Rho_Widget) is
         use type Rho_Float;
      begin
         if Widget.X + Widget.Width > Width then
            Width := Widget.X + Widget.Width;
         end if;
         if Widget.Y + Widget.Height > Height then
            Height := Widget.Y + Widget.Height;
         end if;
      end Process_Child;

   begin
      Width := 0.0;
      Height := 0.0;
      Fixed.Iterate_Children (Process_Child'Access);
   end Get_Preferred_Size;

   ------------
   -- Rho_New --
   ------------

   procedure Rho_New
     (Fixed : in out Rho_Fixed;
      Id    : String := "")
   is
   begin
      Fixed := new Rho_Fixed_Record;
      Fixed.Create (Id);
   end Rho_New;

end Rho.Toolkit.Fixed;
