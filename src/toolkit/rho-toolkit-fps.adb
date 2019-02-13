with Rho.Frame_Event;
with Rho.Main;

with Rho.Toolkit.Div_Element;
with Rho.Toolkit.Label;

package body Rho.Toolkit.FPS is

   Number_Of_Samples : constant := 200;
   type Sample_Count is range 0 .. Number_Of_Samples;
   subtype Sample_Index is Sample_Count range 1 .. Sample_Count'Last;

   type Sample_Array is array (Sample_Index) of Duration;

   type FPS_Listener is
     new Rho.Frame_Event.Rho_Frame_Listener_Interface with
      record
         Label      : Rho.Toolkit.Label.Rho_Label;
         Size       : Rho.Toolkit.Label.Rho_Label;
         Elapsed    : Duration     := 0.0;
         Samples    : Sample_Array := (others => 0.0);
         Next_Index : Sample_Index := 1;
         Ready      : Boolean      := False;
      end record;

   overriding procedure Frame_Started
     (Listener : in out FPS_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event);

   type FPS_Listener_Access is access all FPS_Listener'Class;

   -----------------------
   -- Create_FPS_Widget --
   -----------------------

   function Create_FPS_Widget
      return Rho.Toolkit.Widget.Rho_Widget
   is
      Main  : Rho.Toolkit.Div_Element.Rho_Div_Element;
      Label : Rho.Toolkit.Label.Rho_Label;
      Size  : Rho.Toolkit.Label.Rho_Label;
   begin
      Rho.Toolkit.Label.Rho_New (Label, "FPS");
      Rho.Toolkit.Label.Rho_New (Size, "width x height");
      Rho.Toolkit.Div_Element.Rho_New (Main);
      Main.Add (Label);
      Main.Add (Size);

      declare
         Listener : constant FPS_Listener_Access := new FPS_Listener;
      begin
         Listener.Label := Label;
         Listener.Size := Size;
         Rho.Main.Add_Frame_Listener (Listener);
      end;

      return Rho.Toolkit.Widget.Rho_Widget (Main);
   end Create_FPS_Widget;

   -------------------
   -- Frame_Started --
   -------------------

   overriding procedure Frame_Started
     (Listener : in out FPS_Listener;
      Event    : Rho.Frame_Event.Rho_Frame_Event)
   is
   begin
      Listener.Size.Set_Label
        (Natural'Image (Natural (Event.Render_Target.Viewport.Width))
         & " x"
         & Natural'Image (Natural (Event.Render_Target.Viewport.Height)));

      Listener.Elapsed := Listener.Elapsed
        - Listener.Samples (Listener.Next_Index)
        + Event.Time_Since_Last_Event;

      Listener.Samples (Listener.Next_Index) :=
        Event.Time_Since_Last_Event;

      if Listener.Next_Index = Sample_Index'Last then
         Listener.Ready := True;
         Listener.Next_Index := 1;
      else
         Listener.Next_Index := Listener.Next_Index + 1;
      end if;

      if Listener.Ready then
         Listener.Label.Set_Label
           (Natural'Image
              (Natural
                   (Float (Number_Of_Samples)
                    / Float (Listener.Elapsed)))
           & " FPS");
      end if;
   end Frame_Started;

end Rho.Toolkit.FPS;
