with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions;
with Ada.Text_IO;

with Rho.Assets;
with Rho.Paths;

package body Rho.Main is

   package Frame_Listener_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Rho.Frame_Event.Rho_Frame_Listener, Rho.Frame_Event."=");

   protected Frame_Listeners is
      procedure Add_Frame_Listener
        (Listener : Rho.Frame_Event.Rho_Frame_Listener);
      procedure Remove_Frame_Listener
        (Listener : Rho.Frame_Event.Rho_Frame_Listener);
      procedure Run_Frame_Started
        (Event   : Rho.Frame_Event.Rho_Frame_Event);

      procedure Run_Frame_Rendering_Queued
        (Event   : Rho.Frame_Event.Rho_Frame_Event);

      procedure Run_Frame_Ended
        (Event   : Rho.Frame_Event.Rho_Frame_Event);

      procedure Update_Frame_Listener_List;

   private
      Listeners : Frame_Listener_Lists.List;
      Added     : Frame_Listener_Lists.List;
      Removed   : Frame_Listener_Lists.List;
   end Frame_Listeners;

   Local_Renderer : Rho.Rendering.Rho_Renderer;

   First_Frame : Boolean := True;
   Last_Render_Time : Ada.Calendar.Time;

   protected body Frame_Listeners is

      ------------------------
      -- Add_Frame_Listener --
      ------------------------

      procedure Add_Frame_Listener
        (Listener : Rho.Frame_Event.Rho_Frame_Listener)
      is
      begin
         Added.Append (Listener);
      end Add_Frame_Listener;

      ---------------------------
      -- Remove_Frame_Listener --
      ---------------------------

      procedure Remove_Frame_Listener
        (Listener : Rho.Frame_Event.Rho_Frame_Listener)
      is
      begin
         Removed.Append (Listener);
      end Remove_Frame_Listener;

      ---------------------
      -- Run_Frame_Ended --
      ---------------------

      procedure Run_Frame_Ended
        (Event   : Rho.Frame_Event.Rho_Frame_Event)
      is
      begin
         for Listener of Listeners loop
            Listener.Frame_Ended (Event);
         end loop;
      end Run_Frame_Ended;

      --------------------------------
      -- Run_Frame_Rendering_Queued --
      --------------------------------

      procedure Run_Frame_Rendering_Queued
        (Event   : Rho.Frame_Event.Rho_Frame_Event)
      is
      begin
         for Listener of Listeners loop
            Listener.Frame_Rendering_Queued (Event);
         end loop;
      end Run_Frame_Rendering_Queued;

      -----------------------
      -- Run_Frame_Started --
      -----------------------

      procedure Run_Frame_Started
        (Event   : Rho.Frame_Event.Rho_Frame_Event)
      is
      begin
         for Listener of Listeners loop
            Listener.Frame_Started (Event);
         end loop;
      end Run_Frame_Started;

      --------------------------------
      -- Update_Frame_Listener_List --
      --------------------------------

      procedure Update_Frame_Listener_List is
      begin
         for R of Removed loop
            declare
               use Frame_Listener_Lists;
               Position : Cursor :=
                            Listeners.Find (R);
            begin
               if Has_Element (Position) then
                  Listeners.Delete (Position);
               else
                  raise Constraint_Error with
                    "attempted to remove a frame listener which was not found";
               end if;
            end;
         end loop;
         Removed.Clear;

         for A of Added loop
            Listeners.Append (A);
         end loop;
         Added.Clear;

      exception
         when E : others =>
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  Ada.Exceptions.Exception_Name (E)
                                  & Ada.Exceptions.Exception_Message (E));
      end Update_Frame_Listener_List;

   end Frame_Listeners;

   ------------------------
   -- Add_Frame_Listener --
   ------------------------

   procedure Add_Frame_Listener
     (Listener : not null access
        Rho.Frame_Event.Rho_Frame_Listener_Interface'Class)
   is
   begin
      Frame_Listeners.Add_Frame_Listener
        (Rho.Frame_Event.Rho_Frame_Listener (Listener));
   end Add_Frame_Listener;

   ----------------------
   -- Current_Renderer --
   ----------------------

   function Current_Renderer return Rho.Rendering.Rho_Renderer is
   begin
      return Local_Renderer;
   end Current_Renderer;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Rho.Assets.Add_Search_Path (Rho.Paths.Config_Path);
      Rho.Assets.Add_Image_Path (Rho.Paths.Config_Path);
      Rho.Assets.Add_Folder_Name ("texture", "textures");
      Rho.Assets.Add_Folder_Name ("mesh", "mesh");
      Rho.Assets.Add_Folder_Name ("material", "material");
      Rho.Assets.Add_Folder_Name ("shader", "shaders");

      Local_Renderer := Rho.Rendering.Create_Renderer ("gl-cairo");
   end Init;

   ---------------------
   -- Leave_Main_Loop --
   ---------------------

   procedure Leave_Main_Loop is
   begin
      Local_Renderer.EXIT_Render_Loop;
   end Leave_Main_Loop;

   ---------------
   -- Main_Loop --
   ---------------

   procedure Main_Loop is
   begin
      Local_Renderer.Render_Loop;
   end Main_Loop;

   ---------------------------
   -- Remove_Frame_Listener --
   ---------------------------

   procedure Remove_Frame_Listener
     (Listener : not null access
        Rho.Frame_Event.Rho_Frame_Listener_Interface'Class)
   is
   begin
      Frame_Listeners.Remove_Frame_Listener (Listener);
   end Remove_Frame_Listener;

   ----------------------
   -- Render_One_Frame --
   ----------------------

   procedure Render_One_Frame is
      use Ada.Calendar;
      Now     : constant Time := Clock;
      Elapsed : constant Duration :=
                  (if First_Frame then 0.0 else Now - Last_Render_Time);
      Event   : constant Rho.Frame_Event.Rho_Frame_Event :=
                  (Time_Since_Last_Event => Elapsed,
                   Render_Target         =>
                     Local_Renderer.Current_Render_Target);
   begin
      Frame_Listeners.Run_Frame_Started (Event);
      Frame_Listeners.Run_Frame_Rendering_Queued (Event);

      Local_Renderer.Render_One_Frame;

      Frame_Listeners.Run_Frame_Ended (Event);

      Frame_Listeners.Update_Frame_Listener_List;

      First_Frame := False;
      Last_Render_Time := Now;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               Ada.Exceptions.Exception_Name (E)
                               & ": " & Ada.Exceptions.Exception_Message (E));

   end Render_One_Frame;

end Rho.Main;
