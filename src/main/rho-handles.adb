with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Rho.Paths;

package body Rho.Handles is

   ------------------------
   -- Add_Frame_Listener --
   ------------------------

   procedure Add_Frame_Listener
     (Handle   : not null access Root_Rho_Handle'Class;
      Listener : not null access
        Rho.Frame_Event.Rho_Frame_Listener_Interface'Class)
   is
   begin
      Handle.Listeners.Add_Frame_Listener (Listener);
   end Add_Frame_Listener;

   -------------------
   -- Delete_Handle --
   -------------------

   procedure Delete_Handle
     (Handle : in out Rho_Handle)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation (Root_Rho_Handle'Class, Rho_Handle);
   begin
      Free (Handle);
      Handle := null;
   end Delete_Handle;

   ---------------------
   -- Frame_Listeners --
   ---------------------

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

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      null;
   end Init;

   ---------------------
   -- Leave_Main_Loop --
   ---------------------

   procedure Leave_Main_Loop
     (Handle : not null access Root_Rho_Handle'Class)
   is
   begin
      Handle.Renderer.Exit_Render_Loop;
   end Leave_Main_Loop;

   ---------------
   -- Main_Loop --
   ---------------

   procedure Main_Loop
     (Handle : not null access Root_Rho_Handle'Class)
   is
   begin
      Handle.Event_Source.Run (Rho_Handle (Handle));
   end Main_Loop;

   --------------
   -- Material --
   --------------

   overriding function Material
     (Handle : not null access Root_Rho_Handle;
      Name   : String)
      return Rho.Materials.Material.Rho_Material
   is
   begin
      return Handle.Assets.Material (Name);
   end Material;

   ----------------
   -- New_Handle --
   ----------------

   function New_Handle
     (Event_Source  : Render_Event_Access;
      Shader_Folder : String)
      return Rho_Handle
   is
   begin
      return Handle : constant Rho_Handle :=
        new Root_Rho_Handle'
          (Rho.Context.Rho_Context_Record with
             Event_Source => Event_Source,
             others       => <>)
      do
         Rho.Assets.Add_Search_Path (Handle.Assets, Rho.Paths.Config_Path);
         Rho.Assets.Add_Image_Path (Handle.Assets, Rho.Paths.Config_Path);
         Rho.Assets.Add_Folder_Name (Handle.Assets, "texture", "textures");
         Rho.Assets.Add_Folder_Name (Handle.Assets, "mesh", "mesh");
         Rho.Assets.Add_Folder_Name (Handle.Assets, "material", "material");
         Rho.Assets.Add_Folder_Name
           (Handle.Assets, "shader", "shaders/" & Shader_Folder);
      end return;
   end New_Handle;

   ---------------------------
   -- Remove_Frame_Listener --
   ---------------------------

   procedure Remove_Frame_Listener
     (Handle   : not null access Root_Rho_Handle'Class;
      Listener : not null access
        Rho.Frame_Event.Rho_Frame_Listener_Interface'Class)
   is
   begin
      Handle.Listeners.Remove_Frame_Listener (Listener);
   end Remove_Frame_Listener;

   ----------------------
   -- Render_One_Frame --
   ----------------------

   procedure Render_One_Frame
     (Handle : not null access Root_Rho_Handle'Class)
   is
      use Ada.Calendar;
      Now     : constant Time := Clock;
      Elapsed : constant Duration :=
                  (if Handle.First_Frame
                   then 0.0
                   else Now - Handle.Last_Render_Time);
      Event   : constant Rho.Frame_Event.Rho_Frame_Event :=
                  (Time_Since_Last_Event => Elapsed,
                   Render_Target         =>
                     Handle.Renderer.Current_Render_Target);
   begin
      Handle.Listeners.Run_Frame_Started (Event);
      Handle.Listeners.Run_Frame_Rendering_Queued (Event);

      Handle.Renderer.Render_One_Frame;

      Handle.Listeners.Run_Frame_Ended (Event);

      Handle.Listeners.Update_Frame_Listener_List;

      Handle.First_Frame := False;
      Handle.Last_Render_Time := Now;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               Ada.Exceptions.Exception_Name (E)
                               & ": " & Ada.Exceptions.Exception_Message (E));

   end Render_One_Frame;

   -------------
   -- Texture --
   -------------

   overriding function Texture
     (Handle : not null access Root_Rho_Handle;
      Name   : String)
      return Rho.Texture.Rho_Texture
   is
   begin
      return Handle.Assets.Texture (Name);
   end Texture;

   ------------------
   -- Use_Renderer --
   ------------------

   procedure Use_Renderer
     (Handle   : not null access Root_Rho_Handle'Class;
      Renderer : Rho.Rendering.Rho_Renderer)
   is
   begin
      Handle.Renderer := Renderer;
   end Use_Renderer;

end Rho.Handles;
