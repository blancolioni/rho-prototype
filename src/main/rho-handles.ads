private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Calendar;

private with Rho.Assets;
private with Rho.Materials.Material;
private with Rho.Texture;

with Rho.Context;
with Rho.Frame_Event;

with Rho.Rendering;

package Rho.Handles is

   type Root_Rho_Handle is
     new Rho.Context.Rho_Context_Record with private;

   type Rho_Handle is access all Root_Rho_Handle'Class;

   overriding function Renderer
     (Handle : Root_Rho_Handle)
      return access Rho.Rendering.Rho_Renderer_Record'Class;

   procedure Init;

   procedure Main_Loop
     (Handle : not null access Root_Rho_Handle'Class);

   procedure Leave_Main_Loop
     (Handle : not null access Root_Rho_Handle'Class);

   procedure Use_Renderer
     (Handle   : not null access Root_Rho_Handle'Class;
      Renderer : Rho.Rendering.Rho_Renderer);

   procedure Render_One_Frame
     (Handle : not null access Root_Rho_Handle'Class);

   procedure Add_Frame_Listener
     (Handle   : not null access Root_Rho_Handle'Class;
      Listener : not null access
        Rho.Frame_Event.Rho_Frame_Listener_Interface'Class);

   procedure Remove_Frame_Listener
     (Handle   : not null access Root_Rho_Handle'Class;
      Listener : not null access
        Rho.Frame_Event.Rho_Frame_Listener_Interface'Class);

   type Render_Event_Source is synchronized interface;

   procedure Run
     (Source : Render_Event_Source;
      Handle : Rho_Handle)
   is abstract;

   procedure Stop
     (Source : Render_Event_Source)
   is abstract;

   type Render_Event_Access is access all Render_Event_Source'Class;

   function New_Handle
     (Event_Source : Render_Event_Access)
      return Rho_Handle;

   procedure Delete_Handle
     (Handle : in out Rho_Handle);

private

   package Frame_Listener_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Rho.Frame_Event.Rho_Frame_Listener, Rho.Frame_Event."=");

   protected type Frame_Listeners is
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

   type Root_Rho_Handle is
     new Rho.Context.Rho_Context_Record with
      record
         Assets           : Rho.Assets.Rho_Asset_Handle;
         Renderer         : Rho.Rendering.Rho_Renderer;
         First_Frame      : Boolean := True;
         Last_Render_Time : Ada.Calendar.Time;
         Listeners        : Frame_Listeners;
         Event_Source     : Render_Event_Access;
      end record;

   overriding function Image_Path
     (Handle          : Root_Rho_Handle;
      Image_File_Name : String)
      return String
   is (Handle.Assets.Image_Path (Image_File_Name));

   overriding function Material
     (Handle : not null access Root_Rho_Handle;
      Name   : String)
      return Rho.Materials.Material.Rho_Material;

   overriding function Texture
     (Handle : not null access Root_Rho_Handle;
      Name   : String)
      return Rho.Texture.Rho_Texture;

   overriding function Renderer
     (Handle : Root_Rho_Handle)
      return access Rho.Rendering.Rho_Renderer_Record'Class
   is (Handle.Renderer);

end Rho.Handles;
