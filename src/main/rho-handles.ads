with Rho.Frame_Event;

with Rho.Rendering;

package Rho.Handles is

   type Root_Rho_Handle is tagged private;

   type Rho_Handle is access all Root_Rho_Handle'Class;

   procedure Init;

   function New_Handle
     return Rho_Handle;

   procedure Delete_Handle
     (Handle : in out Rho_Handle);

   procedure Main_Loop
     (Handle : not null access Root_Rho_Handle'Class);

   procedure Leave_Main_Loop
     (Handle : not null access Root_Rho_Handle'Class);

   procedure Use_Renderer
     (Handle   : not null access Root_Rho_Handle'Class;
      Renderer : Rho.Rendering.Rho_Renderer);

   function Current_Renderer
     (Handle : not null access Root_Rho_Handle'Class)
      return Rho.Rendering.Rho_Renderer;

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

private

   type Root_Rho_Handle is tagged
      record
         Renderer : Rho.Rendering.Rho_Renderer;
      end record;

end Rho.Handles;
