with Cairo;

with Rho.Color;
with Rho.Float_Buffer;
with Rho.Rectangle;
with Rho.Render_Target;
with Rho.Render_Window;
with Rho.Texture;

package Rho.Rendering is

   Render_Error : exception;

   type Rho_Renderer_Record is abstract tagged limited private;

   procedure Render_Loop
     (Renderer : in out Rho_Renderer_Record)
   is abstract;

   procedure EXIT_Render_Loop
     (Renderer : in out Rho_Renderer_Record)
   is abstract;

   function Create_Top_Level_Window
     (Renderer : in out Rho_Renderer_Record)
      return Rho.Render_Window.Rho_Render_Window
   is abstract;

   function Current_Render_Target
     (Renderer : Rho_Renderer_Record)
      return Rho.Render_Target.Rho_Render_Target
      is abstract;

   procedure Render_One_Frame
     (Renderer : in out Rho_Renderer_Record)
   is abstract;

   function Create_Surface
     (Renderer : in out Rho_Renderer_Record;
      Width, Height : Rho_Float)
      return Cairo.Cairo_Surface
      is abstract;

   function Load_Buffer
     (Renderer : in out Rho_Renderer_Record;
      Buffer   : Rho.Float_Buffer.Rho_Float_Buffer_Record'Class)
      return Rho.Float_Buffer.Rho_Float_Buffer_Id
      is abstract;

   function Load_Texture
     (Renderer : in out Rho_Renderer_Record;
      S_Wrap       : Rho.Texture.Texture_Address_Mode;
      T_Wrap       : Rho.Texture.Texture_Address_Mode;
      Mag_Filter   : Rho.Texture.Texture_Filter_Type)
      return Rho.Texture.Texture_Id
      is abstract;

   procedure Load_Texture_Data
     (Renderer     : in out Rho_Renderer_Record;
      Texture_Id   : Rho.Texture.Texture_Id;
      From_Surface : Cairo.Cairo_Surface;
      Region       : Rho.Rectangle.Rho_Rectangle)
   is abstract;

   procedure Load_Texture_Data
     (Renderer     : in out Rho_Renderer_Record;
      Texture_Id   : Rho.Texture.Texture_Id;
      From_Data    : Rho.Color.Rho_Color_2D_Array)
   is abstract;

   type Rho_Renderer is access all Rho_Renderer_Record'Class;

   function Create_Renderer
     (Name : String)
      return Rho_Renderer;

private

   type Rho_Renderer_Record is abstract tagged limited null record;

end Rho.Rendering;
