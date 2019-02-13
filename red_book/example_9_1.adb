with Rho.Bitmap;
with Rho.Entity.Manual;
with Rho.Event;
with Rho.Scene;
with Rho.Scene_Node;
with Rho.Scene_Window;
with Rho.Texture;
with Rho.Window;

package body Example_9_1 is

   Check_Bitmap : Rho.Bitmap.Rho_Bitmap;

   procedure Create_Check_Image;

   ------------------------
   -- Create_Check_Image --
   ------------------------

   procedure Create_Check_Image is
      Image_Height : constant := 64;
      Image_Width  : constant := 64;

      Image        : Rho.Rho_Octet_Array (1 .. Image_Width * Image_Height * 4);

   begin
      for I in 1 .. Image_Height loop
         for J in 1 .. Image_Width loop
            declare
               Index : constant Positive :=
                         4 * (J - 1 + Image_Width * (I - 1)) + 1;
               Value : constant Rho.Rho_Octet :=
                         (if I / 8 mod 2 = 0 xor J / 8 mod 2 = 0
                          then 255
                          else 0);
            begin
               Image (Index) := Value;
               Image (Index + 1) := Value;
               Image (Index + 2) := Value;
               Image (Index + 3) := 255;
            end;
         end loop;
      end loop;

      Check_Bitmap := Rho.Bitmap.Create_Bitmap (Image_Width, Image_Height,
                                               Rho.Bitmap.RGBA, Image);
   end Create_Check_Image;

   -------------------
   -- Create_Window --
   -------------------

   procedure Create_Window is
      Scene : Rho.Scene.Rho_Scene :=
                Rho.Scene.Create_Scene;
   begin
      Create_Check_Image;

      GL.ClearColor (0.0, 0.0, 0.0, 0.0);
      GL.ShadeModel (GL.FLAT);
      GL.Enable (GL.DEPTH_TEST);

      declare
         Texture : constant Rho.Texture.Rho_Texture :=
                     Rho.Texture.Create_From_Bitmap
                       ("texture", Rho.Texture.Texture_2D,
                        Check_Bitmap);
         Entity  : constant Rho.Entity.Manual.Rho_Manual_Entity :=
                     new Rho.Entity.Manual.Rho_Manual_Entity_Record;
         Node : constant Rho.Scene_Node.Rho_Scene_Node :=
                  Scene.Root_Node.Create_Child;
      begin

         Node.Set_Position (0.0, 0.0, -3.6);

         Entity.Set_Texture (Texture);
         Entity.Start_Operation (Rho.Entity.Manual.Quads);
         Entity.Index (1);
         --  Entity.Color (Float'(0.0), 0.0, 0.0);

         Entity.Texture_Coordinate (Float'(0.0), 0.0);
         Entity.Vertex (Float'(-1.0), -1.0, 0.0);

         Entity.Index (2);
         Entity.Texture_Coordinate (Float'(0.0), 1.0);
         Entity.Vertex (Float'(-1.0), 1.0, 0.0);

         Entity.Index (3);
         Entity.Texture_Coordinate (Float'(1.0), 1.0);
         Entity.Vertex (Float'(1.0), 1.0, 0.0);

         Entity.Index (4);
         Entity.Texture_Coordinate (Float'(1.0), 0.0);
         Entity.Vertex (Float'(1.0), -1.0, 0.0);

         Entity.Finish_Operation;

         Node.Set_Name ("node");
         Node.Set_Entity (Entity);

      end;

      Scene.Active_Camera.Set_Position (0.0, 0.0, -1.0);
      Scene.Active_Camera.Set_Orientation (0.0, 0.0, 0.0);
      Scene.Active_Camera.Set_Near_Clip_Distance (1.0);
      Scene.Active_Camera.Set_Far_Clip_Distance (30.0);

      declare
         W : constant Rho.Window.Rho_Window :=
               Rho.Scene_Window.Create_Scene_Window (Scene);
      begin
         null;
      end;

   end Create_Window;

end Example_9_1;
