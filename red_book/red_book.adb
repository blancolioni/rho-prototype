with Ada.Command_Line;
with Ada.Text_IO;

with Interfaces.C.Strings;

with Triangles;

with GLUT;
with GL;
with GL_Constants;

procedure Red_Book is
   use Interfaces.C;
begin

   GLUT.Init;
   GLUT.Init_Display_Mode (GLUT.RGBA);
--
--       (Mode => GLUT.DOUBLE or GLUT.RGBA or GLUT.DEPTH);
   GLUT.Init_Window_Size (512, 512);
--     GLUT.Init_Context_Version (4, 3);
--     GLUT.Init_Context_Profile (GLUT.CORE_PROFILE);

   declare
      Demo_Name : constant String :=
                    (if Ada.Command_Line.Argument_Count /= 1
                     then "triangles"
                     else Ada.Command_Line.Argument (1));
      Wid : constant Integer :=
                    GLUT.Create_Window (Demo_Name);
      pragma Unreferenced (Wid);

      function Load_Functions return Interfaces.C.int;
      pragma Import (C, Load_Functions, "ogl_LoadFunctions");

   begin

      if Load_Functions = 0 then
         Ada.Text_IO.Put_Line ("cannot load OpenGL");
      else
         Ada.Text_IO.Put_Line
           (Interfaces.C.Strings.Value
              (GL.Get_String (GL_Constants.GL_VENDOR))
            & " "
              & Interfaces.C.Strings.Value
              (GL.Get_String (GL_Constants.GL_VERSION)));
      end if;
      if Demo_Name = "triangles" then
         Triangles.Run;
      end if;
   end;

   GLUT.Main_Loop;

end Red_Book;
