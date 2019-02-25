with GL_Constants;                     use GL_Constants;
with GL_Types;                         use GL_Types;

with GL;                               use GL;
with GLUT;

with Rho.Paths;
with Rho.Shaders;

package body Triangles is

   type Vertex_Array_Object is (Triangles);
   type Buffer_Object is (Array_Buffer);

   VAOs : array (Vertex_Array_Object) of aliased Uint;
   Buffers : array (Buffer_Object) of aliased Uint;

   Vertices : Array_Of_Float :=
                (-0.90, -0.90,
                 0.85, -0.90,
                 -0.90, 0.85,
                 0.90, -0.85,
                 0.90, 0.90,
                 -0.85, 0.90);

   procedure Display;

   -------------
   -- Display --
   -------------

   procedure Display is
   begin
      Clear (GL_COLOR_BUFFER_BIT);
      Bind_Vertex_Array (VAOs (Triangles));
      Draw_Arrays
        (Mode  => GL_TRIANGLES,
         First => 0,
         Count => 6);
   end Display;

   ---------
   -- Run --
   ---------

   procedure Run is
      Vertex_Shader : constant Rho.Shaders.Rho_Shader :=
                        Rho.Shaders.Load (Rho.Paths.Config_Path
                                        & "/shaders/triangles.vert",
                                        Rho.Shaders.Vertex);
      Fragment_Shader : constant Rho.Shaders.Rho_Shader :=
                        Rho.Shaders.Load (Rho.Paths.Config_Path
                                        & "/shaders/triangles.frag",
                                        Rho.Shaders.Fragment);
      Program         : constant Rho.Shaders.Rho_Program :=
                          Rho.Shaders.Create;
   begin

      Gen_Vertex_Arrays (1, VAOs (VAOs'First)'Access);
      Bind_Vertex_Array (VAOs (Triangles));

      Gen_Buffers (1, Buffers (Buffers'First)'Access);
      Bind_Buffer (GL_ARRAY_BUFFER, Buffers (Array_Buffer));
      Buffer_Data (GL_ARRAY_BUFFER, Vertices'Size / 8,
                   Vertices (Vertices'First)'Address, GL_STATIC_DRAW);

      Program.Add (Vertex_Shader);
      Program.Add (Fragment_Shader);
      Program.Compile;

      Program.Activate;

      Vertex_Attribute_Pointer
        (Index        => 0,
         Size         => 2,
         Element_Type => GL_FLOAT,
         Normalized   => GL_FALSE,
         Stride       => 0,
         Pointer      => 0);
      Enable_Vertex_Attribute_Array (0);

      GLUT.Display_Function (Display'Access);

   end Run;

end Triangles;
