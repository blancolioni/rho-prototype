with GL;
with GL_Types;

package body Rho.Vertex_Array is

   -------------
   -- Disable --
   -------------

   procedure Disable (Vertex_Array : in out Rho_Vertex_Array_Record) is
      pragma Unreferenced (Vertex_Array);
   begin
      GL.Bind_Vertex_Array (0);
   end Disable;

   ------------
   -- Enable --
   ------------

   procedure Enable (Vertex_Array : in out Rho_Vertex_Array_Record) is
   begin
      GL.Bind_Vertex_Array (GL_Types.Uint (Vertex_Array.Id));
   end Enable;

   ----------
   -- Load --
   ----------

   procedure Load (Vertex_Array : in out Rho_Vertex_Array_Record) is
   begin
      if not Vertex_Array.Loaded then
         declare
            Id : aliased GL_Types.Uint;
         begin
            GL.Gen_Vertex_Arrays (1, Id'Access);
            Vertex_Array.Id := Vertex_Array_Id (Id);
            Vertex_Array.Loaded := True;
         end;
      end if;
   end Load;

   ------------
   -- Rho_New --
   ------------

   procedure Rho_New (Vertex_Array : in out Rho_Vertex_Array) is
   begin
      Vertex_Array := new Rho_Vertex_Array_Record'(Loaded => False,
                                                  Id     => 0);
   end Rho_New;

end Rho.Vertex_Array;
