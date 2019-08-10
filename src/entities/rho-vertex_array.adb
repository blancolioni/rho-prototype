with Rho.Context;
with Rho.Rendering;

package body Rho.Vertex_Array is

   -------------
   -- Disable --
   -------------

   procedure Disable (Vertex_Array : in out Rho_Vertex_Array_Record) is
   begin
      Vertex_Array.Context.Renderer.Disable_Vertex_Array (Vertex_Array.Id);
   end Disable;

   ------------
   -- Enable --
   ------------

   procedure Enable (Vertex_Array : in out Rho_Vertex_Array_Record) is
   begin
      Vertex_Array.Context.Renderer.Enable_Vertex_Array (Vertex_Array.Id);
   end Enable;

   ----------
   -- Load --
   ----------

   procedure Load (Vertex_Array : in out Rho_Vertex_Array_Record) is
   begin
      if not Vertex_Array.Loaded then
         Vertex_Array.Id :=
           Vertex_Array.Context.Renderer.Create_Vertex_Array;
         Vertex_Array.Loaded := True;
      end if;
   end Load;

   ------------
   -- Rho_New --
   ------------

   procedure Rho_New
     (Vertex_Array : in out Rho_Vertex_Array;
      Context      : not null access Rho.Context.Rho_Context_Record'Class)
   is
   begin
      Vertex_Array := new Rho_Vertex_Array_Record'
        (Loaded  => False,
         Id      => 0,
         Context => Context);
   end Rho_New;

end Rho.Vertex_Array;
