limited with Rho.Context;

package Rho.Vertex_Array is

   type Rho_Vertex_Array_Record is tagged private;

   type Rho_Vertex_Array is access all Rho_Vertex_Array_Record'Class;

   procedure Rho_New
     (Vertex_Array : in out Rho_Vertex_Array;
      Context      : not null access Rho.Context.Rho_Context_Record'Class);

   procedure Load (Vertex_Array : in out Rho_Vertex_Array_Record);
   procedure Enable (Vertex_Array : in out Rho_Vertex_Array_Record);
   procedure Disable (Vertex_Array : in out Rho_Vertex_Array_Record);

private

   type Rho_Vertex_Array_Record is tagged
      record
         Loaded  : Boolean := False;
         Id      : Rho_Vertex_Array_Id;
         Context : access Rho.Context.Rho_Context_Record'Class;
      end record;

end Rho.Vertex_Array;
