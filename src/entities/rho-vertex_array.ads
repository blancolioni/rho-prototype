package Rho.Vertex_Array is

   type Rho_Vertex_Array_Record is tagged private;

   type Rho_Vertex_Array is access all Rho_Vertex_Array_Record'Class;

   procedure Rho_New (Vertex_Array : in out Rho_Vertex_Array);

   procedure Load (Vertex_Array : in out Rho_Vertex_Array_Record);
   procedure Enable (Vertex_Array : in out Rho_Vertex_Array_Record);
   procedure Disable (Vertex_Array : in out Rho_Vertex_Array_Record);

private

   type Vertex_Array_Id is new Natural;

   type Rho_Vertex_Array_Record is tagged
      record
         Loaded : Boolean := False;
         Id     : Vertex_Array_Id;
      end record;

end Rho.Vertex_Array;
