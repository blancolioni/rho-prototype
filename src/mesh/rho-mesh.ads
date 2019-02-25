private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Vectors;

private with Rho.Color;
private with Rho.Matrices;

limited with Rho.Context;

with Rho.Object;
with Rho.Resource;

with Rho.Materials.Material;
with Rho.Texture;

with Rho.Entity;

package Rho.Mesh is

   Rho_Mesh_Class_Name : constant String := "mesh";

   type Rho_Mesh_Record is
     new Rho.Object.Rho_Resource_Record
     and Rho.Resource.Rho_Resource_Interface
   with private;

   type Rho_Mesh is access all Rho_Mesh_Record'Class;

   overriding function Class_Name
     (Mesh : Rho_Mesh_Record)
      return String
   is (Rho_Mesh_Class_Name);

   function Create_Entity
     (Mesh   : Rho_Mesh_Record'Class)
      return Rho.Entity.Rho_Entity;

private

   type Texture_Vertex is
      record
         S, T : Rho_Float;
      end record;

   type Vertex_Record is
      record
         Vertex  : Rho.Matrices.Vector_3;
         Normal  : Rho.Matrices.Vector_3;
         Color   : Rho.Color.Rho_Color;
         Texture : Texture_Vertex;
      end record;

   package Vertex_Vectors is
     new Ada.Containers.Vectors (Positive, Vertex_Record);

   type Vertex_Index_Array is array (Positive range <>) of Positive;

   package Vertex_Index_Vectors is
     new Ada.Containers.Vectors (Positive, Positive);

   package Face_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Vertex_Index_Array);

   type Rho_Sub_Mesh_Record is
      record
         Index     : Positive;
         Vertices  : Vertex_Vectors.Vector;
         Faces     : Face_Vectors.Vector;
         Texture   : Rho.Texture.Rho_Texture;
         Material  : Rho.Materials.Material.Rho_Material;
         Triangles : Boolean := True;
      end record;

   package Sub_Mesh_Vectors is
     new Ada.Containers.Vectors (Positive, Rho_Sub_Mesh_Record);

   type Rho_Mesh_Record is
     new Rho.Object.Rho_Resource_Record
     and Rho.Resource.Rho_Resource_Interface with
      record
         Context    : access Rho.Context.Rho_Context_Record'Class;
         Sub_Meshes : Sub_Mesh_Vectors.Vector;
      end record;

end Rho.Mesh;
