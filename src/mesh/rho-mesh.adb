with Rho.Render_Operation;
with Rho.Context;

package body Rho.Mesh is

   -------------------
   -- Create_Entity --
   -------------------

   function Create_Entity
     (Mesh   : Rho_Mesh_Record'Class)
      return Rho.Entity.Rho_Entity
   is

      function Create_Sub_Entity
        (Sub_Mesh   : Rho_Sub_Mesh_Record)
         return Rho.Entity.Rho_Entity;

      -----------------------
      -- Create_Sub_Entity --
      -----------------------

      function Create_Sub_Entity
        (Sub_Mesh   : Rho_Sub_Mesh_Record)
         return Rho.Entity.Rho_Entity
      is
         use type Rho.Materials.Material.Rho_Material;
         Entity   : constant Rho.Entity.Rho_Entity :=
                      Rho.Entity.Create
                        (Mesh.Context,
                         Mesh.Name & " - sub-entity"
                         & Positive'Image (Sub_Mesh.Index));
         Material : constant Rho.Materials.Material.Rho_Material :=
                      (if Sub_Mesh.Material = null
                       then Mesh.Context.Material ("default")
                       else Sub_Mesh.Material);
      begin
         Entity.Set_Material (Material);

         Entity.Begin_Operation (Rho.Render_Operation.Triangle_List);
--           Ada.Text_IO.Put_Line ("   X       Y       Z      S      T   ");

         for Face_Index in 1 .. Sub_Mesh.Faces.Last_Index loop
            declare
               Face    : Vertex_Index_Array renames
                           Sub_Mesh.Faces (Face_Index);
            begin

               for Index in Face'Range loop
                  declare
                     Vertex_Index : constant Positive :=
                                      Face (Index);
                     Vertex : Vertex_Record renames
                                Sub_Mesh.Vertices.Element (Vertex_Index);
                  begin
--                       Ada.Float_Text_IO.Put
--                         (Float (Vertex.Vertex (1)), 3, 3, 0);
--                       Ada.Float_Text_IO.Put
--                         (Float (Vertex.Vertex (2)), 4, 3, 0);
--                       Ada.Float_Text_IO.Put
--                         (Float (Vertex.Vertex (3)), 4, 3, 0);

                     Entity.Normal (Vertex.Normal);
                     Entity.Color (Vertex.Color);

                     declare
                        Tex_Coord : constant Texture_Vertex :=
                                      Vertex.Texture;
                     begin
--                          Ada.Float_Text_IO.Put
--                            (Float (Tex_Coord.S), 2, 3, 0);
--                          Ada.Float_Text_IO.Put
--                            (Float (Tex_Coord.T), 2, 3, 0);
                        Entity.Texture_Coordinate (Tex_Coord.S, Tex_Coord.T);
                     end;

                     Entity.Vertex (Vertex.Vertex);

--                       Ada.Text_IO.New_Line;
                  end;
               end loop;
            end;
         end loop;
         Entity.End_Operation;
         return Entity;
      end Create_Sub_Entity;

   begin
      if Mesh.Sub_Meshes.Last_Index = 0 then
         return null;
      elsif Mesh.Sub_Meshes.Last_Index = 1 then
         return Create_Sub_Entity (Mesh.Sub_Meshes.First_Element);
      else
         declare
            Entity : Rho.Entity.Rho_Entity;
         begin
            Rho.Entity.Rho_New (Entity, Mesh.Context, Mesh.Name & " - entity");
            for Sub of Mesh.Sub_Meshes loop
               Entity.Add_Child (Create_Sub_Entity (Sub));
            end loop;
            return Entity;
         end;
      end if;
   end Create_Entity;

end Rho.Mesh;
