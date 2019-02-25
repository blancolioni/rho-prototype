with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Rho.Float_Arrays;

with Rho.Context;
with Rho.Texture.Loader;

with Partoe.DOM;

package body Rho.Mesh.Reader is

   type Dat_Command is (Nverts, Nfaces, Vertex, Faces,
                        Textures, Normals, Names);

   function Next_Line (File : Ada.Text_IO.File_Type) return String;

   function Parse_Line
     (Line    : String;
      Command : out Dat_Command;
      Value   : out Integer)
      return Boolean;

   procedure Get_Number
     (Line  : String;
      Index : in out Positive;
      Value : out Rho_Float);

   procedure Get_Number
     (Line  : String;
      Index : in out Positive;
      Value : out Integer);

   ----------------
   -- Get_Number --
   ----------------

   procedure Get_Number
     (Line  : String;
      Index : in out Positive;
      Value : out Rho_Float)
   is
      Start : Positive;
   begin
      while Index <= Line'Last
        and then Line (Index) in ' ' | ',' | Character'Val (9)
      loop
         Index := Index + 1;
      end loop;

      Start := Index;
      while Index <= Line'Last
        and then Line (Index) in '0' .. '9' | '-' | '+' | '.' | 'e' | 'E'
      loop
         Index := Index + 1;
      end loop;

      Value := Rho_Float'Value (Line (Start .. Index - 1));
   end Get_Number;

   ----------------
   -- Get_Number --
   ----------------

   procedure Get_Number
     (Line  : String;
      Index : in out Positive;
      Value : out Integer)
   is
      X : Rho_Float;
   begin
      Get_Number (Line, Index, X);
      Value := Integer (X);
   end Get_Number;

   ----------
   -- Load --
   ----------

   function Load
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Path    : String)
      return Rho_Mesh
   is
      Extension : constant String :=
                    Ada.Characters.Handling.To_Lower
                      (Ada.Directories.Extension (Path));
   begin
      if Extension = "dat" then
         return Read_Dat_File (Context, Path);
      elsif Extension = "xml" or else Extension = "mesh" then
         return Read_Mesh_XML_File (Context, Path);
      else
         return null;
      end if;
   end Load;

   ---------------
   -- Next_Line --
   ---------------

   function Next_Line (File : Ada.Text_IO.File_Type) return String is
   begin
      while not Ada.Text_IO.End_Of_File (File) loop
         declare
            Full_Line : constant String := Ada.Text_IO.Get_Line (File);
            Trim_Line : constant String :=
                          Ada.Strings.Fixed.Trim (Full_Line,
                                                  Ada.Strings.Both);
         begin
            if Trim_Line'Length > 1
              and then (Trim_Line (Trim_Line'First) /= '/'
                        or else Trim_Line (Trim_Line'First + 1) /= '/')
            then
               Ada.Text_IO.Put_Line (Trim_Line);
               return Trim_Line;
            end if;
         end;
      end loop;
      return "";
   end Next_Line;

   ----------------
   -- Parse_Line --
   ----------------

   function Parse_Line
     (Line    : String;
      Command : out Dat_Command;
      Value   : out Integer)
      return Boolean
   is
      Index : Positive := Line'First;
   begin
      while Index <= Line'Last
        and then Line (Index) /= ' '
        and then Line (Index) /= Character'Val (9)
      loop
         Index := Index + 1;
      end loop;
      begin
         Command := Dat_Command'Value (Line (Line'First .. Index - 1));
      exception
         when Constraint_Error =>
            return False;
      end;

      if Index < Line'Last then
         begin
            Value := Integer'Value (Line (Index + 1 .. Line'Last));
         exception
            when Constraint_Error =>
               return False;
         end;
      else
         Value := 0;
      end if;
      return True;
   end Parse_Line;

   -------------------
   -- Read_Dat_File --
   -------------------

   function Read_Dat_File
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Path    : String)
      return Rho_Mesh
   is
      use Ada.Text_IO;
      File : File_Type;
      Num_Faces    : Natural := 0;
      Num_Vertices : Natural := 0;
      Result       : constant Rho_Mesh := new Rho_Mesh_Record;
      Sub_Mesh     : Rho_Sub_Mesh_Record;
      Texture      : Rho.Texture.Rho_Texture;
   begin

      Result.Context := Context;
      Sub_Mesh.Index := 1;
      Result.Set_Name (Ada.Directories.Base_Name (Path));

      Open (File, In_File, Path);

      loop
         declare
            Line : constant String := Next_Line (File);
            Command : Dat_Command;
            Value   : Integer;
         begin
            exit when Line = "" or else Line = "END";

            if not Parse_Line (Line, Command, Value) then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "bad line in dat: " & Line);
               return null;
            end if;

            case Command is
               when Nverts =>
                  Num_Vertices := Value;
               when Nfaces =>
                  Num_Faces := Value;
               when Vertex =>
                  for I in 1 .. Num_Vertices loop
                     declare
                        Vertex_Line : constant String := Next_Line (File);
                        Index       : Positive := Vertex_Line'First;
                        V           : Vertex_Record;
                     begin
                        for I in V.Vertex'Range loop
                           Get_Number (Vertex_Line, Index, V.Vertex (I));
                        end loop;
                        V.Color := (0.5, 0.5, 0.5, 1.0);
                        V.Normal := (0.0, 0.0, 0.0);
                        V.Texture := (0.0, 0.0);
                        Sub_Mesh.Vertices.Append (V);
                     end;
                  end loop;
               when Faces =>
                  for I in 1 .. Num_Faces loop
                     declare
                        Vertex_Line : constant String := Next_Line (File);
                        Index       : Positive := Vertex_Line'First;
                        R, G, B     : Integer;
                        Normal : Rho.Matrices.Vector_3;
                        Count  : Natural;
                     begin
                        Get_Number (Vertex_Line, Index, R);
                        Get_Number (Vertex_Line, Index, G);
                        Get_Number (Vertex_Line, Index, B);
                        for I in Normal'Range loop
                           Get_Number (Vertex_Line, Index, Normal (I));
                        end loop;
                        Get_Number (Vertex_Line, Index, Count);
                        if Count /= 3 then
                           Sub_Mesh.Triangles := False;
                        end if;
                        declare
                           Face : Vertex_Index_Array (1 .. Count);
                        begin
                           for I in reverse 1 .. Count loop
                              declare
                                 X : Integer;
                              begin
                                 Get_Number (Vertex_Line, Index, X);
                                 Face (I) := X + 1;
                                 declare
                                    use Rho.Float_Arrays;
                                    N : Rho.Matrices.Vector_3 renames
                                          Sub_Mesh.Vertices (X + 1).Normal;
                                 begin
                                    N := N + Normal;
                                 end;
                              end;
                           end loop;

                           Sub_Mesh.Faces.Append (Face);

                        end;
                     end;
                  end loop;
               when Textures =>
                  for I in 1 .. Num_Faces loop
                     declare
                        Texture_Line : constant String := Next_Line (File);
                        First_Space  : Positive := 1;
                        Index        : Positive;
                     begin
                        while Texture_Line (First_Space) /= ' '
                          and then Texture_Line (First_Space)
                          /= Character'Val (9)
                        loop
                           First_Space := First_Space + 1;
                        end loop;

                        declare
                           use Rho.Texture;
                           File_Name : constant String :=
                                         Ada.Strings.Fixed.Trim
                                           (Texture_Line
                                              (1 .. First_Space - 1),
                                            Ada.Strings.Both);
                        begin
                           if Texture = null then
                              Texture :=
                                Rho.Texture.Loader.Load_Texture
                                  (Context, Context.Image_Path (File_Name));
                              Sub_Mesh.Material :=
                                Rho.Materials.Material.Rho_New_With_Texture
                                  (Context, Result.Name, Texture);
                           end if;

                           Index := First_Space + 1;
                           declare
                              Max_S, Max_T, S, T : Rho_Float;
                           begin
                              Get_Number (Texture_Line, Index, Max_S);
                              Get_Number (Texture_Line, Index, Max_T);
                              for Vertex_Index of
                                Sub_Mesh.Faces.Element (I)
                              loop
                                 Get_Number (Texture_Line, Index, S);
                                 Get_Number (Texture_Line, Index, T);

                                 Sub_Mesh.Vertices (Vertex_Index).Texture :=
                                   (S / Max_S, 1.0 - T / Max_T);
                              end loop;
                           end;

                        end;
                     end;
                  end loop;
               when Names =>
                  Skip_Line (File, Ada.Text_IO.Count (Value));

               when Normals =>
                  Skip_Line
                    (File, Ada.Text_IO.Count (Sub_Mesh.Vertices.Last_Index));

            end case;
         end;
      end loop;

      Close (File);

      Result.Sub_Meshes.Append (Sub_Mesh);
      return Result;

   end Read_Dat_File;

   ------------------------
   -- Read_Mesh_XML_File --
   ------------------------

   function Read_Mesh_XML_File
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Path    : String)
      return Rho_Mesh
   is
      use Partoe.DOM;
      Doc : constant Partoe_Document := Load (Path);
      Sub_Meshes : constant Array_Of_Partoe_Nodes :=
                     Doc.Select_Nodes ("//mesh/submeshes/submesh");
      Result : constant Rho_Mesh := new Rho_Mesh_Record;
   begin
      Result.Set_Name
        (Ada.Directories.Base_Name (Path));
      for Sub_Mesh_XML of Sub_Meshes loop
         declare
            Sub_Mesh : Rho_Sub_Mesh_Record;
            Geometry_XML : constant Partoe_Node :=
                             Sub_Mesh_XML.Select_Node ("geometry");
            Vertex_Buffer_XML : constant Partoe_Node :=
                                  Geometry_XML.Child ("vertexbuffer");
            Vertices_XML      : constant Array_Of_Partoe_Nodes :=
                                  Vertex_Buffer_XML.Children ("vertex");
            Faces_XML         : constant Array_Of_Partoe_Nodes :=
                                  Sub_Mesh_XML.Select_Nodes ("faces/face");
            Material_Attr     : constant Partoe_Attribute :=
                                  Sub_Mesh_XML.Attribute ("material");
         begin

            Sub_Mesh.Index := Result.Sub_Meshes.Last_Index + 1;

            if Material_Attr /= null then
               Sub_Mesh.Material := Context.Material (Material_Attr.Text);
            end if;

            for Vertex_XML of Vertices_XML loop
               declare
                  Position_XML : constant Partoe_Node :=
                                   Vertex_XML.Child ("position");
                  Normal_XML   : constant Partoe_Node :=
                                   Vertex_XML.Child ("normal");
                  Texture_XML  : constant Partoe_Node :=
                                   Vertex_XML.Child ("texcoord");
                  Vertex       : constant Rho.Matrices.Vector_3 :=
                                   (Rho_Float'Value
                                      (Position_XML.Attribute ("x").Text),
                                    Rho_Float'Value
                                      (Position_XML.Attribute ("y").Text),
                                    Rho_Float'Value
                                      (Position_XML.Attribute ("z").Text));
                  Color        : constant Rho.Color.Rho_Color :=
                                   (0.5, 0.5, 0.5, 1.0);
                  Tex_Coord    : constant Texture_Vertex :=
                                   (Rho_Float'Value
                                      (Texture_XML.Attribute ("u").Text),
                                    Rho_Float'Value
                                      (Texture_XML.Attribute ("v").Text));
                  Normal       : constant Rho.Matrices.Vector_3 :=
                                   (Rho_Float'Value
                                      (Normal_XML.Attribute ("x").Text),
                                    Rho_Float'Value
                                      (Normal_XML.Attribute ("y").Text),
                                    Rho_Float'Value
                                      (Normal_XML.Attribute ("z").Text));
               begin
                  Sub_Mesh.Vertices.Append
                    ((Vertex, Normal, Color, Tex_Coord));
               end;
            end loop;

            for Face_XML of Faces_XML loop
               declare
                  V1 : constant Natural :=
                         Natural'Value (Face_XML.Attribute ("v1").Text);
                  V2 : constant Natural :=
                         Natural'Value (Face_XML.Attribute ("v2").Text);
                  V3 : constant Natural :=
                         Natural'Value (Face_XML.Attribute ("v3").Text);
               begin
                  Sub_Mesh.Faces.Append ((V1 + 1, V2 + 1, V3 + 1));
               end;
            end loop;

            Result.Sub_Meshes.Append (Sub_Mesh);
         end;
      end loop;

      return Result;

   end Read_Mesh_XML_File;

end Rho.Mesh.Reader;
