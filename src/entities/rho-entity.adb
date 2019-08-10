with Ada.Text_IO;

with GL;

with Rho.Materials.Technique;
with Rho.Materials.Pass;

with Rho.Names;

with Rho.Context;
with Rho.Rendering;

package body Rho.Entity is

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
     (Parent_Entity : in out Rho_Entity_Record;
      Child_Entity  : not null access Rho_Entity_Record'Class)
   is
   begin
      Parent_Entity.Children.Append (Rho_Entity (Child_Entity));
   end Add_Child;

   ---------------------
   -- Begin_Operation --
   ---------------------

   overriding procedure Begin_Operation
     (Item      : in out Rho_Entity_Record;
      Operation : Rho.Render_Operation.Operation_Type)
   is
   begin
      Item.Operation := Operation;
      Item.Current_Position :=
        (Has_Normal  => False,
         Has_Color   => False,
         Has_Texture => False,
         Operation   => Operation,
         Position    => (0.0, 0.0, 0.0),
         Normal      => (0.0, 0.0, 0.0),
         Color       => (0.0, 0.0, 0.0, 0.0),
         Texture     => null,
         S           => 0.0,
         T           => 0.0);
   end Begin_Operation;

   -------------------
   -- Bind_Material --
   -------------------

   procedure Bind_Material
     (Entity : in out Rho_Entity_Record'Class)
   is
      use type Rho.Shaders.Values.Rho_Attribute_Value;
      use Rho.Float_Buffer;
      use Rho.Render_Operation;

      First        : Boolean := True;
      Current_Draw : Draw_Operation;

      procedure Start_Operation
        (Operation : Rho.Render_Operation.Operation_Type);

      procedure Finish_Operation;

      ----------------------
      -- Finish_Operation --
      ----------------------

      procedure Finish_Operation is
      begin
         Entity.Draw_Ops.Append (Current_Draw);
      end Finish_Operation;

      ---------------------
      -- Start_Operation --
      ---------------------

      procedure Start_Operation
        (Operation : Rho.Render_Operation.Operation_Type)
      is
      begin
         Current_Draw.Operation := Operation;

         Create (Current_Draw.Vertices, Entity.Context);
         if Entity.Use_Normals then
            Create (Current_Draw.Normals, Entity.Context);
         end if;
         if Entity.Use_Textures then
            Create (Current_Draw.Textures, Entity.Context);
         end if;
         if Entity.Use_Colors then
            Create (Current_Draw.Colors, Entity.Context);
         end if;
         Current_Draw.Count := 0;
      end Start_Operation;

   begin
      if not Entity.Material.Loaded then
         Entity.Material.Load;
      end if;

      for Item of Entity.Positions loop
         if First or else Current_Draw.Operation /= Item.Operation then
            if not First then
               Finish_Operation;
            end if;

            Start_Operation (Item.Operation);
         end if;

         First := False;

         Current_Draw.Count := Current_Draw.Count + 1;

         Current_Draw.Vertices.Append (Item.Position);

         if Entity.Use_Normals then
            Current_Draw.Normals.Append (Item.Normal);
         end if;

         if Entity.Use_Colors then
            Current_Draw.Colors.Append (Item.Color);
         end if;

         if Entity.Use_Textures then
            Current_Draw.Textures.Append (Item.S);
            Current_Draw.Textures.Append (Item.T);
         end if;

      end loop;

      Finish_Operation;

      declare
         Technique : constant Rho.Materials.Technique.Rho_Technique :=
                       Entity.Material.Technique (1);
      begin
         for Pass_Index in 1 .. Technique.Pass_Count loop
            declare
               use Rho.Materials.Pass;
               Material_Pass : constant Rho_Material_Pass :=
                                 Technique.Pass (Pass_Index);
               Entity_Pass   : Render_Pass_Record;

--                 procedure Bind_Attribute
--                   (Attribute : Rho.Shaders.Rho_Attribute_Value;
--                    Start     : Positive;
--                    Size      : Positive);
--
--                 --------------------
--                 -- Bind_Attribute --
--                 --------------------
--
--                 procedure Bind_Attribute
--                   (Attribute : Rho.Shaders.Rho_Attribute_Value;
--                    Start     : Positive;
--                    Size      : Positive)
--                 is
--                 begin
--                    if Attribute /= null then
--                       Attribute.Bind_Vertex_Buffer
--                         (Entity.Draw_Buffer, Start, Size);
--                       Entity_Pass.Bindings.Append (Attribute);
--                    end if;
--                 end Bind_Attribute;

            begin

               for Draw_Op of Entity.Draw_Ops loop
                  declare
                     Binding : constant Rho.Draw_Binding.Rho_Draw_Binding :=
                                 Rho.Draw_Binding.Rho_New
                                   (Entity.Context, Draw_Op.Operation);
                  begin

                     Binding.Append
                       (Attribute => Material_Pass.Position_Attribute,
                        Size      => 3,
                        Buffer    => Draw_Op.Vertices);

                     if Entity.Use_Normals
                       and then Material_Pass.Has_Normal_Attribute
                     then
                        Binding.Append
                          (Attribute => Material_Pass.Normal_Attribute,
                           Size      => 3,
                           Buffer    => Draw_Op.Normals);
                     end if;

                     if Entity.Use_Textures
                       and then Material_Pass.Has_Texture_Coordinate_Attribute
                     then
                        Binding.Append
                          (Attribute =>
                             Material_Pass.Texture_Coordinate_Attribute,
                           Size      => 2,
                           Buffer    => Draw_Op.Textures);
                     end if;

                     if Entity.Use_Colors
                       and then Material_Pass.Has_Color_Attribute
                     then
                        Binding.Append
                          (Attribute => Material_Pass.Color_Attribute,
                           Size      => 4,
                           Buffer    => Draw_Op.Colors);
                     end if;

                     Binding.Load;

                     Entity_Pass.Bindings.Append (Binding);
                  end;
               end loop;

--                 if Pass_Index = Technique.Pass_Count then
--                    Entity_Pass.Vertex_Array_Object.Disable;
--                 end if;
--
--                 Rho.Vertex_Array.Rho_New (Entity_Pass.Vertex_Array_Object);
--                 Entity_Pass.Vertex_Array_Object.Load;
--                 Entity_Pass.Vertex_Array_Object.Enable;
--
--                 Bind_Attribute (Material_Pass.Position_Attribute, 1, 3);
--
--                 if Entity.Use_Normals then
--                    Bind_Attribute (Material_Pass.Normal_Attribute,
--                                    Entity.Normal_Start, 3);
--                 end if;
--
--                 if Entity.Use_Textures then
--                    Bind_Attribute
--                      (Material_Pass.Texture_Coordinate_Attribute,
--                       Entity.Texture_Start, 2);
--                 end if;
--
--                 if Entity.Use_Colors then
--                    Bind_Attribute
--                      (Material_Pass.Color_Attribute,
--                       Entity.Color_Start, 4);
--                 end if;
--
--                 if Pass_Index = Technique.Pass_Count then
--                    Entity_Pass.Vertex_Array_Object.Disable;
--                 end if;

               Entity.Render_Passes.Append (Entity_Pass);
            end;

         end loop;
      end;

   end Bind_Material;

   -----------------
   -- Child_Count --
   -----------------

   function Child_Count
     (Entity : Rho_Entity_Record)
      return Natural
   is
   begin
      return Entity.Children.Last_Index;
   end Child_Count;

   ------------------
   -- Child_Entity --
   ------------------

   function Child_Entity
     (Entity : Rho_Entity_Record;
      Index  : Positive)
      return Rho_Entity
   is
   begin
      return Entity.Children (Index);
   end Child_Entity;

   ------------------
   -- Child_Entity --
   ------------------

   function Child_Entity
     (Entity : Rho_Entity_Record;
      Name   : String)
      return Rho_Entity
   is
   begin
      for Child of Entity.Children loop
         if Child.Name = Name then
            return Child;
         end if;
      end loop;
      return null;
   end Child_Entity;

   -----------
   -- Color --
   -----------

   overriding procedure Color (Item   : in out Rho_Entity_Record;
                    Color  : Rho.Color.Rho_Color)
   is
   begin
      Item.Current_Position.Has_Color := True;
      Item.Current_Position.Color := Color;
      Item.Use_Colors := True;
   end Color;

   ------------
   -- Create --
   ------------

   function Create
     (Context  : not null access Rho.Context.Rho_Context_Record'Class;
      Name     : String := "")
      return Rho_Entity
   is
   begin
      return Entity : constant Rho_Entity := new Rho_Entity_Record do
         Entity.Context := Context;
         if Name = "" then
            Entity.Set_Name (Rho.Names.New_Name ("entity"));
         else
            Entity.Set_Name (Name);
         end if;
      end return;
   end Create;

   -----------
   -- Depth --
   -----------

   function Depth (Entity : Rho_Entity_Record) return Non_Negative_Float is
   begin
      return Entity.Max_Z - Entity.Min_Z;
   end Depth;

   ------------------
   -- Draw_Binding --
   ------------------

   function Draw_Binding
     (Entity : Rho_Entity_Record)
      return Rho.Draw_Binding.Rho_Draw_Binding
   is
   begin
      return Entity.Render_Passes.First_Element.Bindings.First_Element;
   end Draw_Binding;

   -------------------
   -- End_Operation --
   -------------------

   overriding procedure End_Operation (Item : in out Rho_Entity_Record)
   is null;

   --------------------
   -- Execute_Render --
   --------------------

   overriding procedure Execute_Render
     (Item : in out Rho_Entity_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
      use type Rho.Materials.Material.Rho_Material;
   begin
      if GL.Debug_Enabled then
         Ada.Text_IO.Put_Line
           ("Render entity: " & Item.Name);
      end if;

      if Item.Material /= null then
         if not Item.Material.Loaded then
            Item.Material.Load;
         end if;
         Item.Render_Material (Target);
      end if;

      for Child of Item.Children loop
         Child.Render (Target);
      end loop;

   end Execute_Render;

   ----------------
   -- Get_Shader --
   ----------------

   overriding function Get_Shader
     (Entity : in out Rho_Entity_Record)
      return Rho.Shaders.Rho_Shader
   is
   begin
      return Entity.Shader;
   end Get_Shader;

   ----------------
   -- Has_Shader --
   ----------------

   overriding function Has_Shader
     (Entity : Rho_Entity_Record)
      return Boolean
   is
      use type Rho.Shaders.Rho_Shader;
   begin
      return Entity.Shader /= null;
   end Has_Shader;

   ------------
   -- Height --
   ------------

   function Height (Entity : Rho_Entity_Record) return Non_Negative_Float is
   begin
      return Entity.Max_Y - Entity.Min_Y;
   end Height;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Entity : in out Rho_Entity_Record;
      Context  : not null access Rho.Context.Rho_Context_Record'Class;
      Name     : String := "")
   is
   begin
      Entity.Context := Context;
      Entity.Set_Name (Name);
   end Initialize;

   ----------------------
   -- Instanced_Render --
   ----------------------

--     procedure Instanced_Render
--       (Entity : in out Rho_Entity_Record;
--        Target : Rho.Render_Target.Rho_Render_Target;
--        Count  : Positive)
--     is
--     begin
--        Entity.Render (Target);
--        for Op of Entity.Draw_Ops loop
--           Target.Draw_Buffer
--             (Buffer         => Entity.Draw_Buffer,
--              Operation      => Op.Operation,
--              Count          => Op.Count,
--              Instance_Count => Count);
--        end loop;
--     end Instanced_Render;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Entity : in out Rho_Entity_Record)
   is
      use type Rho.Materials.Material.Rho_Material;
   begin

      if Entity.Material /= null then
         Entity.Bind_Material;
      end if;

      for Child of Entity.Children loop
         --  child entities can be shared
         if not Child.Loaded then
            Child.Load;
         end if;
      end loop;

      Entity.Loaded := True;
   end Load;

   ------------
   -- Loaded --
   ------------

   overriding function Loaded
     (Entity : Rho_Entity_Record)
      return Boolean
   is
   begin
      return Entity.Loaded;
   end Loaded;

   --------------
   -- Material --
   --------------

   function Material
     (Item : Rho_Entity_Record)
      return Rho.Materials.Material.Rho_Material
   is
   begin
      return Item.Material;
   end Material;

   ------------
   -- Normal --
   ------------

   overriding procedure Normal
     (Item    : in out Rho_Entity_Record;
      V       : Rho.Matrices.Vector_3)
   is
   begin
      Item.Current_Position.Has_Normal := True;
      Item.Current_Position.Normal := V;
      Item.Use_Normals := True;
   end Normal;

   ---------------------
   -- Render_Material --
   ---------------------

   procedure Render_Material
     (Entity : in out Rho_Entity_Record'Class;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
      Technique_Index : constant := 1;
   begin
      Entity.Material.Activate (Target);

      declare
         Technique : constant Rho.Materials.Technique.Rho_Technique :=
                       Entity.Material.Technique (Technique_Index);
      begin
         Technique.Activate (Target);

         for Pass_Index in 1 .. Technique.Pass_Count loop
            declare
               Pass : constant Rho.Materials.Pass.Rho_Material_Pass :=
                        Technique.Pass (Pass_Index);
               Bindings : Binding_Operation_Vectors.Vector renames
                            Entity.Render_Passes.Element (Pass_Index).Bindings;
            begin

               Pass.Activate (Target);

               for Binding of Bindings loop

                  Binding.Before_Render (Target);

                  if not Entity.Context.Renderer.Matrix_Saved
                    (Rho.Matrices.Projection)
                  then
                     Entity.Context.Renderer.Save_Matrix
                       (Rho.Matrices.Projection);
                  end if;

                  Entity.Context.Renderer.Save_Matrix
                    (Rho.Matrices.Model_View);

                  Binding.Execute_Render (Target);
                  Binding.After_Render (Target);

               end loop;

               Pass.Deactivate (Target);
            end;
         end loop;

         Technique.Deactivate (Target);
      end;

      Entity.Material.Deactivate (Target);

   end Render_Material;

   ------------
   -- Rho_New --
   ------------

   procedure Rho_New
     (Entity   : in out Rho_Entity;
      Context  : not null access Rho.Context.Rho_Context_Record'Class;
      Name     : in String         := "")
   is
   begin
      Entity := new Rho_Entity_Record;
      Entity.Context := Context;
      Entity.Set_Name (Name);
   end Rho_New;

   ------------------
   -- Set_Material --
   ------------------

   procedure Set_Material
     (Item     : in out Rho_Entity_Record;
      Material : in     Rho.Materials.Material.Rho_Material)
   is
--        use type Rho.Texture.Rho_Texture;
   begin
      Item.Material := Material;
--        Item.Shader := Item.Material.Shader;
--        Item.Texture := Item.Material.Texture;
--
--        if Item.Texture /= null then
--           Item.Bind_Shader
--             (Vertices => Item.Shader.Declare_Attribute_Value ("vPosition"),
--           Textures => Item.Shader.Declare_Attribute_Value ("texture_coord"),
--              Normals  => Item.Shader.Declare_Attribute_Value ("vNormal"),
--              Colors   => Item.Shader.Declare_Attribute_Value ("vColor"));
--           Item.Texture.Set_Uniform
--             (Item.Shader.Declare_Uniform_Value ("tex"));
--        end if;
   end Set_Material;

   ----------------
   -- Set_Shader --
   ----------------

   overriding procedure Set_Shader
     (Entity  : in out Rho_Entity_Record;
      Shader  : Rho.Shaders.Rho_Shader)
   is
   begin
      Entity.Shader := Shader;
   end Set_Shader;

   -----------------
   -- Set_Texture --
   -----------------

   procedure Set_Texture
     (Item    : in out Rho_Entity_Record;
      Texture : in     Rho.Texture.Rho_Texture)
   is
   begin
      Item.Material :=
        Rho.Materials.Material.Rho_New_With_Texture
          (Item.Context, Texture.Name, Texture, Lighting => False);
   end Set_Texture;

   -------------
   -- Texture --
   -------------

   function Texture
     (Item : Rho_Entity_Record)
      return Rho.Texture.Rho_Texture
   is
   begin
      return Item.Material.Technique (1).Pass (1).Texture;
   end Texture;

   ------------------------
   -- Texture_Coordinate --
   ------------------------

   overriding procedure Texture_Coordinate
     (Item    : in out Rho_Entity_Record;
      S, T    : Rho_Float)
   is
   begin
      Item.Current_Position.S := S;
      Item.Current_Position.T := T;
      Item.Current_Position.Has_Texture := True;
      Item.Use_Textures := True;
   end Texture_Coordinate;

   ------------
   -- Vertex --
   ------------

   overriding procedure Vertex
     (Item    : in out Rho_Entity_Record;
      V       : Rho.Matrices.Vector_3)
   is
   begin
      Item.Current_Position.Position := V;
      Item.Positions.Append (Item.Current_Position);
      Item.Min_X := Rho_Float'Min (V (1), Item.Min_X);
      Item.Min_Y := Rho_Float'Min (V (2), Item.Min_Y);
      Item.Min_Z := Rho_Float'Min (V (3), Item.Min_Z);
      Item.Max_X := Rho_Float'Max (V (1), Item.Max_X);
      Item.Max_Y := Rho_Float'Max (V (2), Item.Max_Y);
      Item.Max_Z := Rho_Float'Max (V (3), Item.Max_Z);
   end Vertex;

   -----------
   -- Width --
   -----------

   function Width (Entity : Rho_Entity_Record) return Non_Negative_Float is
   begin
      return Entity.Max_X - Entity.Min_X;
   end Width;

   -----------
   -- X_Max --
   -----------

   function X_Max (Entity : Rho_Entity_Record) return Rho_Float is
   begin
      return Entity.Max_X;
   end X_Max;

   -----------
   -- X_Min --
   -----------

   function X_Min (Entity : Rho_Entity_Record) return Rho_Float is
   begin
      return Entity.Min_X;
   end X_Min;

   -----------
   -- Y_Max --
   -----------

   function Y_Max (Entity : Rho_Entity_Record) return Rho_Float is
   begin
      return Entity.Max_Y;
   end Y_Max;

   -----------
   -- Y_Min --
   -----------

   function Y_Min (Entity : Rho_Entity_Record) return Rho_Float is
   begin
      return Entity.Min_Y;
   end Y_Min;

   -----------
   -- Z_Max --
   -----------

   function Z_Max (Entity : Rho_Entity_Record) return Rho_Float is
   begin
      return Entity.Max_Z;
   end Z_Max;

   -----------
   -- Z_Min --
   -----------

   function Z_Min (Entity : Rho_Entity_Record) return Rho_Float is
   begin
      return Entity.Min_Z;
   end Z_Min;

end Rho.Entity;
