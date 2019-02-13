package body Rho.Node.Instanced is

   ------------
   -- Append --
   ------------

   procedure Append
     (Node        : in out Rho_Instanced_Node_Record'Class;
      Position    : in     Rho.Matrices.Vector_3 := (0.0, 0.0, 0.0);
      Orientation : in Rho.Matrices.Matrix_3 :=
        Rho.Float_Arrays.Unit_Matrix (3);
      Scale       : in Rho.Matrices.Vector_3 := (1.0, 1.0, 1.0);
      Color       : in Rho.Color.Rho_Color := (0.0, 0.0, 0.0, 0.0))
   is
   begin
      if Node.Attributes (Instanced_Position).Active then
         for X of Position loop
            Node.Attributes (Instanced_Position).Buffer.Append (X);
         end loop;
      end if;
      if Node.Attributes (Instanced_Orientation).Active then
         for I in Orientation'Range (2) loop
            for J in Orientation'Range (1) loop
               Node.Attributes (Instanced_Orientation).Buffer.Append
                 (Orientation (I, J));
            end loop;
         end loop;
      end if;
      if Node.Attributes (Instanced_Scale).Active then
         for X of Scale loop
            Node.Attributes (Instanced_Scale).Buffer.Append (X);
         end loop;
      end if;
      if Node.Attributes (Instanced_Color).Active then
         declare
            Buf : Rho.Float_Buffer.Rho_Float_Buffer renames
                    Node.Attributes (Instanced_Color).Buffer;
         begin
            Buf.Append (Color.Red);
            Buf.Append (Color.Green);
            Buf.Append (Color.Blue);
            Buf.Append (Color.Alpha);
         end;
      end if;

      Node.Count := Node.Count + 1;

   end Append;

   ------------
   -- Create --
   ------------

   function Create
     (Name        : String := "";
      Position    : Boolean := False;
      Orientation : Boolean := False;
      Scale       : Boolean := False;
      Color       : Boolean := False)
      return Rho_Instanced_Node
   is
      Node : Rho_Instanced_Node;
   begin
      Rho_New (Node, Name, Position, Orientation, Scale, Color);
      return Node;
   end Create;

   --------------------
   -- Execute_Render --
   --------------------

   overriding procedure Execute_Render
     (Node     : in out Rho_Instanced_Node_Record;
      Target   : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
   begin
      Node.Instanced_Binding.Render (Target);
   end Execute_Render;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Node : in out Rho_Instanced_Node_Record'Class;
      Name : String := "";
      Position    : Boolean := False;
      Orientation : Boolean := False;
      Scale       : Boolean := False;
      Color       : Boolean := False)
   is
      procedure Create_Instance
        (Attribute : Instanced_Node_Attribute;
         Size      : Positive);

      ---------------------
      -- Create_Instance --
      ---------------------

      procedure Create_Instance
        (Attribute : Instanced_Node_Attribute;
         Size      : Positive)
      is
      begin
         Node.Attributes (Attribute) :=
           (Active    => True,
            Attribute => null,
            Buffer    => null,
            Size      => Size);
         Rho.Float_Buffer.Create
           (Node.Attributes (Attribute).Buffer);
      end Create_Instance;

   begin
      if Position then
         Create_Instance (Instanced_Position, 3);
      end if;
      if Orientation then
         Create_Instance (Instanced_Orientation, 9);
      end if;
      if Scale then
         Create_Instance (Instanced_Scale, 3);
      end if;
      if Color then
         Create_Instance (Instanced_Color, 4);
      end if;

      Node.Set_Name (Name);
   end Initialize;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Node : in out Rho_Instanced_Node_Record)
   is

   begin
      Rho_Node_Record (Node).Load;
      Node.Instanced_Binding := Node.Entity.Draw_Binding.Copy;
      for Attribute in Node.Attributes'Range loop
         if Node.Attributes (Attribute).Active then
            Node.Instanced_Binding.Append
              (Node.Attributes (Attribute).Attribute,
               Node.Attributes (Attribute).Size,
               Node.Attributes (Attribute).Buffer);
         end if;
      end loop;
   end Load;

   ------------
   -- Rho_New --
   ------------

   procedure Rho_New
     (Node        : in out Rho_Instanced_Node;
      Name        : String := "";
      Position    : Boolean := False;
      Orientation : Boolean := False;
      Scale       : Boolean := False;
      Color       : Boolean := False)
   is
   begin
      Node := new Rho_Instanced_Node_Record;
      Node.Initialize (Name, Position, Orientation, Scale, Color);
   end Rho_New;

end Rho.Node.Instanced;
