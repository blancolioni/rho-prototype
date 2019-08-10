package body Rho.Draw_Binding is

   ------------
   -- Append --
   ------------

   procedure Append
     (Draw_Binding : in out Rho_Draw_Binding_Record;
      Attribute    : not null access
        Rho.Shaders.Values.Rho_Attribute_Value_Record'Class;
      Size         : Positive;
      Buffer       : Rho.Float_Buffer.Rho_Float_Buffer)
   is
   begin
      Draw_Binding.Bindings.Append
        ((Rho.Shaders.Values.Rho_Attribute_Value (Attribute),
         Draw_Binding.Buffer.Count + 1, Size));
      Draw_Binding.Buffer.Append (Buffer);
      if Draw_Binding.Vertex_Count = 0 then
         Draw_Binding.Vertex_Count := Buffer.Count / Size;
      end if;
   end Append;

   -------------------
   -- Before_Render --
   -------------------

   overriding procedure Before_Render
     (Item   : in out Rho_Draw_Binding_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
      pragma Unreferenced (Target);
   begin
      Item.Vertex_Array.Enable;
   end Before_Render;

   ----------
   -- Copy --
   ----------

   function Copy
     (Draw_Binding : Rho_Draw_Binding_Record)
      return Rho_Draw_Binding
   is
      Result : constant Rho_Draw_Binding :=
                 new Rho_Draw_Binding_Record'(Draw_Binding);
   begin
      Result.Vertex_Array := null;
      Result.Buffer := Result.Buffer.Copy;
      return Result;
   end Copy;

   --------------------
   -- Execute_Render --
   --------------------

   overriding procedure Execute_Render
     (Item   : in out Rho_Draw_Binding_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
   begin
      if Item.Instance_Count = 0 then
         Target.Draw_Buffer
           (Buffer         => Item.Buffer,
            Operation      => Item.Operation,
            Count          => Item.Vertex_Count,
            Instance_Count => 1);
      else
         Target.Draw_Buffer
           (Buffer         => Item.Buffer,
            Operation      => Item.Operation,
            Count          => Item.Vertex_Count,
            Instance_Count => Item.Vertex_Count);
      end if;
   end Execute_Render;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Draw_Binding : in out Rho_Draw_Binding_Record;
      Context      : not null access Rho.Context.Rho_Context_Record'Class;
      Operation    : Rho.Render_Operation.Operation_Type) is
   begin
      Draw_Binding.Context := Context;
      Draw_Binding.Operation := Operation;
      Rho.Float_Buffer.Create (Draw_Binding.Buffer, Context);
   end Initialize;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Item : in out Rho_Draw_Binding_Record)
   is
   begin
      Item.Buffer.Load;
      Rho.Vertex_Array.Rho_New (Item.Vertex_Array, Item.Context);
      Item.Vertex_Array.Load;
      Item.Vertex_Array.Enable;
      for Binding of Item.Bindings loop
         Binding.Attribute.Bind_Vertex_Buffer
           (Item.Buffer, Binding.Start, Binding.Size);
      end loop;
      Item.Loaded := True;
   end Load;

   ------------
   -- Loaded --
   ------------

   overriding function Loaded
     (Item : Rho_Draw_Binding_Record)
      return Boolean
   is
   begin
      return Item.Loaded;
   end Loaded;

   ------------
   -- Rho_New --
   ------------

   function Rho_New
     (Context      : not null access Rho.Context.Rho_Context_Record'Class;
      Operation    : Rho.Render_Operation.Operation_Type)
      return Rho_Draw_Binding
   is
   begin
      return Draw_Binding : constant Rho_Draw_Binding :=
        new Rho_Draw_Binding_Record
      do
         Draw_Binding.Initialize (Context, Operation);
      end return;
   end Rho_New;

   -------------------
   -- Set_Instanced --
   -------------------

   procedure Set_Instanced
     (Draw_Binding : in out Rho_Draw_Binding_Record;
      Count        : Positive)
   is
   begin
      Draw_Binding.Instance_Count := Count;
   end Set_Instanced;

end Rho.Draw_Binding;
