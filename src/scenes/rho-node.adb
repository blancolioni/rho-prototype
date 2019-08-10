with Ada.Text_IO;

with GL;

with Rho.Names;

with Rho.Context;
with Rho.Rendering;

with Rho.Float_Arrays;
with Rho.Logging;

with Rho.Node.Lists;

package body Rho.Node is

   -----------------------
   -- Add_Click_Handler --
   -----------------------

   procedure Add_Click_Handler
     (Node    : not null access Rho_Node_Record'Class;
      Handler : Node_Click_Handler)
   is
   begin
      Node.On_Click := Handler;
      Node.Event_Manager.Register_Event_Source
        (Node, Rho.Event.Mouse_Button_Click);
   end Add_Click_Handler;

   ------------------
   -- After_Render --
   ------------------

   overriding procedure After_Render
     (Node   : in out Rho_Node_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
      pragma Unreferenced (Target);
   begin
      if Node.Translated
        or else Node.Rotated
        or else Node.Scaled
      then
         Node.Context.Renderer.Pop_Matrix;
      end if;
   end After_Render;

   ------------------
   -- Append_Child --
   ------------------

   procedure Append_Child
     (Parent : not null access Rho_Node_Record;
      Child  : not null access Rho_Node_Record'Class)
   is
   begin
      Parent.Children.Append (Child);
      Child.Parent := Rho_Node (Parent);
   end Append_Child;

   -------------------
   -- Before_Render --
   -------------------

   overriding procedure Before_Render
     (Node   : in out Rho_Node_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
      pragma Unreferenced (Target);
   begin

      Node.Clear_Child_Cache := Node.Clear_Child_Cache
        or else not Node.View_Matrix_Cached;

      if Node.Translated
        or else Node.Rotated
        or else Node.Scaled
      then
         Node.Context.Renderer.Push_Matrix;
         if not Node.View_Matrix_Cached then
            if Node.Translated then
               Node.Context.Renderer.Translate (Node.Position (1 .. 3));
            end if;
            if Node.Rotated and then not Node.Billboard then
               Node.Context.Renderer.Rotate (Node.Orientation);
            end if;
            Node.Model_View_Matrix := Node.Context.Renderer.Current;
            Node.View_Matrix_Cached := True;
            Node.Clear_Child_Cache := True;
         else
            Node.Context.Renderer.Set_Current (Node.Model_View_Matrix);
            Node.Clear_Child_Cache := False;
         end if;
      else
         Node.View_Matrix_Cached := True;
      end if;

   end Before_Render;

   -------------
   -- Changed --
   -------------

   function Changed
     (Node : Rho_Node_Record'Class)
      return Boolean
   is
   begin
      return Node.Changed;
   end Changed;

   --------------------
   -- Check_Identity --
   --------------------

   procedure Check_Identity (Node : in out Rho_Node_Record'Class) is
   begin
      Node.Node_Identity :=
        not Node.Scaled
        and then not Node.Translated
        and then not Node.Scaled;
   end Check_Identity;

   ----------------
   -- Child_Node --
   ----------------

   function Child_Node
     (Parent     : Rho_Node_Record;
      Child_Name : String)
      return Rho_Node
   is
   begin
      for N of Parent.Children loop
         if N.Name = Child_Name then
            return N;
         end if;
      end loop;
      return null;
   end Child_Node;

   ----------------
   -- Child_Node --
   ----------------

   function Child_Node
     (Parent      : Rho_Node_Record;
      Child_Index : Positive)
      return Rho_Node
   is
   begin
      return Parent.Children.Element (Child_Index);
   end Child_Node;

   -------------------
   -- Clear_Changed --
   -------------------

   procedure Clear_Changed
     (Node : in out Rho_Node_Record'Class)
   is
   begin
      Node.Changed := False;
   end Clear_Changed;

   ------------
   -- Create --
   ------------

   function Create
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Name    : String)
      return Rho_Node
   is
   begin
      return Node : Rho_Node do
         Rho_New (Node, Context, Name);
      end return;
   end Create;

   ------------------
   -- Create_Child --
   ------------------

   function Create_Child
     (Parent : not null access Rho_Node_Record;
      Name   : String)
      return Rho_Node
   is
      Child : constant Rho_Node :=
                new Rho_Node_Record;
   begin
      Child.Initialize (Parent.Context, Name);
      Child.Parent_Identity := False;
      Rho_Node_Record'Class (Parent.all).Append_Child (Child);
      return Child;
   end Create_Child;

   ------------------
   -- Create_Child --
   ------------------

   function Create_Child
     (Parent : not null access Rho_Node_Record'Class)
      return Rho_Node
   is
   begin
      return Parent.Create_Child (Rho.Names.New_Name ("node"));
   end Create_Child;

   ------------------
   -- Delete_Child --
   ------------------

   procedure Delete_Child
     (Parent : in out Rho_Node_Record;
      Name   : String)
   is
   begin
      Parent.Delete_Child (Parent.Find_Child (Name));
   end Delete_Child;

   ------------------
   -- Delete_Child --
   ------------------

   procedure Delete_Child
     (Parent : in out Rho_Node_Record;
      Child  : not null access Rho_Node_Record'Class)
   is
      Found_Index : Natural := 0;
   begin
      for I in 1 .. Parent.Children.Last_Index loop
         if Parent.Children (I) = Rho_Node (Child) then
            Found_Index := I;
            exit;
         end if;
      end loop;
      if Found_Index = 0 then
         raise Constraint_Error with
         Parent.Name
           & ": Delete_Child: child not found: " & Child.Name;
      end if;
      Parent.Children.Delete (Found_Index);
   end Delete_Child;

   ------------
   -- Entity --
   ------------

   function Entity
     (Node : Rho_Node_Record)
      return Rho.Entity.Rho_Entity
   is
   begin
      return Node.Entity;
   end Entity;

   -------------------
   -- Event_Manager --
   -------------------

   function Event_Manager
     (Node : Rho_Node_Record'Class)
      return access Rho.Event.Rho_Event_Manager'Class
   is
   begin
      if Node.Local_Event_Manager /= null then
         return Node.Local_Event_Manager;
      else
         declare
            It : Rho_Node := Node.Parent_Node;
         begin
            while It /= null and then It.Local_Event_Manager = null loop
               It := It.Parent_Node;
            end loop;
            return It.Local_Event_Manager;
         end;
      end if;
   end Event_Manager;

   --------------------
   -- Execute_Render --
   --------------------

   overriding procedure Execute_Render
     (Node : in out Rho_Node_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
      use type Rho.Entity.Rho_Entity;
      Current_Depth_Test  : constant Boolean := Target.Depth_Test;
      Renderer : constant Rho.Rendering.Rho_Renderer :=
                   Node.Context.Renderer;
   begin

      if GL.Debug_Enabled then
         Ada.Text_IO.Put_Line
           ("Render node: " & Node.Name);
      end if;

      Node.Screen_Rectangle := (0.0, 0.0, 0.0, 0.0);

      if not Node.Visible then
         return;
      end if;

      if Node.Entity /= null then

         if not Node.Entity.Loaded then
            Node.Entity.Load;
         end if;

         if Node.Scaled then
            Renderer.Push_Matrix;
            Renderer.Scale (Node.Scale);
         end if;

         if Node.Billboard then
            declare
               use Rho.Float_Arrays;
             --              MV  : constant Rho.Matrices.Matrix_4 :=
             --                      Target.Current (Rho.Matrices.Model_View);
             --              P   : constant Rho.Matrices.Matrix_4 :=
             --                      Target.Current (Rho.Matrices.Projection);
             --              MVP : constant Rho.Matrices.Matrix_4 := MV * P;
             --              pragma Unreferenced (MVP);
               use Rho.Matrices;
               Screen_Position : Vector_4 :=
                                   (Renderer.Current (Projection)
                                    * Renderer.Current (Model_View))
                                   * Node.Position;
            begin

               Screen_Position :=
                 Screen_Position / Screen_Position (4);

               if GL.Debug_Enabled then
                  Rho.Logging.Put ("world position: ");
                  Rho.Logging.Put (Node.Position);
                  Rho.Logging.New_Line;
                  Rho.Logging.Put ("screen position: ");
                  Rho.Logging.Put (Screen_Position);
                  Rho.Logging.New_Line;
               end if;

               if Screen_Position (3) > 0.0 then
                  return;
               end if;

               Renderer.Matrix_Mode (Projection);
               Renderer.Push_Matrix;
               Renderer.Load_Identity;
               --                    Target.Ortho
               --                      (Left   => -1.0,
               --                       Right  => 1.0,
               --                       Bottom => -1.0,
               --                       Top    => 1.0,
               --                       Near   => -1.0,
               --                       Far    => 1.0);
               Target.Set_Depth_Test (False);

               Renderer.Matrix_Mode (Model_View);
               Renderer.Push_Matrix;
               Renderer.Load_Identity;

               Renderer.Translate
                 (Screen_Position (1), Screen_Position (2), 0.0);

               if Node.Pixel_Scaled then
                  Renderer.Translate
                    (Node.Pixel_Offset_X
                     / Target.Viewport.Width,
                     Node.Pixel_Offset_Y
                     / Target.Viewport.Height,
                     0.0);

                  declare
                     X_Scale : constant Non_Negative_Float :=
                                 2.0 * Node.Pixel_Scale_Width
                                   / Target.Viewport.Width
                                 / Node.Entity.Width;
                     Y_Scale : constant Non_Negative_Float :=
                                 2.0 * Node.Pixel_Scale_Height
                                   / Target.Viewport.Height
                                 / Node.Entity.Height;
                     X       : constant Rho_Float :=
                                 (Screen_Position (1) + 1.0)
                                 * Target.Viewport.Width / 2.0
                                   + Node.Pixel_Offset_X
                                 + Node.Entity.X_Min * X_Scale;
                     Y       : constant Rho_Float :=
                                 (Screen_Position (2) + 1.0)
                                 * Target.Viewport.Height / 2.0
                                   + Node.Pixel_Offset_Y
                                 + Node.Entity.Y_Min * Y_Scale;
                  begin
                     Renderer.Scale (X_Scale, Y_Scale, 1.0);
                     Node.Screen_Rectangle :=
                       (X - Node.Pixel_Scale_Width,
                        Y - Node.Pixel_Scale_Height,
                        2.0 * Node.Pixel_Scale_Width,
                        2.0 * Node.Pixel_Scale_Height);
--                       Rho.Logging.Put (Node.Name & ": ");
--                       Rho.Logging.Put (X);
--                       Rho.Logging.Put (" ");
--                       Rho.Logging.Put (Y);
--                       Rho.Logging.Put (" ");
--                       Rho.Logging.Put (X_Scale);
--                       Rho.Logging.Put (" ");
--                       Rho.Logging.Put (Y_Scale);
--                       Rho.Logging.New_Line;
                  end;
               end if;
               if GL.Debug_Enabled then
                  for Y in -1 .. 1 loop
                     for X in -1 .. 1 loop
                        Rho.Logging.Put ("(");
                        Rho.Logging.Put (Rho_Float (X), 0);
                        Rho.Logging.Put (", ");
                        Rho.Logging.Put (Rho_Float (Y), 0);
                        Rho.Logging.Put (") => ");
                        Rho.Logging.Put (Renderer.Current (Projection)
                                        * Renderer.Current (Model_View)
                                        * (Rho_Float (X),
                                          Rho_Float (Y),
                                          0.0, 1.0));
                        Rho.Logging.New_Line;
                     end loop;
                  end loop;
               end if;
            end;
         end if;

         if Node.Instanced then
            null;
            --  Node.Entity.Instanced_Render (Target, Node.Instance_Count);
         else
            Node.Entity.Render (Target);
         end if;

         if Node.Scaled then
            Renderer.Pop_Matrix;
         end if;

      end if;

      Node.Clear_Changed;

      for I in 1 .. Node.Children.Last_Index loop
         declare
            N : constant Rho_Node :=
                  Node.Children.Element (I);
         begin
            if Node.Node_Identity and then Node.Parent_Identity then
               N.Parent_Identity := True;
            end if;
            if Node.Clear_Child_Cache then
               N.View_Matrix_Cached := False;
            end if;
            N.Render (Target);
         end;
      end loop;

      Node.Clear_Child_Cache := False;

      if Node.Billboard then
         Renderer.Matrix_Mode (Rho.Matrices.Projection);
         Renderer.Pop_Matrix;
         Renderer.Matrix_Mode (Rho.Matrices.Model_View);
         Renderer.Pop_Matrix;
         Target.Set_Depth_Test (Current_Depth_Test);
      end if;

   end Execute_Render;

   ---------------------------------
   -- Final_Transformation_Matrix --
   ---------------------------------

   function Final_Transformation_Matrix
     (Node : Rho_Node_Record)
      return Rho.Matrices.Matrix_4
   is
      use Rho.Matrices, Rho.Float_Arrays;
      M : Matrix_4 := Unit_Matrix (4);
      Parents : Rho.Node.Lists.List;
      It : Rho_Node := Node.Parent;
   begin
      while It /= null loop
         Parents.Append (It);
         It := It.Parent;
      end loop;
      for T of reverse Parents loop
         M := M * T.Transformation_Matrix;
      end loop;
      M := M * Node.Transformation_Matrix;
      return M;
   end Final_Transformation_Matrix;

   ----------------
   -- Find_Child --
   ----------------

   function Find_Child
     (Parent : not null access Rho_Node_Record'Class;
      Name   : String)
      return Rho_Node
   is
   begin
      if Parent.Name = Name then
         return Rho_Node (Parent);
      else
         for Node of Parent.Children loop
            declare
               Result : constant Rho_Node :=
                          Node.Find_Child (Name);
            begin
               if Result /= null then
                  return Result;
               end if;
            end;
         end loop;
         return null;
      end if;
   end Find_Child;

   ----------------------
   -- Fixed_Pixel_Size --
   ----------------------

   procedure Fixed_Pixel_Size
     (Node     : in out Rho_Node_Record;
      Width    : Non_Negative_Float;
      Height   : Non_Negative_Float;
      X_Offset : Rho_Float := 0.0;
      Y_Offset : Rho_Float := 0.0)
   is
   begin
      Node.Pixel_Scaled := True;
      Node.Pixel_Scale_Width := Width;
      Node.Pixel_Scale_Height := Height;
      Node.Pixel_Offset_X := X_Offset;
      Node.Pixel_Offset_Y := Y_Offset;
   end Fixed_Pixel_Size;

   ---------------------
   -- Has_Parent_Node --
   ---------------------

   function Has_Parent_Node
     (Node : Rho_Node_Record'Class)
      return Boolean
   is
   begin
      return Node.Parent /= null;
   end Has_Parent_Node;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Node    : in out Rho_Node_Record;
      Context : not null access Rho.Context.Rho_Context_Record'Class;
      Name    : String)
   is
   begin
      Node.Context := Context;
      Node.Set_Name (Name);
   end Initialize;

   -----------------------------------
   -- Inverse_Transformation_Matrix --
   -----------------------------------

   function Inverse_Transformation_Matrix
     (Node : Rho_Node_Record)
      return Rho.Matrices.Matrix_4
   is
      M : Rho.Matrices.Matrix_4 := Rho.Float_Arrays.Unit_Matrix (4);
   begin
      if Node.Rotated then
         declare
            R : constant Rho.Matrices.Matrix_3 :=
                  Rho_Node_Record'Class (Node).Orientation;
         begin
            for I in 1 .. 3 loop
               for J in 1 .. 3 loop
                  M (J, I) := R (I, J);
               end loop;
            end loop;
         end;
      end if;

      if Node.Translated then
         declare
            T : constant Rho.Matrices.Vector_3 :=
                  Rho_Node_Record'Class (Node).Position_3;
         begin
            for I in 1 .. 3 loop
               M (I, 4) := -T (I);
            end loop;
         end;
      end if;

      return M;
   end Inverse_Transformation_Matrix;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Node   : in out Rho_Node_Record)
   is
      use type Rho.Entity.Rho_Entity;
   begin
      if Node.Loaded then
         return;
      end if;

      Node.Loaded := True;

      if Node.Entity /= null
        and then not Node.Entity.Loaded
      then
         Node.Entity.Load;
      end if;

      if Node.Instanced then
         Node.Load_Instanced_Attributes;
      else
         for I in 1 .. Node.Children.Last_Index loop
            declare
               N : constant Rho_Node :=
                     Node.Children.Element (I);
            begin
               N.Load;
            end;
         end loop;
      end if;

   end Load;

   -------------------------------
   -- Load_Instanced_Attributes --
   -------------------------------

   procedure Load_Instanced_Attributes
     (Node : in out Rho_Node_Record'Class)
   is
   begin
      for I in 1 .. Node.Instanced_Vertices.Last_Index loop
         declare
            Attr   : Instanced_Vertex_Value renames
                       Node.Instanced_Vertices (I);
            Buffer : constant Rho.Float_Buffer.Rho_Float_Buffer :=
                       Rho.Float_Buffer.Create (Node.Context);
         begin
            Buffer.Start_Vertices;
            for I in 1 .. Node.Instance_Count loop
               declare
                  use Rho.Float_Arrays;
                  V : constant Real_Vector (1 .. Attr.Length) :=
                        (case Attr.Length is
                            when 1 => (1 => Attr.Scalar_Fn (I)),
                            when 2 => (0.0, 0.0),
                            when 3 => Attr.Vector_3_Fn (I),
                            when 4 => (0.0, 0.0, 0.0, 0.0));
               begin
                  for I in V'Range loop
                     Buffer.Append (V (I));
                  end loop;
               end;
            end loop;

            Buffer.Load;
            Attr.Buffer := Buffer;
         end;
      end loop;
   end Load_Instanced_Attributes;

   ------------
   -- Loaded --
   ------------

   overriding function Loaded
     (Node : Rho_Node_Record)
   return Boolean
   is
   begin
      return Node.Loaded;
   end Loaded;

   --------------
   -- On_Event --
   --------------

   overriding procedure On_Event
     (Node    : not null access Rho_Node_Record;
      Event   : Rho.Event.Rho_Event)
   is
      use all type Rho.Event.Rho_Signal;
   begin
      case Rho.Event.Get_Signal (Event) is
         when Mouse_Button_Click =>
            if Node.On_Click /= null then
               Node.On_Click (Rho.Node.Rho_Node (Node));
            end if;
         when others =>
            null;
      end case;
   end On_Event;

   -----------------
   -- Orientation --
   -----------------

   overriding function Orientation
     (Node    : in     Rho_Node_Record)
   return Rho.Matrices.Matrix_3
   is
   begin
      return Node.Orientation;
   end Orientation;

   -----------------
   -- Parent_Node --
   -----------------

   function Parent_Node
     (Node : Rho_Node_Record'Class)
      return Rho_Node
   is
   begin
      return Node.Parent;
   end Parent_Node;

   --------------
   -- Position --
   --------------

   overriding function Position
     (Node    : in     Rho_Node_Record)
   return Rho.Matrices.Vector_4
   is
   begin
      return Node.Position;
   end Position;

   --------------
   -- Rho_New --
   --------------

   procedure Rho_New
     (Node    : in out Rho_Node;
      Context : not null access Rho.Context.Rho_Context_Record'Class;
      Name    : String)
   is
   begin
      Node := new Rho_Node_Record;
      Node.Initialize (Context, Name);
   end Rho_New;

   -----------
   -- Scale --
   -----------

   procedure Scale
     (Node    : in out Rho_Node_Record'Class;
      X, Y, Z : Rho_Float)
   is
   begin
      Node.Scale := (X, Y, Z);
      Node.Scaled := X /= 1.0 or else Y /= 1.0 or else Z /= 1.0;
      Node.View_Matrix_Cached := False;
      Node.Check_Identity;
   end Scale;

   -----------
   -- Scale --
   -----------

   procedure Scale
     (Node    : in out Rho_Node_Record'Class;
      XYZ     : Rho_Float)
   is
   begin
      Scale (Node, XYZ, XYZ, XYZ);
   end Scale;

   ----------------------
   -- Screen_Rectangle --
   ----------------------

   function Screen_Rectangle
     (Node : Rho_Node_Record)
      return Rho.Rectangle.Rho_Rectangle
   is
   begin
      return Node.Screen_Rectangle;
   end Screen_Rectangle;

   -------------------
   -- Set_Billboard --
   -------------------

   procedure Set_Billboard
     (Node    : in out Rho_Node_Record;
      Enabled : Boolean)
   is
   begin
      Node.Billboard := Enabled;
      Node.View_Matrix_Cached := False;
   end Set_Billboard;

   -----------------
   -- Set_Changed --
   -----------------

   procedure Set_Changed
     (Node : in out Rho_Node_Record'Class)
   is
   begin
      Node.Changed := True;
      Node.View_Matrix_Cached := False;
      Node.Clear_Child_Cache := True;
   end Set_Changed;

   ----------------
   -- Set_Entity --
   ----------------

   procedure Set_Entity
     (Node     : in out Rho_Node_Record;
      Entity   : not null access Rho.Entity.Rho_Entity_Record'Class)
   is
   begin
      Node.Entity := Rho.Entity.Rho_Entity (Entity);
   end Set_Entity;

   -----------------------
   -- Set_Event_Manager --
   -----------------------

   procedure Set_Event_Manager
     (Node    : in out Rho_Node_Record'Class;
      Manager : access Rho.Event.Rho_Event_Manager'Class)
   is
   begin
      Node.Local_Event_Manager := Manager;
   end Set_Event_Manager;

   ------------------------
   -- Set_Instance_Value --
   ------------------------

   procedure Set_Instance_Value
     (Node            : in out Rho_Node_Record;
      Attribute_Value : in Rho.Shaders.Values.Rho_Attribute_Value;
      Access_Function : Float_Instance_Value_Function)
   is
   begin
      Node.Instanced_Vertices.Append
        ((1, Attribute_Value, null, Access_Function));
   end Set_Instance_Value;

   ------------------------
   -- Set_Instance_Value --
   ------------------------

   procedure Set_Instance_Value
     (Node            : in out Rho_Node_Record;
      Attribute_Value : in Rho.Shaders.Values.Rho_Attribute_Value;
      Access_Function : Vector_3_Instance_Value_Function)
   is
   begin
      Node.Instanced_Vertices.Append
        ((3, Attribute_Value, null, Access_Function));
   end Set_Instance_Value;

   -------------------
   -- Set_Instanced --
   -------------------

   procedure Set_Instanced
     (Node           : in out Rho_Node_Record;
      Instance_Count : Positive)
   is
   begin
      Node.Instanced := True;
      Node.Instance_Count := Instance_Count;
   end Set_Instanced;

   ---------------------
   -- Set_Orientation --
   ---------------------

   overriding
   procedure Set_Orientation
     (Node     : in out Rho_Node_Record;
      Rotation : in Rho.Matrices.Matrix_3)
   is
      use type Rho.Matrices.Matrix_3;
   begin
      Node.Orientation := Rotation;
      Node.Rotated := Rotation /= Rho.Float_Arrays.Unit_Matrix (3);
      Node.View_Matrix_Cached := False;
      Node.Check_Identity;
      Rho_Node_Record'Class (Node).Set_Changed;
   end Set_Orientation;

   ------------------
   -- Set_Position --
   ------------------

   overriding
   procedure Set_Position
     (Node     : in out Rho_Node_Record;
      Position : in Rho.Matrices.Vector)

   is
      use type Rho.Float_Arrays.Real_Vector;
   begin
      if Position'Length = 3 then
         Node.Position := Position & 1.0;
      elsif Position'Length = 4 then
         Node.Position := Position;
      elsif Position'Length = 2 then
         Node.Position := Position & (0.0, 1.0);
      elsif Position'Length = 1 then
         Node.Position := Position & (0.0, 0.0, 1.0);
      else
         raise Constraint_Error with "invalid position vector (length was"
           & Natural'Image (Position'Length);
      end if;
      Node.Translated := Node.Position /= (0.0, 0.0, 0.0, 1.0);
      Node.View_Matrix_Cached := False;
      Node.Check_Identity;
      Rho_Node_Record'Class (Node).Set_Changed;
   end Set_Position;

   -----------------
   -- Set_Visible --
   -----------------

   procedure Set_Visible
     (Node    : in out Rho_Node_Record;
      Visible : Boolean)
   is
   begin
      Node.Visible := Visible;
   end Set_Visible;

   ---------------------------
   -- Transformation_Matrix --
   ---------------------------

   function Transformation_Matrix
     (Node : Rho_Node_Record)
      return Rho.Matrices.Matrix_4
   is
      M : Rho.Matrices.Matrix_4 := Rho.Float_Arrays.Unit_Matrix (4);
   begin
      if Node.Rotated then
         declare
            R : constant Rho.Matrices.Matrix_3 :=
                  Rho_Node_Record'Class (Node).Orientation;
         begin
            for I in 1 .. 3 loop
               for J in 1 .. 3 loop
                  M (I, J) := R (I, J);
               end loop;
            end loop;
         end;
      end if;

      if Node.Translated then
         declare
            T : constant Rho.Matrices.Vector_3 :=
                  Rho_Node_Record'Class (Node).Position_3;
         begin
            for I in 1 .. 3 loop
               M (I, 4) := T (I);
            end loop;
         end;
      end if;

      return M;
   end Transformation_Matrix;

end Rho.Node;
