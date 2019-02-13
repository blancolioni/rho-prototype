with Rho.Color;
with Rho.Float_Arrays;
with Rho.Draw_Binding;
with Rho.Render_Target;

private with Rho.Vertex_Array;

package Rho.Node.Instanced is

   type Rho_Instanced_Node_Record is
     new Rho_Node_Record with private;

   type Rho_Instanced_Node is access all Rho_Instanced_Node_Record'Class;

   procedure Rho_New
     (Node        : in out Rho_Instanced_Node;
      Name        : String := "";
      Position    : Boolean := False;
      Orientation : Boolean := False;
      Scale       : Boolean := False;
      Color       : Boolean := False);

   procedure Initialize
     (Node        : in out Rho_Instanced_Node_Record'Class;
      Name        : String := "";
      Position    : Boolean := False;
      Orientation : Boolean := False;
      Scale       : Boolean := False;
      Color       : Boolean := False);

   function Create
     (Name        : String := "";
      Position    : Boolean := False;
      Orientation : Boolean := False;
      Scale       : Boolean := False;
      Color       : Boolean := False)
      return Rho_Instanced_Node;

   procedure Append
     (Node        : in out Rho_Instanced_Node_Record'Class;
      Position    : in     Rho.Matrices.Vector_3 := (0.0, 0.0, 0.0);
      Orientation : in Rho.Matrices.Matrix_3 :=
        Rho.Float_Arrays.Unit_Matrix (3);
      Scale       : in Rho.Matrices.Vector_3 := (1.0, 1.0, 1.0);
      Color       : in Rho.Color.Rho_Color := (0.0, 0.0, 0.0, 0.0));

   overriding procedure Load
     (Node : in out Rho_Instanced_Node_Record);

   overriding procedure Execute_Render
     (Node     : in out Rho_Instanced_Node_Record;
      Target   : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

private

   type Instanced_Node_Attribute is
     (Instanced_Position,
      Instanced_Orientation,
      Instanced_Scale,
      Instanced_Color,
      Custom_Instanced_1,
      Custom_Instanced_2);

   type Instanced_Attribute_Record is
      record
         Active    : Boolean := False;
         Attribute : Rho.Shader.Rho_Attribute_Value;
         Buffer    : Rho.Float_Buffer.Rho_Float_Buffer;
         Size      : Positive;
      end record;

   type Instanced_Attribute_Array is
     array (Instanced_Node_Attribute) of Instanced_Attribute_Record;

   type Rho_Instanced_Node_Record is
     new Rho_Node_Record with
      record
         Draw_Buffer       : Rho.Float_Buffer.Rho_Float_Buffer;
         Vertex_Array      : Rho.Vertex_Array.Rho_Vertex_Array;
         Attributes        : Instanced_Attribute_Array;
         Count             : Natural := 0;
         Instanced_Binding : Rho.Draw_Binding.Rho_Draw_Binding;
      end record;

end Rho.Node.Instanced;
