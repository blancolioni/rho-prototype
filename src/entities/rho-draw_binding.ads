private with Ada.Containers.Vectors;
private with Rho.Vertex_Array;

limited with Rho.Context;

with Rho.Float_Buffer;
with Rho.Renderable;
with Rho.Render_Operation;
with Rho.Render_Target;
with Rho.Shaders.Values;

package Rho.Draw_Binding is

   type Rho_Draw_Binding_Record is
     new Rho.Renderable.Rho_Renderable with private;

   type Rho_Draw_Binding is access all Rho_Draw_Binding_Record'Class;

   function Rho_New
     (Context      : not null access Rho.Context.Rho_Context_Record'Class;
      Operation    : Rho.Render_Operation.Operation_Type)
      return Rho_Draw_Binding;

   procedure Initialize
     (Draw_Binding : in out Rho_Draw_Binding_Record;
      Context      : not null access Rho.Context.Rho_Context_Record'Class;
      Operation    : Rho.Render_Operation.Operation_Type);

   overriding function Loaded
     (Item : Rho_Draw_Binding_Record)
      return Boolean;

   overriding procedure Load
     (Item : in out Rho_Draw_Binding_Record);

   overriding procedure Before_Render
     (Item   : in out Rho_Draw_Binding_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   overriding procedure Execute_Render
     (Item   : in out Rho_Draw_Binding_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   function Copy
     (Draw_Binding : Rho_Draw_Binding_Record)
      return Rho_Draw_Binding;

   procedure Append
     (Draw_Binding : in out Rho_Draw_Binding_Record;
      Attribute    : not null access
        Rho.Shaders.Values.Rho_Attribute_Value_Record'Class;
      Size         : Positive;
      Buffer       : Rho.Float_Buffer.Rho_Float_Buffer);

   procedure Set_Instanced
     (Draw_Binding : in out Rho_Draw_Binding_Record;
      Count        : Positive);

private

   type Attribute_Binding_Record is
      record
         Attribute : Rho.Shaders.Values.Rho_Attribute_Value;
         Start     : Positive;
         Size      : Positive;
      end record;

   package Attribute_Binding_Vectors is
     new Ada.Containers.Vectors
       (Positive, Attribute_Binding_Record);

   type Rho_Draw_Binding_Record is
     new Rho.Renderable.Rho_Renderable with
      record
         Loaded         : Boolean := False;
         Bound          : Boolean := False;
         Context        : access Rho.Context.Rho_Context_Record'Class;
         Operation      : Rho.Render_Operation.Operation_Type;
         Vertex_Array   : Rho.Vertex_Array.Rho_Vertex_Array;
         Buffer         : Rho.Float_Buffer.Rho_Float_Buffer;
         Bindings       : Attribute_Binding_Vectors.Vector;
         Vertex_Count   : Natural := 0;
         Instance_Count : Natural := 0;
      end record;

end Rho.Draw_Binding;
