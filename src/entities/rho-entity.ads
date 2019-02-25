private with Ada.Containers.Vectors;
private with Ada.Strings.Bounded;

private with Rho.Float_Buffer;

with Rho.Draw_Binding;

with Rho.Color;
with Rho.Materials.Material;
with Rho.Matrices;
with Rho.Object;
with Rho.Renderable;
with Rho.Render_Operation;
with Rho.Render_Target;
with Rho.Shaders.Values;
with Rho.Texture;

limited with Rho.Context;

package Rho.Entity is

   Rho_Entity_Class_Name : constant String := "entity";

   type Rho_Entity_Record is
     new Rho.Object.Rho_Object_Record
     and Rho.Renderable.Rho_Renderable
     and Rho.Render_Operation.Rho_Render_Operation_Interface
     and Rho.Shaders.Rho_Has_Shader_Interface with private;

   type Rho_Entity is access all Rho_Entity_Record'Class;

   procedure Rho_New
     (Entity   : in out Rho_Entity;
      Context  : not null access Rho.Context.Rho_Context_Record'Class;
      Name     : in String         := "");

   procedure Initialize
     (Entity : in out Rho_Entity_Record;
      Context  : not null access Rho.Context.Rho_Context_Record'Class;
      Name     : String := "");

   function Create
     (Context  : not null access Rho.Context.Rho_Context_Record'Class;
      Name     : String := "")
      return Rho_Entity;

   overriding function Loaded
     (Entity : Rho_Entity_Record)
      return Boolean;

   overriding procedure Load
     (Entity : in out Rho_Entity_Record)
     with Pre => not Entity.Loaded;

--     procedure Instanced_Render
--       (Entity : in out Rho_Entity_Record;
--        Target : Rho.Render_Target.Rho_Render_Target;
--        Count  : Positive);

   function Texture (Item : Rho_Entity_Record)
                     return Rho.Texture.Rho_Texture;

   procedure Set_Texture (Item    : in out Rho_Entity_Record;
                          Texture : in     Rho.Texture.Rho_Texture);

   function Material
     (Item : Rho_Entity_Record)
      return Rho.Materials.Material.Rho_Material;

   procedure Set_Material
     (Item     : in out Rho_Entity_Record;
      Material : Rho.Materials.Material.Rho_Material);

   overriding procedure Execute_Render
     (Item : in out Rho_Entity_Record;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   overriding function Has_Shader
     (Entity : Rho_Entity_Record)
      return Boolean;

   overriding function Get_Shader
     (Entity : in out Rho_Entity_Record)
      return Rho.Shaders.Rho_Shader;

   overriding procedure Set_Shader
     (Entity  : in out Rho_Entity_Record;
      Shader : Rho.Shaders.Rho_Shader);

   overriding procedure Begin_Operation
     (Item      : in out Rho_Entity_Record;
      Operation : Rho.Render_Operation.Operation_Type);

   overriding procedure End_Operation (Item : in out Rho_Entity_Record);

   overriding procedure Normal
     (Item    : in out Rho_Entity_Record;
      V       : Rho.Matrices.Vector_3);

   overriding procedure Vertex
     (Item    : in out Rho_Entity_Record;
      V       : Rho.Matrices.Vector_3);

   overriding procedure Texture_Coordinate
     (Item    : in out Rho_Entity_Record;
      S, T    : Rho_Float);

   overriding procedure Color
     (Item   : in out Rho_Entity_Record;
      Color  : Rho.Color.Rho_Color);

   procedure Add_Child
     (Parent_Entity : in out Rho_Entity_Record;
      Child_Entity  : not null access Rho_Entity_Record'Class);

   function Child_Count
     (Entity : Rho_Entity_Record)
      return Natural;

   function Child_Entity
     (Entity : Rho_Entity_Record;
      Index  : Positive)
      return Rho_Entity;

   function Child_Entity
     (Entity : Rho_Entity_Record;
      Name   : String)
      return Rho_Entity;

   function Draw_Binding
     (Entity : Rho_Entity_Record)
      return Rho.Draw_Binding.Rho_Draw_Binding;

   function Width (Entity : Rho_Entity_Record) return Non_Negative_Float;
   function Height (Entity : Rho_Entity_Record) return Non_Negative_Float;
   function Depth (Entity : Rho_Entity_Record) return Non_Negative_Float;

   function X_Max (Entity : Rho_Entity_Record) return Rho_Float;
   function X_Min (Entity : Rho_Entity_Record) return Rho_Float;
   function Y_Max (Entity : Rho_Entity_Record) return Rho_Float;
   function Y_Min (Entity : Rho_Entity_Record) return Rho_Float;
   function Z_Max (Entity : Rho_Entity_Record) return Rho_Float;
   function Z_Min (Entity : Rho_Entity_Record) return Rho_Float;

private

   package Entity_Name is
     new Ada.Strings.Bounded.Generic_Bounded_Length
       (Max => 32);

   package Entity_Vectors is
     new Ada.Containers.Vectors (Positive, Rho_Entity);

   type Render_Position is
      record
         Has_Normal  : Boolean := False;
         Has_Color   : Boolean := False;
         Has_Texture : Boolean := False;
         Operation   : Rho.Render_Operation.Operation_Type;
         Position    : Rho.Matrices.Vector_3;
         Normal      : Rho.Matrices.Vector_3;
         Color       : Rho.Color.Rho_Color;
         Texture     : Rho.Texture.Rho_Texture;
         S, T        : Rho_Float;
      end record;

   package Position_Vectors is
     new Ada.Containers.Vectors (Positive, Render_Position);

   type Draw_Operation is
      record
         Operation : Rho.Render_Operation.Operation_Type;
         Count     : Natural := 0;
         Textures  : Rho.Float_Buffer.Rho_Float_Buffer;
         Normals   : Rho.Float_Buffer.Rho_Float_Buffer;
         Colors    : Rho.Float_Buffer.Rho_Float_Buffer;
         Vertices  : Rho.Float_Buffer.Rho_Float_Buffer;
      end record;

   package Draw_Operation_Vectors is
     new Ada.Containers.Vectors (Positive, Draw_Operation);

   package Attribute_Binding_Vectors is
     new Ada.Containers.Vectors
       (Positive, Rho.Shaders.Values.Rho_Attribute_Value,
        Rho.Shaders.Values."=");

   package Binding_Operation_Vectors is
     new Ada.Containers.Vectors (Positive, Rho.Draw_Binding.Rho_Draw_Binding,
                                 Rho.Draw_Binding."=");

   type Render_Pass_Record is
      record
         Bindings : Binding_Operation_Vectors.Vector;
      end record;

   package Render_Pass_Vectors is
     new Ada.Containers.Vectors (Positive, Render_Pass_Record);

   type Rho_Entity_Record is
     new Rho.Object.Rho_Object_Record
     and Rho.Renderable.Rho_Renderable
     and Rho.Render_Operation.Rho_Render_Operation_Interface
     and Rho.Shaders.Rho_Has_Shader_Interface with
      record
         Loaded              : Boolean := False;
         Context             : access Rho.Context.Rho_Context_Record'Class;
         Material            : Rho.Materials.Material.Rho_Material;
         Shader              : Rho.Shaders.Rho_Shader;
         Current_Position    : Render_Position;
         Operation           : Rho.Render_Operation.Operation_Type :=
                                 Rho.Render_Operation.Triangle_List;
         Use_Normals         : Boolean := False;
         Use_Textures        : Boolean := False;
         Use_Colors          : Boolean := False;
         Filled              : Boolean := True;
         Positions           : Position_Vectors.Vector;
         Draw_Ops            : Draw_Operation_Vectors.Vector;
         Normal_Start        : Natural := 0;
         Texture_Start       : Natural := 0;
         Color_Start         : Natural := 0;
         Render_Passes       : Render_Pass_Vectors.Vector;
         Bound_Attributes    : Boolean := False;
         Children            : Entity_Vectors.Vector;
         Min_X, Min_Y, Min_Z : Rho_Float := Rho_Float'Last;
         Max_X, Max_Y, Max_Z : Rho_Float := Rho_Float'First;
      end record;

   procedure Bind_Material
     (Entity : in out Rho_Entity_Record'Class);

   procedure Render_Material
     (Entity : in out Rho_Entity_Record'Class;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

end Rho.Entity;
