private with Ada.Containers.Vectors;

with System.Storage_Elements;

limited with Rho.Context;

with Rho.Color;
with Rho.Matrices;

package Rho.Float_Buffer is

   type Array_Of_Floats is
     array (Positive range <>) of aliased Rho_Float;

   type Rho_Float_Buffer_Record is tagged private;

   type Rho_Float_Buffer_Id is new Natural;

   function Id
     (Buffer : Rho_Float_Buffer_Record'Class)
      return Rho_Float_Buffer_Id;

   function Count
     (Buffer : Rho_Float_Buffer_Record'Class)
      return Natural;

   function Value
     (Buffer : Rho_Float_Buffer_Record'Class;
      Index  : Positive)
      return Rho_Float;

   procedure Append
     (Buffer : in out Rho_Float_Buffer_Record'Class;
      Value  : in     Rho_Float);

   procedure Append
     (Buffer : in out Rho_Float_Buffer_Record'Class;
      Vertex : in     Rho.Matrices.Vector);

   procedure Append
     (Buffer : in out Rho_Float_Buffer_Record'Class;
      Color  : in     Rho.Color.Rho_Color);

   procedure Append
     (Buffer : in out Rho_Float_Buffer_Record'Class;
      X      : Rho_Float;
      Y      : Rho_Float;
      Z      : Rho_Float := 0.0);

   procedure Append
     (Buffer         : in out Rho_Float_Buffer_Record'Class;
      Item           : access Rho_Float_Buffer_Record'Class;
      Is_Vertex_Data : Boolean := False);

   procedure Start_Vertices
     (Buffer : in out Rho_Float_Buffer_Record'Class);

   procedure Load
     (Buffer : in out Rho_Float_Buffer_Record'Class);

   function To_Offset (Buffer : Rho_Float_Buffer_Record'Class;
                       Index  : Positive)
                       return System.Storage_Elements.Storage_Offset;

   type Rho_Float_Buffer is access all Rho_Float_Buffer_Record'Class;

   function Copy
     (Buffer : Rho_Float_Buffer_Record'Class)
      return Rho_Float_Buffer;

   function Join (Left, Right : Rho_Float_Buffer)
                  return Rho_Float_Buffer;

   procedure Create
     (Item : out Rho_Float_Buffer;
      Context : not null access Rho.Context.Rho_Context_Record'Class);

   function Create
     (Context : not null access Rho.Context.Rho_Context_Record'Class)
      return Rho_Float_Buffer;

   function Create
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Values  : Array_Of_Floats)
      return Rho_Float_Buffer;

private

   package Float_Vectors is
     new Ada.Containers.Vectors (Positive, Rho_Float);

   type Vertex_Info is
      record
         Start : Positive;
      end record;

   package Vertex_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Vertex_Info);

   type Rho_Float_Buffer_Record is tagged
      record
         Context       : access Rho.Context.Rho_Context_Record'Class;
         Values        : Float_Vectors.Vector;
         Vertex_Arrays : Vertex_Info_Vectors.Vector;
         Id            : Rho_Float_Buffer_Id;
      end record;

end Rho.Float_Buffer;
