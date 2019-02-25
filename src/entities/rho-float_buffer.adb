with GL_Types;

with Rho.Context;
with Rho.Rendering;

package body Rho.Float_Buffer is

   ------------
   -- Append --
   ------------

   procedure Append
     (Buffer : in out Rho_Float_Buffer_Record'Class;
      Value  : in     Rho_Float)
   is
   begin
      Buffer.Values.Append (Value);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Buffer : in out Rho_Float_Buffer_Record'Class;
      Vertex : in     Rho.Matrices.Vector)
   is
   begin
      for V of Vertex loop
         Buffer.Append (V);
      end loop;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Buffer : in out Rho_Float_Buffer_Record'Class;
      Color  : in     Rho.Color.Rho_Color)
   is
   begin
      Buffer.Append (Color.Red);
      Buffer.Append (Color.Green);
      Buffer.Append (Color.Blue);
      Buffer.Append (Color.Alpha);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Buffer : in out Rho_Float_Buffer_Record'Class;
      X      : Rho_Float;
      Y      : Rho_Float;
      Z      : Rho_Float := 0.0)
   is
   begin
      Buffer.Append (X);
      Buffer.Append (Y);
      Buffer.Append (Z);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Buffer         : in out Rho_Float_Buffer_Record'Class;
      Item           : access Rho_Float_Buffer_Record'Class;
      Is_Vertex_Data : Boolean := False)
   is
   begin
      if Is_Vertex_Data then
         Buffer.Vertex_Arrays.Append ((Start => Buffer.Values.Last_Index + 1));
      end if;
      Buffer.Values.Append (Item.Values);
   end Append;

   ----------
   -- Copy --
   ----------

   function Copy
     (Buffer : Rho_Float_Buffer_Record'Class)
      return Rho_Float_Buffer
   is
   begin
      return new Rho_Float_Buffer_Record'
        (Context       => Buffer.Context,
         Values        => Buffer.Values,
         Vertex_Arrays => Buffer.Vertex_Arrays,
         Id            => 0);
   end Copy;

   -----------
   -- Count --
   -----------

   function Count
     (Buffer : Rho_Float_Buffer_Record'Class)
      return Natural
   is
   begin
      return Buffer.Values.Last_Index;
   end Count;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item    : out Rho_Float_Buffer;
      Context : not null access Rho.Context.Rho_Context_Record'Class)
   is
   begin
      Item := new Rho_Float_Buffer_Record;
      Item.Context := Context;
      Item.Id := 0;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Context : not null access Rho.Context.Rho_Context_Record'Class)
      return Rho_Float_Buffer is
   begin
      return Result : constant Rho_Float_Buffer :=
        new Rho_Float_Buffer_Record
      do
         Result.Context := Context;
         Result.Id := 0;
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Values  : Array_Of_Floats)
      return Rho_Float_Buffer
   is
   begin
      return Result : constant Rho_Float_Buffer := Create (Context) do
         for V of Values loop
            Result.Append (V);
         end loop;
      end return;
   end Create;

   --------
   -- Id --
   --------

   function Id
     (Buffer : Rho_Float_Buffer_Record'Class)
      return Rho_Float_Buffer_Id
   is
   begin
      return Buffer.Id;
   end Id;

   ----------
   -- Join --
   ----------

   function Join
     (Left, Right : Rho_Float_Buffer)
      return Rho_Float_Buffer
   is
      Result : constant Rho_Float_Buffer := Create (Left.Context);
   begin
      Result.Append (Left);
      Result.Append (Right);
      return Result;
   end Join;

   ----------
   -- Load --
   ----------

   procedure Load
     (Buffer : in out Rho_Float_Buffer_Record'Class)
   is
   begin
      Buffer.Id := Buffer.Context.Renderer.Load_Buffer (Buffer);
   end Load;

   --------------------
   -- Start_Vertices --
   --------------------

   procedure Start_Vertices
     (Buffer : in out Rho_Float_Buffer_Record'Class)
   is
   begin
      Buffer.Vertex_Arrays.Append ((Start => Buffer.Values.Last_Index + 1));
   end Start_Vertices;

   ---------------
   -- To_Offset --
   ---------------

   function To_Offset
     (Buffer : Rho_Float_Buffer_Record'Class;
      Index  : Positive)
      return System.Storage_Elements.Storage_Offset
   is
      pragma Unreferenced (Buffer);
      use System.Storage_Elements;
      use GL_Types;
   begin
      return Storage_Offset ((Index - 1) * GLfloat'Size / 8);
   end To_Offset;

   -----------
   -- Value --
   -----------

   function Value
     (Buffer : Rho_Float_Buffer_Record'Class;
      Index  : Positive)
      return Rho_Float
   is
   begin
      return Buffer.Values.Element (Index);
   end Value;

end Rho.Float_Buffer;
